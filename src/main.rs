pub mod parser_pool;
pub mod error;
pub mod helpers;

use dashmap::mapref::entry::Entry;
use dashmap::DashMap;
use futures::future::{join_all, BoxFuture};
use futures::FutureExt;
use helpers::W;
use request::{GotoDeclarationParams, GotoDeclarationResponse};
use tokio::io::AsyncReadExt;
use tokio::sync::OnceCell;
use tree_sitter::{Node, Parser, Point, Tree};

use tokio::fs::File;

use std::collections::{HashMap, HashSet};
use std::env::VarError::{NotPresent, NotUnicode};
use parser_pool::ParserPool;
use tower_lsp::{jsonrpc, lsp_types::*};
use tower_lsp::{Client, LanguageServer, Server, LspService};
use error::{Result, Error};

use tracing::{info, trace, warn};

use crate::parser_pool::new_pool;

#[derive(Debug, Clone, Eq, PartialEq)]
enum Value {
    Bool(bool),
    String(String),
    Number(i32),
    //Array,
    Function,
    Identifier(String),
}

impl Value {
    fn kind(&self) -> &'static str {
        match self {
            Self::Bool(_) => "Bool",
            Self::String(_) => "String",
            Self::Number(_) => "Number",
            Self::Function => "Function",
            Self::Identifier(_) => "Identifier",
        }
    }
}

#[derive(Debug)]
enum Usage {
    Local(String),
    Parent(String),
}

#[derive(Debug)]
enum Syntax {
    Usage(Usage),
    Value(Value),
    Assign(Value),
    Subtract(Value),
    Concatenate(Value),
}

impl From<&Value> for bool {
    fn from(value: &Value) -> Self {
        match value {
            Value::Bool(v) => *v,
            Value::String(s) => !s.is_empty(),
            Value::Identifier(s) => !s.is_empty(),
            Value::Number(n) => *n != 0,
            //Value::Array => true,
            Value::Function => true,
        }
    }
}

#[derive(Debug, Clone)]
struct Symbol {
    value: Option<Value>,
    references: Vec<Location>,
    definition: Option<Location>,
    documentation: MarkupContent,
}

impl Default for Symbol {
    fn default() -> Self {
        Self {
            value: Default::default(),
            references: Default::default(),
            definition: Default::default(),
            documentation: markdown(""),
        }
    }
}

impl Symbol {

    fn define(&mut self, url: &Url, node: W<Node>, documentation: String, value: Value) -> std::result::Result<Value, W<Diagnostic>> {
        match &self.definition {
            Some(location) => Err(node.error(
                "macro redefinition"
            ).with(Some(vec![DiagnosticRelatedInformation {
                location: location.clone(),
                message: "defined here".into(),
            }]))),
            _ => {
                self.value = Some(value.clone());
                self.documentation = markdown(documentation);
                self.definition = Some(node.url(url));
                Ok(value)
            },
        }
    }

    fn undefine(&mut self, url: &Url, node: W<Node>) -> std::result::Result<(), W<Diagnostic>> {
        match self.definition {
            Some(_) => {
                self.value = None;
                self.references.push(node.url(url));
                Ok(())
            },
            _ => Err(None),
        }.map_err(|location| node.error("trying to undefine undefined macro")
            .with(location.map(|location| vec![DiagnosticRelatedInformation {
                location,
                message: "undefined here".into(),
            }]))
        )
    }

    fn reference(&mut self, url: &Url, node: W<Node>) -> std::result::Result<Value, W<Diagnostic>> {
        match &self.definition {
            //Some((location, Reference::Undef)) => Err(Some(location.clone())),
            Some(_) => {
                self.references.push(node.url(url));
                Ok(self.value.clone().unwrap_or(Value::Bool(false)))
            },
            _ => Err(None),
        }.map_err(|location| node.error("accessing undefined variable")
            .with(location.map(|location| vec![DiagnosticRelatedInformation {
                location,
                message: "undefined here".into(),
            }]))
        )
    }

    fn define_or_reference(&mut self, url: &Url, node: W<Node>, documentation: String, value: Value) -> Value {
        match self.definition {
            Some(_) => {
                self.references.push(node.url(url));
            },
            _ => {
                self.definition = Some(node.url(url));
            },
        };
        self.documentation = markdown(documentation);
        self.value = Some(value);
        self.value.clone().expect("just set it above")
    }

}

static NULL: OnceCell<Tree> = OnceCell::const_new();
type Definitions = HashMap<String, Symbol>;

#[derive(Debug)]
struct Scope {
    version: i32,
    diagnostics: Vec<Diagnostic>,
    definitions: Definitions,
    semantic_tokens: Vec<SemanticToken>,
    once: bool,
    content: String,
    references: HashSet<Url>,
    tree: Tree,
    faulty_ranges: Vec<tree_sitter::Range>,
}

type IfStack<'tree> = Vec<(bool, W<Node<'tree>>, Option<W<Node<'tree>>>)>;

#[derive(Debug, Default)]
struct Context<'tree> {
    documentation: String,
    named: HashMap<String, (W<Node<'tree>>, Syntax)>,
    unnamed: Vec<(W<Node<'tree>>, Syntax)>,
}

impl<'tree> Context<'tree> {

    fn new(documentation: String) -> Self {
        Self { documentation, ..Default::default() }
    }

    fn get(&mut self, name: &str, node: W<Node<'tree>>) -> std::result::Result<(W<Node<'tree>>, Syntax), W<Diagnostic>> {
        self.named.remove(name).ok_or_else(|| node.error(format!("expected {}", name)))
    }

}

impl Default for Scope {
    fn default() -> Self {
        Self {
            version: Default::default(),
            diagnostics: Default::default(),
            definitions: Default::default(),
            semantic_tokens: Default::default(),
            once: Default::default(),
            content: Default::default(),
            references: Default::default(),
            tree: NULL.get().unwrap().clone(),
            faulty_ranges: Default::default(),
        }
    }
}

impl Scope {

    fn new(version: i32, content: String) -> Self {
        Scope {
            version,
            content,
            tree: NULL.get().unwrap().clone(),
            ..Default::default()
        }
    }

    fn define(&mut self, url: &Url, node: W<Node>, documentation: String, value: Value) -> std::result::Result<Value, W<Diagnostic>> {
        let name = node.text(self.content.as_bytes())?;
        self.definitions.entry(name.into()).or_default().define(url, node, documentation, value)
    }

    fn undefine(&mut self, url: &Url, node: W<Node>) -> std::result::Result<(), W<Diagnostic>> {
        let name = node.text(self.content.as_bytes())?;
        self.definitions.entry(name.into()).or_default().undefine(url, node)
    }

    fn reference(&mut self, url: &Url, node: W<Node>) -> std::result::Result<Value, W<Diagnostic>> {
        let name = node.text(self.content.as_bytes())?;
        self.definitions.entry(name.into()).or_default().reference(url, node)
    }

    fn define_or_reference(&mut self, url: &Url, node: W<Node>, documentation: String, value: Value) -> std::result::Result<Value, W<Diagnostic>> {
        let name = node.text(self.content.as_bytes())?;
        Ok(self.definitions.entry(name.into()).or_default().define_or_reference(url, node, documentation, value))
    }

    fn find_env_variable(&self, node: W<Node>) -> std::result::Result<Value, W<Diagnostic>> {
        let name = node.text(self.content.as_bytes())?;
        match std::env::var(name) {
            Err(NotPresent) => Err(node.error("environment variable not found")),
            Err(NotUnicode(_)) => Err(node.error("not unicode")),
            Ok(s) => Ok(Value::String(s)),
        }
    }

}

struct Backend {
    client: Client,
    parsers: ParserPool,
    files: DashMap<Url, Scope>,
    builtins: HashMap<Url, String>,
}

impl Backend {

    async fn open_file(&self, url: Url) -> Result<()> {
        let entry = self.files.entry(url.clone());
        if let Entry::Occupied(_) = entry {
            return Ok(());
        }
        drop(entry);
        let content = self.get_content(url.clone()).await.ok_or(Error::Parse)?;
        self.read_file(url, 0, content).await
    }

    async fn read_file(&self, url: Url, version: i32, content: String) -> Result<()> {
        if let Entry::Occupied(entry) = self.files.entry(url.clone()) {
            if entry.get().version > version {
                return Ok(());
            }
        }
        self.preprocessor_parse(url, version, content).await
    }

    async fn preprocessor_parse(&self, url: Url, version: i32, content: String) -> Result<()> {
        self.client.log_message(MessageType::LOG, format!("parsing {}", url)).await;
        let mut parser = self.parsers.get().await?;
        let tree = parser.parse(content.as_bytes(), None).ok_or(Error::Parse)?;
        self.preprocess(url, version, content, tree).await
    }

    fn preprocess(&self, url: Url, version: i32, content: String, tree: Tree) -> BoxFuture<Result<()>> {
        async move {
            self.client.log_message(MessageType::LOG, format!("preprocessing {}", url)).await;
            let mut cursor = tree.walk();
            let mut if_stack = vec![];
            let mut run = true;
            let mut scope = Scope::new(version, content);
            let mut documentation = String::new();
            if !self.builtins.contains_key(&url) {
                for (builtin_url, _) in self.builtins.iter() {
                    if let Some(file_scope) = self.files.get(builtin_url) {
                        scope.definitions.extend(file_scope.definitions.clone());
                        scope.references.insert(builtin_url.clone());
                    }
                }
            }
            while run {
                let traverse = self.preprocessor_enter_node(
                    &url,
                    &mut scope,
                    cursor.node().into(),
                    &mut if_stack,
                    &mut documentation
                ).await;
                match traverse {
                    Err(W(diagnostic)) => scope.diagnostics.push(diagnostic),
                    Ok(true) => {
                        if cursor.goto_first_child() {
                            continue;
                        }
                    },
                    _ => {},
                }
                while !cursor.goto_next_sibling() {
                    if !cursor.goto_parent() {
                        run = false;
                        break;
                    }
                }
            }
            for (_, n_if, n_else) in if_stack {
                scope.diagnostics.push(n_if.error("expected #endif").with(
                    n_else.map(|e| e.related(&url, "#else here"))
                ).0);
            }
            drop(cursor);
            scope.tree = tree;
            self.parse(url, scope).await
        }.boxed()
    }

    async fn parse(&self, url: Url, mut scope: Scope) -> Result<()> {
        self.client.log_message(MessageType::LOG, format!("parsing {}", url)).await;
        if scope.faulty_ranges.is_empty() {
            return self.analyse(url, scope).await;
        }
        let mut parser = self.parsers.get().await?;
        let range = scope.tree.root_node().range();
        let truthy_ranges = W(range).complement(scope.faulty_ranges.clone());
        parser.set_included_ranges(truthy_ranges.as_slice()).map_err(|_| Error::Parse)?;
        scope.tree = parser.parse(scope.content.as_bytes(), None).ok_or(Error::Parse)?;
        parser.set_included_ranges(&[]).map_err(|_| Error::Parse)?;
        self.analyse(url, scope).await
    }

    async fn analyse(&self, url: Url, mut scope: Scope) -> Result<()> {
        self.client.log_message(MessageType::LOG, format!("analysing {}", url)).await;
        let tree = std::mem::replace(&mut scope.tree, NULL.get().cloned().expect("no NULL tree"));
        let mut cursor = tree.walk();
        let mut stack: Vec<Context> = Vec::new();
        let mut run = true;
        let mut documentation = String::new();
        while run {
            let node = W(cursor.node());
            match self.enter_node(&url, &mut scope, node, &mut documentation) {
                Err(W(diagnostic)) => { scope.diagnostics.push(diagnostic); },
                Ok(Some(value)) => if let Some(last) = stack.last_mut() {
                    if let Some(name) = cursor.field_name() {
                        last.named.insert(name.into(), (node, Syntax::Value(value)));
                    }
                    else {
                        last.unnamed.push((node, Syntax::Value(value)));
                    }
                },
                _ => {},
            }
            if cursor.goto_first_child() {
                stack.push(Context::new(std::mem::take(&mut documentation)));
                continue;
            }
            while !cursor.goto_next_sibling() {
                if !cursor.goto_parent() {
                    run = false;
                    break;
                }
                let node = W(cursor.node());
                let context = stack
                    .pop()
                    .ok_or_else(|| scope.diagnostics.push(node.error("internal: docs stack drained").0))
                    .unwrap_or_default();
                match self.exit_node(&url, &mut scope, cursor.node().into(), context) {
                    Err(W(diagnostic)) => { scope.diagnostics.push(diagnostic); },
                    Ok(Some(value)) => if let Some(last) = stack.last_mut() {
                        if let Some(name) = cursor.field_name() {
                            last.named.insert(name.into(), (W(cursor.node()), value));
                        }
                        else {
                            last.unnamed.push((W(cursor.node()), value));
                        }
                    },
                    _ => {},
                };
            }
        }
        drop(cursor);
        scope.tree = tree;
        self.update(url, scope).await
    }

    async fn update(&self, url: Url, scope: Scope) -> Result<()> {
        self.client.log_message(MessageType::LOG, format!("updating diagnostics {}", url)).await;
        self.client.publish_diagnostics(
            url.clone(),
            scope.diagnostics.clone(),
            Some(scope.version)
        ).await;
        let _ = self.files.insert(url.clone(), scope);
        let references: Vec<Url> = self.files.iter().filter_map(|v| if v.value().references.contains(&url) { Some(v.key().clone()) } else { None }).collect();
        let should_refresh = !references.is_empty();
        let items: Vec<(Url, Scope)> = references.into_iter().filter_map(|v| self.files.remove(&v)).collect();
        self.client.log_message(MessageType::LOG, format!("updating dependants {}", url)).await;
        let _ = join_all(
            items
                .into_iter()
                .map(|(reference, scope)| self.preprocess(reference, scope.version, scope.content, scope.tree))
        ).await;
        if should_refresh {
            self.client.log_message(MessageType::LOG, format!("refreshing semantic tokens {}", url)).await;
            self.client.semantic_tokens_refresh().await?;
        }
        Ok(())
    }

    fn preprocess_expression(&self, url: &Url, scope: &mut Scope, node: W<Node>) -> std::result::Result<bool, W<Diagnostic>> {
        Ok(match node.kind() {
            "string" => node.is_empty(),
            "decimal" => node
                .text(scope.content.as_bytes())?
                .parse::<i32>()
                .unwrap_or_default() != 0,
            "identifier" => {
                match scope.reference(url, node)? {
                    Value::Bool(v) => v,
                    _ => false,
                }
            },
            "not" => {
                !self.preprocess_expression(url, scope, node.expect("right")?)?
            },
            "and" => {
                self.preprocess_expression(url, scope, node.expect("left")?)?
                &&
                self.preprocess_expression(url, scope, node.expect("right")?)?
            },
            "or" => {
                self.preprocess_expression(url, scope, node.expect("left")?)?
                ||
                self.preprocess_expression(url, scope, node.expect("right")?)?
            },
            "call" => {
                let argument = node.expect("arguments")?;
                match node.expect("name")?.text(scope.content.as_bytes())? {
                    "exists" => scope.find_env_variable(argument).map(|_| ()),
                    "file_exists" => self.find_file(url, scope, argument.expect("double_quoted")?).and(Ok(())),
                    _ => Err(node.error("unknown macro function")),
                }.and(Ok(true))?
            },
            _ => {
                return Err(node.error("unknown macro expression"));
            }
        })
    }

    fn search_file(&self, url: &Url, path: &str) -> Option<Url> {
        if url.scheme() == "file" {
            if let Ok(current) = std::env::current_dir() {
                if let Ok(path) = Url::from_file_path(current.join(path)) {
                    return Some(path);
                }
            }
        }

        //let mut url = url.clone();
        //let dir = url.to_file_path().ok()?;
        //url.set_path(dir.parent()?.to_str()?);
        url.join(path).ok()
    }

    fn find_file(&self, url: &Url, scope: &Scope, node: W<Node>) -> std::result::Result<Url, W<Diagnostic>> {
        let path = node.text(scope.content.as_bytes())?;
        self.search_file(url, path).ok_or_else(|| node.error("could not find file"))
    }

    async fn get_content(&self, path: Url) -> Option<String> {
        self.client.log_message(MessageType::LOG, format!("reading {}", path)).await;
        let mut file = File::open(path.to_file_path().ok()?).await.ok()?;
        let mut result = String::new();
        file.read_to_string(&mut result).await.ok()?;
        Some(result)
    }

    async fn preprocessor_enter_node<'tree>(&self, url: &Url, scope: &mut Scope, node: W<Node<'tree>>, if_stack: &mut IfStack<'tree>, documentation: &mut String) -> std::result::Result<bool, W<Diagnostic>> {
        let mut traverse = false;
        match node.kind() {
            "if" => {
                let condition = node.expect("condition")?;
                let result = match self.preprocess_expression(url, scope, condition) {
                    Ok(result) => result,
                    Err(diagnostic) => {
                        scope.diagnostics.push(diagnostic.0);
                        false
                    },
                };
                if_stack.push((result, node, None));
            },
            "else" => {
                match if_stack.last() {
                    Some((_, _, None)) => {
                        if let Some(last) = if_stack.last_mut() {
                            last.2 = Some(node);
                        }
                    },
                    Some((_, _, Some(n_else))) => Err(
                        node.error("duplicate #else directive").with(
                            Some(n_else.related(url, "previous #else here"))
                        )
                    )?,
                    None => Err(node.error("expected #if"))?,
                }
            },
            "endif" => {
                let last = if_stack.pop().ok_or_else(|| node.error("expected #if"))?;
                if let Some((start, end)) = match last {
                    (true, _, None) => None,
                    (true, _, Some(n_else)) => Some((n_else, node)),
                    (false, n_if, n_else) => Some((n_if, n_else.unwrap_or(node))),
                } {
                    scope.faulty_ranges.push(tree_sitter::Range {
                        start_byte: start.end_byte(),
                        end_byte: end.start_byte(),
                        start_point: start.end_position(),
                        end_point: end.start_position(),
                    });
                    scope.semantic_tokens.extend(
                        (start.end_position().row + 1..end.start_position().row)
                            .map(|line| SemanticToken {
                                delta_line: line as u32,
                                delta_start: 0,
                                length: u32::max_value(),
                                token_type: 0,
                                token_modifiers_bitset: 0,
                            })
                    );
                }
            },
            _ => {},
        }
        let skip = if_stack.last().map(|(if_condition, _, n_else)| *if_condition != n_else.is_none()).unwrap_or_default();
        if skip {
            return Ok(traverse);
        }
        match node.kind() {
            "if" | "else" | "endif" => {},
            "define" => {
                scope.define(url, node.expect("variable")?, std::mem::take(documentation), Value::Bool(true))?;
            },
            "import" => {
                scope.find_env_variable(node.expect("variable")?)?;
            },
            "undef" => {
                scope.undefine(url, node.expect("variable")?)?;
            },
            "include" => {
                let filename = node.expect("filename")?.expect("double_quoted")?;
                let file_url = self.find_file(url, scope, filename)?;
                self.open_file(file_url.clone()).await.map_err(|_| filename.error("could not read file"))?;
                if let Some(file_scope) = self.files.get(&file_url) {
                    scope.definitions.extend(file_scope.definitions.clone());
                }
                else {
                    Err(filename.error("error while processing file"))?;
                }
                scope.references.insert(file_url.clone());
            },
            "once" => { scope.once = true; },
            "unknown" => { Err(node.error("unknown directive"))? },
            "comment" => {
                let text = node.text(scope.content.as_bytes())?;
                *documentation += text.get(2..).unwrap_or_default();
                documentation.push('\n');
                return Ok(false);
            },
            _ => {
                traverse = true;
            },
        }
        documentation.clear();
        Ok(traverse)
    }

    fn enter_node(&self, _: &Url, scope: &mut Scope, node: W<Node<'_>>, documentation: &mut String) -> std::result::Result<Option<Value>, W<Diagnostic>> {
        let value = match node.kind() {
            "if" | "else" | "endif" | "define" | "import" | "undef" | "include" | "once" | "unknown" => { return Ok(None); },
            "ERROR" => { return Err(node.error("syntax")); },
            "comment" => {
                let text = node.text(scope.content.as_bytes())?;
                *documentation += text.get(2..).unwrap_or_default();
                documentation.push('\n');
                return Ok(None);
            },
            "string" => Value::String(node.expect("double_quoted").or(node.expect("single_quoted"))?.text(scope.content.as_bytes())?.into()),
            "decimal" => Value::Number(node
                .text(scope.content.as_bytes())?
                .parse::<i32>()
                .unwrap_or_default()),
            "boolean" => Value::Bool(node.text(scope.content.as_bytes())? == "true"),
            "identifier" => Value::Identifier(node.text(scope.content.as_bytes())?.into()),
            _ => {
                return Ok(None);
            }
        };
        documentation.clear();
        Ok(Some(value))
    }

    fn exit_node<'tree>(&self, url: &Url, scope: &mut Scope, node: W<Node<'tree>>, mut context: Context<'tree>) -> std::result::Result<Option<Syntax>, W<Diagnostic>> {
        Ok(Some(match node.kind() {
            "usage" => {
                let name = match context.get("variable", node)?.1 {
                    Syntax::Value(Value::Identifier(s)) => s,
                    Syntax::Value(Value::String(s)) => s,
                    _ => Err(node.error("unexpected type"))?,
                };
                let scope = node.expect("scope")?;
                Syntax::Usage(match scope.kind() {
                    "." => Usage::Local(name),
                    "^" => Usage::Parent(name),
                    _ => Err(scope.error("unexpected scope"))?,
                })
            },
            "assign" => Syntax::Assign(self.resolve(url, scope, node, context.get("right", node)?.1)?),
            "subtract" => Syntax::Subtract(self.resolve(url, scope, node, context.get("right", node)?.1)?),
            "concatenate" => Syntax::Concatenate(self.resolve(url, scope, node, context.get("right", node)?.1)?),
            "compound" => {
                let (left, name) = context.get("left", node)?;
                let name = match name {
                    Syntax::Usage(Usage::Local(name)) | Syntax::Usage(Usage::Parent(name)) => name,
                    _ => Err(node.error("expected usage"))?,
                };
                let mut value = scope.definitions.get(&name).and_then(|x| x.value.clone());
                for (n, child) in context.unnamed {
                    let result = match child {
                        Syntax::Assign(e) => Ok(e),
                        Syntax::Concatenate(e) => {
                            match (value.clone(), e) {
                                (None, e) => Ok(e),
                                (Some(Value::String(a)), Value::String(b)) => Ok(Value::String(a + &b)),
                                (Some(Value::Number(a)), Value::Number(b)) => Ok(Value::Number(a + b)),
                                (Some(Value::String(a)), Value::Number(b)) => Ok(Value::String(a + &b.to_string())),
                                (Some(a), b) => Err(n.error(format!("concatenation is unsupported between {} and {}", a.kind(), b.kind()))),
                            }
                        },
                        Syntax::Subtract(e) => {
                            match (value.clone(), e) {
                                (None, e) => Ok(e),
                                (Some(Value::String(a)), Value::String(b)) => Ok(Value::String(
                                    if let Some(index) = a.find(&b) {
                                        a.get(0..index).unwrap_or_default().to_owned() + a.get(index + b.len()..a.len()).unwrap_or_default()
                                    }
                                    else {
                                        a
                                    }
                                )),
                                (Some(Value::Number(a)), Value::Number(b)) => Ok(Value::Number(a - b)),
                                (Some(a), b) => Err(n.error(format!("subtraction is unsupported between {} and {}", a.kind(), b.kind()))),
                            }
                        },
                        _ => Err(n.error("unexpected")),
                    };
                    match result {
                        Ok(v) => { value = Some(v); },
                        Err(W(diagnostic)) => { scope.diagnostics.push(diagnostic); }
                    }
                }
                Syntax::Value(scope.define_or_reference(
                    url,
                    left.expect("variable")?,
                    context.documentation,
                    value.ok_or_else(|| node.error("expected assignment"))?
                )?)
            },
            "function_definition" => {
                Syntax::Value(scope.define(
                    url,
                    node.expect("name")?,
                    std::mem::take(&mut context.documentation),
                    Value::Function
                )?)
            },
            _ => { return Ok(None); },
        }))
    }

    fn resolve(&self, url: &Url, scope: &mut Scope, node: W<Node<'_>>, syntax: Syntax) -> std::result::Result<Value, W<Diagnostic>> {
        Ok(match syntax {
            Syntax::Value(v) => v,
            Syntax::Usage(Usage::Local(n) | Usage::Parent(n)) =>
                scope.definitions.entry(n).or_default().reference(url, node)?,
            _ => Err(node.error("cannot resolve"))?,
        })
    }

    async fn find_documentation(&self, url: &Url, position: Position) -> Option<MarkupContent> {
        let file = self.files.get(url)?;
        let point = W(position).into();
        let node = file.tree.root_node().descendant_for_point_range(point, point).map(W)?;
        let node = if !node.is_named() {
            if node.kind() == "#" {
                node.next_named_sibling()
            }
            else {
                node.parent().or_else(|| node.next_named_sibling())
            }.map(W)?
        }
        else {
            node
        };
        let word = match node.kind() {
            "identifier" => Ok(node),
            "interpolation" => node.expect("variable"),
            "call" | "function_definition" => node.expect("name"),
            "usage" => node.expect("variable"),
            "placeholder" => node.expect("argument"),
            _ => Err(Diagnostic::default()),
        }.ok()?.text(file.content.as_bytes()).ok()?;
        let symbol = file.definitions.get(word)?;
        Some(symbol.documentation.clone())
    }

    async fn suggest_completions(&self, url: Url, position: Position, trigger: String) -> Option<Vec<CompletionItem>> {
        let file = self.files.get(&url)?;
        let point: Point = W(position).into();
        let node = file.tree.root_node().descendant_for_point_range(point, point).map(W)?;
        Some(if trigger == "#" || ["undef", "define", "import", "include", "once", "else", "endif", "undef", "unknown"].contains(&node.kind()) {
            vec![
                CompletionItem {
                    label: "import".into(),
                    insert_text: Some("import ${1:env_var}".into()),
                    insert_text_format: Some(InsertTextFormat::SNIPPET),
                    label_details: Some(CompletionItemLabelDetails {
                        detail: Some("environment variable".into()),
                        description: None,
                    }),
                    kind: Some(CompletionItemKind::SNIPPET),
                    ..Default::default()
                },
                CompletionItem {
                    label: "define".into(),
                    insert_text: Some("define ${1:macro}".into()),
                    insert_text_format: Some(InsertTextFormat::SNIPPET),
                    label_details: Some(CompletionItemLabelDetails {
                        detail: Some("macro".into()),
                        description: None,
                    }),
                    kind: Some(CompletionItemKind::SNIPPET),
                    ..Default::default()
                },
                CompletionItem {
                    label: "include".into(),
                    insert_text: Some("include \"${1:filename}.bff\"".into()),
                    insert_text_format: Some(InsertTextFormat::SNIPPET),
                    label_details: Some(CompletionItemLabelDetails {
                        detail: Some("\"filename\"".into()),
                        description: None,
                    }),
                    kind: Some(CompletionItemKind::SNIPPET),
                    ..Default::default()
                },
                CompletionItem {
                    label: "undef".into(),
                    insert_text: Some("undef ${1:macro}".into()),
                    insert_text_format: Some(InsertTextFormat::SNIPPET),
                    label_details: Some(CompletionItemLabelDetails {
                        detail: Some("macro".into()),
                        description: None,
                    }),
                    kind: Some(CompletionItemKind::SNIPPET),
                    ..Default::default()
                },
                CompletionItem {
                    label: "if".into(),
                    insert_text: Some("if ${1:0}\n$0\n#endif // $1".into()),
                    insert_text_format: Some(InsertTextFormat::SNIPPET),
                    kind: Some(CompletionItemKind::SNIPPET),
                    ..Default::default()
                },
                CompletionItem {
                    label: "endif".into(),
                    insert_text: Some("endif\n".into()),
                    kind: Some(CompletionItemKind::SNIPPET),
                    ..Default::default()
                },
                CompletionItem {
                    label: "once".into(),
                    insert_text: Some("once\n".into()),
                    kind: Some(CompletionItemKind::SNIPPET),
                    ..Default::default()
                },
                CompletionItem {
                    label: "else".into(),
                    insert_text: Some("else\n".into()),
                    kind: Some(CompletionItemKind::SNIPPET),
                    ..Default::default()
                },
            ]
        }
        else {
            file.definitions
                .iter()
                .map(|(name, definition)| CompletionItem {
                    label: name.clone(),
                    kind: Some(CompletionItemKind::VARIABLE),
                    documentation: Some(Documentation::MarkupContent(definition.documentation.clone())),
                    ..Default::default()
                })
                .collect()
        })
    }

    async fn get_declarations(&self, url: &Url, position: Position) -> Option<Vec<Location>> {
        let file = self.files.get(url)?;
        let point = W(position).into();
        let node = file.tree.root_node().descendant_for_point_range(point, point).map(W)?;
        let node = if !node.is_named() {
            if node.kind() == "#" {
                node.next_named_sibling()
            }
            else {
                node.parent().or_else(|| node.next_named_sibling())
            }.map(W)?
        }
        else {
            node
        };
        let word = match node.kind() {
            "identifier" => Ok(node),
            "string" => node.expect("double_quoted"),
            "interpolation" | "define" | "undef" | "import"
                => node.expect("variable"),
            "include"
                => node.expect("filename").ok()?.expect("double_quoted"),
            "call" | "function_definition"
                => node.expect("name"),
            "usage" => node.expect("variable"),
            "placeholder" => node.expect("argument"),
            _ => Err(Diagnostic::default()),
        }.ok()?.text(file.content.as_bytes()).ok()?;
        let symbol = file.definitions.get(word)?;
        Some(vec![symbol.definition.clone()?])
    }

    async fn get_references(&self, url: &Url, position: Position, include_declaration: bool) -> Option<Vec<Location>> {
        let file = self.files.get(url)?;
        let point = W(position).into();
        let node = file.tree.root_node().descendant_for_point_range(point, point).map(W)?;
        let word = match node.kind() {
            "identifier" => Ok(node),
            "interpolation" => node.expect("variable"),
            "call" | "function_definition" => node.expect("name"),
            "usage" => node.expect("variable"),
            "placeholder" => node.expect("argument"),
            _ => Err(Diagnostic::default()),
        }.ok()?.text(file.content.as_bytes()).ok()?;
        let symbol = file.definitions.get(word)?;
        let mut references = Vec::new();
        if include_declaration {
            if let Some(declaration) = &symbol.definition {
                references.push(declaration.clone());
            }
        }
        references.extend(symbol.references.iter().cloned());
        Some(references)
    }

    fn new(client: Client) -> Result<Self> {
        let mut builtins = HashMap::new();
        builtins.insert(Url::parse("memory:///builtins/alias.bff")?, include_str!("../builtins/alias.bff").into());
        match std::env::consts::OS {
            "linux" => builtins.insert(Url::parse("memory:///builtins/linux.bff")?, include_str!("../builtins/linux.bff").into()),
            "windows" => builtins.insert(Url::parse("memory:///builtins/windows.bff")?, include_str!("../builtins/windows.bff").into()),
            "macos" => builtins.insert(Url::parse("memory:///builtins/macos.bff")?, include_str!("../builtins/macos.bff").into()),
            _ => None,
        };
        let language = tree_sitter_fastbuild::language();
        let mut parser = Parser::new();
        parser.set_language(&language)?;
        let tree = parser.parse("", None).ok_or(Error::Parse)?;
        let _ = NULL.set(tree);
        let parsers = new_pool(language)?;
        let files = DashMap::new();
        Ok(Backend { client, parsers, files, builtins })
    }

}

pub fn markdown(value: impl Into<String>) -> MarkupContent {
    MarkupContent { kind: MarkupKind::Markdown, value: value.into() }
}

pub fn hover_markdown(value: impl Into<String>) -> Hover {
    Hover {
        contents: HoverContents::Markup(markdown(value)),
        range: None,
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {

    async fn initialize(&self, _: InitializeParams) -> jsonrpc::Result<InitializeResult> {
        trace!("initialize");
        for (url, content) in self.builtins.iter() {
            let _ = self.read_file(url.clone(), 0, content.clone()).await;
        }
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                declaration_provider: Some(DeclarationCapability::Simple(true)),
                definition_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
                completion_provider: Some(CompletionOptions {
                    trigger_characters: Some(vec!["#"].into_iter().map(|x| x.to_owned()).collect()),
                    ..Default::default()
                }),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensRegistrationOptions(
                        SemanticTokensRegistrationOptions {
                            text_document_registration_options: {
                                TextDocumentRegistrationOptions {
                                    document_selector: Some(vec![
                                        DocumentFilter {
                                            language: Some("fastbuild".into()),
                                            scheme: Some("file".into()),
                                            pattern: Some("*.bff".into())
                                        }
                                    ])
                                }
                            },
                            semantic_tokens_options: SemanticTokensOptions {
                                legend: SemanticTokensLegend {
                                    token_types: vec!(SemanticTokenType::COMMENT),
                                    ..Default::default()
                                },
                                full: Some(SemanticTokensFullOptions::Bool(true)),
                                ..Default::default()
                            },
                            static_registration_options: Default::default(),
                        },
                    )
                ),
                ..Default::default()
            },
            ..Default::default()
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        trace!("initialized");
        self.client.log_message(MessageType::INFO, "server initialized!").await;
    }

    async fn shutdown(&self) -> jsonrpc::Result<()> {
        trace!("shutdown");
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        trace!("textDocument/didOpen {:?}", params.text_document.uri);
        let document = params.text_document;
        if document.language_id != "fastbuild" {
            return;
        }
        self.read_file(
            document.uri,
            document.version,
            document.text,
        ).await.unwrap();
    }

    async fn did_change(&self, mut params: DidChangeTextDocumentParams) {
        trace!("textDocument/didChange {:?}", params.text_document.uri);
        let document = params.text_document;
        if params.content_changes.len() != 1 {
            warn!("file changes: {:?}", params.content_changes);
            return;
        }
        let _ = self.read_file(
            document.uri,
            document.version,
            std::mem::take(&mut params.content_changes[0].text),
        ).await;
    }

    async fn semantic_tokens_full(&self, params: SemanticTokensParams) -> jsonrpc::Result<Option<SemanticTokensResult>> {
        trace!("textDocument/semanticTokens/full {:?}", params);
        let mut prev = SemanticToken::default();
        Ok(self.files
            .get(&params.text_document.uri)
            .map(|f| f.semantic_tokens.iter().map(|t| W(*t).delta(&mut prev)).collect())
            .map(|t| SemanticTokensResult::Tokens(SemanticTokens {
                data: t,
                ..Default::default()
            }))
        )
    }

    // async fn semantic_tokens_range(&self, params: SemanticTokensRangeParams) -> jsonrpc::Result<Option<SemanticTokensRangeResult>> {
    //     info!("textDocument/semanticTokens/full {:?}", params);
    //     Ok(self.files
    //         .get(&params.text_document.uri)
    //         .map(|f| f.semantic_tokens.clone())
    //         .map(|t| SemanticTokensRangeResult::Tokens(SemanticTokens {
    //             data: t,
    //             ..Default::default()
    //         }))
    //     )
    // }

    async fn hover(&self, params: HoverParams) -> jsonrpc::Result<Option<Hover>> {
        trace!("textDocument/hover {:?}", params);
        let document = params.text_document_position_params;
        let documentation = self.find_documentation(&document.text_document.uri, document.position).await;
        Ok(documentation
            .and_then(|s| if s.value.is_empty() { None } else { Some(s) })
            .map(|s| Hover {
                contents: HoverContents::Markup(s),
                range: None,
            })
        )
    }

    async fn completion(&self, params: CompletionParams) -> jsonrpc::Result<Option<CompletionResponse>> {
        trace!("textDocument/completion {:?}", params);
        let pos = params.text_document_position;
        Ok(self.suggest_completions(pos.text_document.uri, pos.position, params.context.and_then(|x| x.trigger_character).unwrap_or_default())
            .await
            .map(CompletionResponse::Array)
        )
    }

    async fn goto_declaration(&self, params: GotoDeclarationParams) -> jsonrpc::Result<Option<GotoDeclarationResponse>> {
        trace!("textDocument/gotoDeclaration");
        let pos = params.text_document_position_params;
        Ok(self.get_declarations(&pos.text_document.uri, pos.position).await
            .map(GotoDeclarationResponse::Array))
    }

    async fn goto_definition(&self, params: GotoDefinitionParams) -> jsonrpc::Result<Option<GotoDefinitionResponse>> {
        self.goto_declaration(params).await
    }

    async fn references(&self, params: ReferenceParams) -> jsonrpc::Result<Option<Vec<Location>>> {
        trace!("textDocument/references {:?}", params);
        let pos = params.text_document_position;
        Ok(self.get_references(&pos.text_document.uri, pos.position, params.context.include_declaration).await)
    }

}

#[tokio::main]
async fn main() -> std::result::Result<(), Box<dyn std::error::Error>> {
    info!("starting LSP server");
    let (service, socket) = LspService::new(|c| Backend::new(c).expect("failed to initialize backend"));

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();
    Server::new(stdin, stdout, socket).serve(service).await;
    Ok(())
}

#[cfg(test)]
mod tests {

    use super::*;

    async fn make() -> LspService<Backend> {
        let (service, _) = LspService::new(|client| Backend::new(client).expect("failed to initialize backend"));
        let backend = service.inner();
        backend.initialize(InitializeParams::default()).await.expect("failed to initialize server");
        backend.initialized(InitializedParams{}).await;
        service
    }

    async fn make_with_file(uri: &str, content: &str) -> (Url, LspService<Backend>) {
        let uri = Url::parse(uri).expect("failed to parse url");
        let service = make().await;
        let backend = service.inner();
        backend.did_open(DidOpenTextDocumentParams {
            text_document: TextDocumentItem {
                uri: uri.clone(),
                language_id: "fastbuild".into(),
                version: 0,
                text: content.into(),
            },
        }).await;
        (uri, service)
    }

    async fn make_file(uri: &str, content: &str) -> (Url, Scope) {
        let uri = Url::parse(uri).expect("failed to parse url");
        let service = make().await;
        let backend = service.inner();
        backend.did_open(DidOpenTextDocumentParams {
            text_document: TextDocumentItem {
                uri: uri.clone(),
                language_id: "fastbuild".into(),
                version: 0,
                text: content.into(),
            },
        }).await;
        (uri.clone(), service.inner().files.remove(&uri).expect("no file").1)
    }

    async fn make_with_files(files: Vec<(&str, &str)>) -> (Vec<Url>, LspService<Backend>) {
        let service = make().await;
        let backend = service.inner();
        let mut urls = Vec::new();
        for (uri, content) in files {
            let uri = Url::parse(uri).expect("failed to parse url");
            backend.did_open(DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: uri.clone(),
                    language_id: "fastbuild".into(),
                    version: 0,
                    text: content.into(),
                },
            }).await;
            urls.push(uri);
        }
        (urls, service)
    }

    #[tokio::test]
    async fn syntax_error() {
        let (_, scope) = make_file("memory:///syntax_error.bff", "=").await;
        let diagnostic = scope.diagnostics.first().expect("no diagnostic");
        assert_eq!(diagnostic.message, "syntax");
        assert_eq!(scope.semantic_tokens, Vec::new());
    }

    #[tokio::test]
    async fn redefinition() {
        let (uri, scope) = make_file("memory:///redefinition.bff", "#define A\n#define A").await;
        let diagnostic = scope.diagnostics.first().expect("no diagnostic");
        assert_eq!(scope.semantic_tokens, Vec::new());
        assert_eq!(diagnostic.message, "macro redefinition");
        let binding = diagnostic.related_information.clone().expect("no related information");
        let related = binding.first().expect("no related entrys");
        assert_eq!(related.message, "defined here");
        assert_eq!(related.location.uri, uri);
        assert_eq!(related.location.range.start, Position::new(0, 8));
        assert_eq!(related.location.range.end, Position::new(0, 9));
    }

    #[tokio::test]
    async fn import_not_found() {
        let (_, scope) = make_file("memory:///import_not_found.bff", "#import __SURELYNOSUCHENVVAR").await;
        assert_eq!(scope.semantic_tokens, Vec::new());
        let diagnostic = scope.diagnostics.first().expect("no diagnostic");
        assert_eq!(diagnostic.message, "environment variable not found");
        assert_eq!(diagnostic.range.start.line, 0);
        assert_eq!(diagnostic.range.start.character, 8);
        assert_eq!(diagnostic.range.end.line, 0);
        assert_eq!(diagnostic.range.end.character, 28);
    }

    #[tokio::test]
    async fn undef() {
        let (_, scope) = make_file("memory:///undefine.bff", "#define A\n#undef A").await;
        assert_eq!(scope.diagnostics, Vec::new());
        assert_eq!(scope.semantic_tokens, Vec::new());
    }

    #[tokio::test]
    async fn undef_undefined() {
        let (_, scope) = make_file("memory:///undefined.bff", "#undef A").await;
        assert_eq!(scope.semantic_tokens, Vec::new());
        let diagnostic = scope.diagnostics.first().expect("no diagnostic");
        assert_eq!(diagnostic.message, "trying to undefine undefined macro");
        assert_eq!(diagnostic.range.start.line, 0);
        assert_eq!(diagnostic.range.start.character, 7);
        assert_eq!(diagnostic.range.end.line, 0);
        assert_eq!(diagnostic.range.end.character, 8);
    }

    #[tokio::test]
    async fn import() {
        let (_, scope) = make_file("memory:///import.bff", "#import PATH").await;
        assert_eq!(scope.diagnostics, vec!());
        assert_eq!(scope.semantic_tokens, Vec::new());
    }

    #[tokio::test]
    async fn once() {
        let (_, scope) = make_file("memory:///once.bff", "#once").await;
        assert!(scope.once);
        assert_eq!(scope.semantic_tokens, Vec::new());
        assert_eq!(scope.diagnostics, Vec::new());
    }

    #[tokio::test]
    async fn preprocessor_if() {
        let (_, scope) = make_file("memory:///preprocessor_if.bff", "#if 1\n.A = 3\n#else\n.B = 4\n#endif").await;
        assert_eq!(scope.diagnostics, Vec::new());
        let token = scope.semantic_tokens.first().expect("no semantic tokens");
        assert_eq!(token.delta_line, 3);
        assert_eq!(token.delta_start, 0);
        assert_eq!(token.length, u32::max_value());
        assert_eq!(token.token_modifiers_bitset, 0);
        assert_eq!(token.token_type, 0);
    }

    #[tokio::test]
    async fn preprocessor_else() {
        let (_, scope) = make_file("memory:///preprocessor_else.bff", "#if 0\n.A = 3\n#else\n.B = 4\n#endif").await;
        assert_eq!(scope.diagnostics, Vec::new());
        let token = scope.semantic_tokens.first().expect("no semantic tokens");
        assert_eq!(token.delta_line, 1, "mismatching lines");
        assert_eq!(token.delta_start, 0, "mismatching columns");
        assert_eq!(token.length, u32::max_value(), "mismatching length");
        assert_eq!(token.token_modifiers_bitset, 0, "mismatching modifiers");
        assert_eq!(token.token_type, 0, "mismatching types");
    }

    #[tokio::test]
    async fn preprocessor_if_evaluation() {
        let (_, scope) = make_file("memory:///preprocessor_if_missing.bff", "#define A\n#if 1\n#undef A\n\n#endif\n#if A\n.A = 3\n#endif").await;
        assert_eq!(scope.diagnostics, Vec::new());
        let token = scope.semantic_tokens.first().expect("no semantic tokens");
        assert_eq!(scope.diagnostics, vec![]);
        assert_eq!(token.delta_line, 6, "mismatching lines");
        assert_eq!(token.delta_start, 0, "mismatching columns");
        assert_eq!(token.length, u32::max_value(), "mismatching length");
        assert_eq!(token.token_modifiers_bitset, 0, "mismatching modifiers");
        assert_eq!(token.token_type, 0, "mismatching types");
    }

    #[tokio::test]
    async fn preprocessor_if_missing() {
        let (_, scope) = make_file("memory:///preprocessor_if_missing.bff", "#else").await;
        assert_eq!(scope.semantic_tokens, Vec::new());
        let diagnostic = scope.diagnostics.first().expect("no diagnostic");
        assert_eq!(diagnostic.message, "expected #if");
        assert_eq!(diagnostic.range.start.line, 0);
        assert_eq!(diagnostic.range.start.character, 0);
        assert_eq!(diagnostic.range.end.line, 0);
        assert_eq!(diagnostic.range.end.character, 5);
    }

    #[tokio::test]
    async fn preprocessor_endif_missing() {
        let (_, scope) = make_file("memory:///preprocessor_if_missing.bff", "#if 1").await;
        assert_eq!(scope.semantic_tokens, Vec::new());
        let diagnostic = scope.diagnostics.first().expect("no diagnostic");
        assert_eq!(diagnostic.message, "expected #endif");
        assert_eq!(diagnostic.range.start.line, 0);
        assert_eq!(diagnostic.range.start.character, 0);
        assert_eq!(diagnostic.range.end.line, 0);
        assert_eq!(diagnostic.range.end.character, 5);
    }

    #[tokio::test]
    async fn preprocessor_unknown() {
        let (_, scope) = make_file("memory:///preprocessor_unknown.bff", "#unknown").await;
        assert_eq!(scope.semantic_tokens, Vec::new());
        let diagnostic = scope.diagnostics.first().expect("no diagnostic");
        assert_eq!(diagnostic.message, "unknown directive");
        assert_eq!(diagnostic.range.start.line, 0);
        assert_eq!(diagnostic.range.start.character, 0);
        assert_eq!(diagnostic.range.end.line, 0);
        assert_eq!(diagnostic.range.end.character, 8);
    }

    #[tokio::test]
    async fn preprocessor_include() {
        let (_, scope) = make_file("memory:///builtins/preprocessor_include.bff", "#include \"alias.bff\"").await;
        assert_eq!(scope.diagnostics, Vec::new());
        assert_eq!(scope.semantic_tokens, Vec::new());
    }

    #[tokio::test]
    async fn preprocessor_if_file_exists() {
        let (_, scope) = make_file("memory:///builtins/preprocessor_if_file_exists.bff", "#if file_exists(\"alias.bff\")\n#else\n.A = 3\n#endif").await;
        assert_eq!(scope.diagnostics, vec![]);
        assert_eq!(scope.semantic_tokens.first(), Some(&SemanticToken {
            delta_line: 2,
            delta_start: 0,
            length: u32::max_value(),
            token_type: 0,
            token_modifiers_bitset: 0,
        }));
    }

    #[tokio::test]
    async fn os() {
        let (_, scope) = make_file("memory:///os.bff", "").await;
        assert_eq!(scope.semantic_tokens, Vec::new());
        assert_eq!(scope.diagnostics, Vec::new());
        match std::env::consts::OS {
            "linux" => {
                assert!(matches!(scope.definitions.get("__LINUX__"), Some(Symbol { value: Some(Value::Bool(true)), .. })));
                assert!(scope.definitions.get("__WINDOWS__").is_none());
                assert!(scope.definitions.get("__OSX__").is_none());
            },
            "macos" => {
                assert!(scope.definitions.get("__LINUX__").is_none());
                assert!(scope.definitions.get("__WINDOWS__").is_none());
                assert!(matches!(scope.definitions.get("__OSX__"), Some(Symbol { value: Some(Value::Bool(true)), .. })));
            },
            "windows" => {
                assert!(scope.definitions.get("__LINUX__").is_none());
                assert!(matches!(scope.definitions.get("__WINDOWS__"), Some(Symbol { value: Some(Value::Bool(true)), .. })));
                assert!(scope.definitions.get("__OSX__").is_none());
            },
            _ => {},
        }
    }

    #[tokio::test]
    async fn multi_include() {
        let (urls, service) = make_with_files(vec![
            ("memory:///multi_include1.bff", "\n#define A\n#if 0\n.A = 3\n#else\n#undef A\n#endif"),
            ("memory:///multi_include2.bff", "#include \"multi_include1.bff\"\n#define B\n#if A\n#undef B\n#endif"),
            ("memory:///multi_include3.bff", "#include \"multi_include2.bff\"\n#define C\n#if !B\n#undef C\n#endif"),
        ]).await;
        let backend = service.inner();
        for x in backend.files.iter() {
            let scope = x.value();
            let url = x.key();
            assert_eq!(scope.diagnostics, Vec::new());
            if urls.contains(url) {
                assert_eq!(scope.semantic_tokens, vec![SemanticToken {
                    delta_line: 3,
                    delta_start: 0,
                    length: u32::max_value(),
                    token_type: 0,
                    token_modifiers_bitset: 0,
                }]);
            }
            else {
                assert_eq!(scope.semantic_tokens, Vec::new());
            }
        }
    }

    #[tokio::test]
    async fn define_with_documentation() {
        let (_, scope) = make_file("memory:///define_with_documentation.bff", "; documentation\n; for A\n#define A").await;
        assert_eq!(scope.diagnostics, Vec::new());
        assert_eq!(scope.semantic_tokens, Vec::new());
        let definition = scope.definitions.get("A").expect("undefined");
        assert_eq!(definition.documentation, markdown("documentation\nfor A\n"));
    }

    #[tokio::test]
    async fn hover() {
        let (uri, service) = make_with_file("memory:///hover.bff", "; documentation\n; for A\n#define A").await;
        let backend = service.inner();
        let scope = backend.files.get(&uri).expect("no file");
        assert_eq!(scope.diagnostics, Vec::new());
        assert_eq!(scope.semantic_tokens, Vec::new());
        let hover = backend.hover(HoverParams {
            text_document_position_params: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier::new(uri.clone()),
                position: Position::new(2, 8),
            },
            work_done_progress_params: WorkDoneProgressParams {
                work_done_token: None,
            },
        }).await.expect("no hover").expect("no hover");
        assert_eq!(hover, hover_markdown("documentation\nfor A\n"));
    }

    #[tokio::test]
    async fn complete_preprocessor() {
        let (uri, service) = make_with_file("memory:///complete_preprocessor.bff", "#").await;
        let backend = service.inner();
        let scope = backend.files.get(&uri).expect("no file");
        assert_eq!(scope.semantic_tokens, Vec::new());
        let completion = backend.completion(CompletionParams {
            text_document_position: TextDocumentPositionParams::new(
                TextDocumentIdentifier::new(uri),
                Position::new(0, 1),
            ),
            work_done_progress_params: WorkDoneProgressParams { work_done_token: None, },
            partial_result_params: PartialResultParams { partial_result_token: None },
            context: Some(CompletionContext {
                trigger_kind: CompletionTriggerKind::TRIGGER_CHARACTER,
                trigger_character: Some("#".to_owned()),
            }),
        }).await.expect("no completion").expect("no completion");
        if let CompletionResponse::Array(completion) = completion {
            assert_eq!(completion[0].label, "import");
        }
        else {
            panic!();
        }
    }

    #[tokio::test]
    async fn complete_value() {
        let (uri, service) = make_with_file("memory:///complete_value.bff", "#if ").await;
        let backend = service.inner();
        let scope = backend.files.get(&uri).expect("no file");
        assert_eq!(scope.semantic_tokens, Vec::new());
        let completion = backend.completion(CompletionParams {
            text_document_position: TextDocumentPositionParams::new(
                TextDocumentIdentifier::new(uri),
                Position::new(0, 3),
            ),
            work_done_progress_params: WorkDoneProgressParams { work_done_token: None, },
            partial_result_params: PartialResultParams { partial_result_token: None },
            context: None,
        }).await.expect("no completion").expect("no completion");
        if let CompletionResponse::Array(completion) = completion {
            assert!(!completion.is_empty())
        }
        else {
            panic!("wrong completion type")
        }
    }

    #[tokio::test]
    async fn goto_declaration() {
        let (uri, service) = make_with_file("memory:///goto_declaration.bff", "#define A\n#if A\n#endif").await;
        let backend = service.inner();
        let scope = backend.files.get(&uri).expect("no file");
        assert_eq!(scope.semantic_tokens, Vec::new());
        assert_eq!(scope.diagnostics, Vec::new());
        let declatations = backend.goto_declaration(GotoDeclarationParams {
            text_document_position_params: TextDocumentPositionParams::new(TextDocumentIdentifier::new(uri.clone()), Position::new(1, 4)),
            work_done_progress_params: WorkDoneProgressParams { work_done_token: None },
            partial_result_params: PartialResultParams { partial_result_token: None },
        }).await.expect("no completion").expect("no completion");
        if let GotoDefinitionResponse::Array(declarations) = declatations {
            let declaration = declarations.first().expect("no declatations");
            assert_eq!(declaration.uri, uri);
            assert_eq!(declaration.range.start, Position::new(0, 8));
            assert_eq!(declaration.range.end, Position::new(0, 9));
        }
        else {
            panic!("wrong completion type")
        }
    }

    #[tokio::test]
    async fn goto_definition() {
        let (uri, service) = make_with_file("memory:///goto_definition.bff", "#define A\n#if A\n#endif").await;
        let backend = service.inner();
        let scope = backend.files.get(&uri).expect("no file");
        assert_eq!(scope.semantic_tokens, Vec::new());
        assert_eq!(scope.diagnostics, Vec::new());
        let declatations = backend.goto_definition(GotoDefinitionParams {
            text_document_position_params: TextDocumentPositionParams::new(TextDocumentIdentifier::new(uri.clone()), Position::new(1, 4)),
            work_done_progress_params: WorkDoneProgressParams { work_done_token: None },
            partial_result_params: PartialResultParams { partial_result_token: None },
        }).await.expect("no completion").expect("no completion");
        if let GotoDefinitionResponse::Array(definitions) = declatations {
            let definition = definitions.first().expect("no declatations");
            assert_eq!(definition.uri, uri);
            assert_eq!(definition.range.start, Position::new(0, 8));
            assert_eq!(definition.range.end, Position::new(0, 9));
        }
        else {
            panic!("wrong completion type")
        }
    }

    #[tokio::test]
    async fn references() {
        let (uri, service) = make_with_file("memory:///references.bff", "#define A\n#if A\n#endif").await;
        let backend = service.inner();
        let scope = backend.files.get(&uri).expect("no file");
        assert_eq!(scope.semantic_tokens, Vec::new());
        assert_eq!(scope.diagnostics, Vec::new());
        let references = backend.references(ReferenceParams {
            text_document_position: TextDocumentPositionParams::new(TextDocumentIdentifier::new(uri.clone()), Position::new(1, 4)),
            work_done_progress_params: WorkDoneProgressParams { work_done_token: None },
            partial_result_params: PartialResultParams { partial_result_token: None },
            context: ReferenceContext { include_declaration: true },
        }).await.expect("no completion").expect("no completion");
        let reference = references.first().expect("no references");
        assert_eq!(reference.uri, uri);
        assert_eq!(reference.range.start, Position::new(0, 8));
        assert_eq!(reference.range.end, Position::new(0, 9));
        let reference = references.get(1).expect("no references");
        assert_eq!(reference.uri, uri);
        assert_eq!(reference.range.start, Position::new(1, 4));
        assert_eq!(reference.range.end, Position::new(1, 5));
    }

    #[tokio::test]
    async fn references_without_declaration() {
        let (uri, service) = make_with_file("memory:///references_without_declaration.bff", "#define A\n#if A\n#endif").await;
        let backend = service.inner();
        let scope = backend.files.get(&uri).expect("no file");
        assert_eq!(scope.semantic_tokens, Vec::new());
        assert_eq!(scope.diagnostics, Vec::new());
        let references = backend.references(ReferenceParams {
            text_document_position: TextDocumentPositionParams::new(TextDocumentIdentifier::new(uri.clone()), Position::new(1, 4)),
            work_done_progress_params: WorkDoneProgressParams { work_done_token: None },
            partial_result_params: PartialResultParams { partial_result_token: None },
            context: ReferenceContext { include_declaration: false },
        }).await.expect("no completion").expect("no completion");
        let reference = references.first().expect("no references");
        assert_eq!(reference.uri, uri);
        assert_eq!(reference.range.start, Position::new(1, 4));
        assert_eq!(reference.range.end, Position::new(1, 5));
    }

    #[tokio::test]
    async fn function_definition() {
        let (_, scope) = make_file("memory:///function_definition.bff", "; documentation\n; for A\nfunction A() {}").await;
        assert_eq!(scope.diagnostics, Vec::new());
        assert_eq!(scope.semantic_tokens, Vec::new());
        let definition = scope.definitions.get("A").expect("undefined");
        assert_eq!(definition.documentation, markdown("documentation\nfor A\n"));
    }

    #[tokio::test]
    async fn variable_definition() {
        let (uri, scope) = make_file("memory:///variable_definition.bff", "; documentation\n; for A\n.A = 3").await;
        assert_eq!(scope.diagnostics, Vec::new());
        assert_eq!(scope.semantic_tokens, Vec::new());
        let definition = scope.definitions.get("A").expect("undefined");
        assert_eq!(definition.documentation, markdown("documentation\nfor A\n"));
        assert_eq!(definition.value, Some(Value::Number(3)));
        assert_eq!(definition.definition, Some(Location::new(uri, Range::new(Position::new(2, 1), Position::new(2, 2)))));
    }

    #[tokio::test]
    async fn nested_declaration() {
        let (uri, scope) = make_file("memory:///nested_declaration.bff", "function A() {\n; documentation\n; for B\n.B = 3\n}").await;
        assert_eq!(scope.diagnostics, Vec::new());
        assert_eq!(scope.semantic_tokens, Vec::new());
        let definition = scope.definitions.get("B").expect("undefined");
        assert_eq!(definition.documentation, markdown("documentation\nfor B\n"));
        assert_eq!(definition.value, Some(Value::Number(3)));
        assert_eq!(definition.definition, Some(Location::new(uri, Range::new(Position::new(3, 1), Position::new(3, 2)))));
    }

    #[tokio::test]
    async fn nested_preprocessor() {
        let (_, scope) = make_file("memory:///nested_preprocessor.bff", "function A() {\n#if 1\n.B = 3\n#else\n.C = 4\n#endif\n}").await;
        assert_eq!(scope.diagnostics, Vec::new());
        assert_eq!(scope.semantic_tokens, vec![SemanticToken {
            delta_start: 0,
            delta_line: 4,
            length: u32::max_value(),
            token_type: 0,
            token_modifiers_bitset: 0,
        }]);
    }

    #[tokio::test]
    async fn preprocessor_in_compound() {
        let (_, scope) = make_file("memory:///preprocessor_in_compound.bff", "function A() {.B = \"B\"\n#if 1\n+ \"A\"\n#else\n- \"B\"\n#endif\n}").await;
        assert_eq!(scope.diagnostics, Vec::new());
        assert_eq!(scope.semantic_tokens, vec![SemanticToken {
            delta_start: 0,
            delta_line: 4,
            length: u32::max_value(),
            token_type: 0,
            token_modifiers_bitset: 0,
        }]);
    }

    #[tokio::test]
    async fn preprocessor_in_name() {
        let (_, scope) = make_file("memory:///preprocessor_in_name.bff", "function\n#if 0\nA\n#else\nB\n#endif\n() {}").await;
        assert_eq!(scope.diagnostics, Vec::new());
        assert_eq!(scope.semantic_tokens, vec![SemanticToken {
            delta_start: 0,
            delta_line: 2,
            length: u32::max_value(),
            token_type: 0,
            token_modifiers_bitset: 0,
        }]);
        let symbol = scope.definitions.get("B").expect("wrong symbol name");
        assert_eq!(symbol.value, Some(Value::Function));
    }

    #[tokio::test]
    async fn reassign_number() {
        let (_, scope) = make_file("memory:///reassign.bff", ".A = 3\n.A = 4").await;
        assert_eq!(scope.diagnostics, Vec::new());
        assert_eq!(scope.semantic_tokens, Vec::new());
        let symbol = scope.definitions.get("A").expect("undefined");
        assert_eq!(symbol.value, Some(Value::Number(4)));
    }

    #[tokio::test]
    async fn reassign_string() {
        let (_, scope) = make_file("memory:///reassign_string.bff", ".A = 'a'\n.A = 'b'").await;
        assert_eq!(scope.diagnostics, Vec::new());
        assert_eq!(scope.semantic_tokens, Vec::new());
        let symbol = scope.definitions.get("A").expect("undefined");
        assert_eq!(symbol.value, Some(Value::String("b".into())));
    }

    #[tokio::test]
    async fn concat_string() {
        let (_, scope) = make_file("memory:///concat_string.bff", ".A = 'a'\n+ 'b'").await;
        assert_eq!(scope.diagnostics, Vec::new());
        assert_eq!(scope.semantic_tokens, Vec::new());
        let symbol = scope.definitions.get("A").expect("undefined");
        assert_eq!(symbol.value, Some(Value::String("ab".into())));
    }

    #[tokio::test]
    async fn add_number() {
        let (_, scope) = make_file("memory:///add_number.bff", ".A = 3\n+ 4").await;
        assert_eq!(scope.diagnostics, Vec::new());
        assert_eq!(scope.semantic_tokens, Vec::new());
        let symbol = scope.definitions.get("A").expect("undefined");
        assert_eq!(symbol.value, Some(Value::Number(7)));
    }

    #[tokio::test]
    async fn subtract_string() {
        let (_, scope) = make_file("memory:///subtract_string.bff", ".A = 'a and other'\n- 'a'").await;
        assert_eq!(scope.diagnostics, Vec::new());
        assert_eq!(scope.semantic_tokens, Vec::new());
        let symbol = scope.definitions.get("A").expect("undefined");
        assert_eq!(symbol.value, Some(Value::String(" and other".into())));
    }

    #[tokio::test]
    async fn subtract_number() {
        let (_, scope) = make_file("memory:///subtract_number.bff", ".A = 3\n- 2").await;
        assert_eq!(scope.diagnostics, Vec::new());
        assert_eq!(scope.semantic_tokens, Vec::new());
        let symbol = scope.definitions.get("A").expect("undefined");
        assert_eq!(symbol.value, Some(Value::Number(1)));
    }

    #[tokio::test]
    async fn wrong_typing() {
        let (_, scope) = make_file("memory:///wrong_typing.bff", ".A = 3\n- 'a'").await;
        assert_eq!(scope.semantic_tokens, Vec::new());
        let diagnostic = scope.diagnostics.first().expect("no diagnostics");
        assert_eq!(diagnostic.message, "subtraction is unsupported between Number and String");
        assert_eq!(diagnostic.range.start, Position::new(1, 0));
        assert_eq!(diagnostic.range.end, Position::new(1, 5));
    }

}
