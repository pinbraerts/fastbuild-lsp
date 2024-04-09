use deadpool::managed::{self, BuildError};
use tree_sitter::{Language, Parser, LanguageError};

pub struct ParserManager {
    language: Language,
}

#[async_trait::async_trait]
impl managed::Manager for ParserManager {
    type Type = Parser;
    type Error = LanguageError;

    async fn create(&self) -> Result<Parser, Self::Error> {
        let mut parser = Parser::new();
        parser.set_language(&self.language)?;
        Ok(parser)
    }

    async fn recycle(&self, _: &mut Parser, _: &managed::Metrics) -> managed::RecycleResult<Self::Error> {
        Ok(())
    }

}

pub type ParserPool = managed::Pool<ParserManager>;

pub fn new_pool(language: tree_sitter::Language) -> Result<ParserPool, BuildError> {
    let manager = ParserManager { language };
    ParserPool::builder(manager).build()
}
