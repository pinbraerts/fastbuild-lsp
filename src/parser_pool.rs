use deadpool::managed::{self, BuildError};
use tree_sitter::{Language, Parser};

#[derive(Debug)]
pub enum Error { Fail }

pub struct ParserManager {
    language: Language,
}

#[async_trait::async_trait]
impl managed::Manager for ParserManager {
    type Type = Parser;
    type Error = Error;

    async fn create(&self) -> Result<Parser, Error> {
        let mut parser = Parser::new();
        parser.set_language(&self.language).map_err(|_| Error::Fail)?;
        Ok(parser)
    }

    async fn recycle(&self, _: &mut Parser, _: &managed::Metrics) -> managed::RecycleResult<Error> {
        Ok(())
    }

}

pub type ParserPool = managed::Pool<ParserManager>;

pub fn new_pool(language: tree_sitter::Language) -> Result<ParserPool, BuildError> {
    let manager = ParserManager { language };
    ParserPool::builder(manager).build()
}
