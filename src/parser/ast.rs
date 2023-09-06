use super::{Parse, ParseResult, PeekLexer};
use super::metadata::Metadata;
use super::rule::Rule;
use std::matches;

#[derive(PartialEq, Debug)]
pub(crate) struct Wag {
	pub(crate) metadata: Metadata,
	pub(crate) grammar: Vec<Rule>,
}

impl Parse for Wag {
    fn parse(lexer: &mut PeekLexer) -> ParseResult<Self> {
        let metadata = Metadata::parse(lexer)?;
        let mut grammar = Vec::new();
        while matches!(lexer.peek(), Some(_)) {
        	grammar.push(Rule::parse(lexer)?);
        }
        Ok(Self {metadata, grammar})
    }
}
