use crate::lexer::{UnsafeNext, UnsafePeek, Spannable};
use super::{Parse, PeekLexer, ParseResult, Tokens, WagParseError};
use crate::lexer::productions::{Productions, GrammarType};

#[derive(PartialEq, Debug)]
pub(crate) struct Metadata {
	pub(crate) includes: Vec<String>,
	pub(crate) spec: Option<GrammarType>
}

impl Parse for Metadata {
    fn parse(lexer: &mut PeekLexer) -> ParseResult<Self> {
        let mut includes = Vec::new();
        while let Tokens::ProductionToken(Productions::Include(_)) = lexer.peek_unwrap() {
            if let Some(Ok(Tokens::ProductionToken(Productions::Include(s)))) = lexer.next() {
                includes.push(s);
            } else {
                return Err(WagParseError::Fatal((lexer.span(), "Something went terribly wrong unwrapping the includes".to_string())))
            }
        }
        if let Tokens::ProductionToken(Productions::GrammarSpec(_)) = lexer.peek_unwrap() {
        	if let Tokens::ProductionToken(Productions::GrammarSpec(s)) = lexer.next_unwrap() {
        		Ok(Self {includes, spec: Some(s)})
        	} else {
        		Err(WagParseError::Fatal((lexer.span(), "Something went terribly wrong unwrapping the grammarspec token".to_string())))
        	}
        } else {
        	Ok(Self {includes, spec: None})
        }
    }
}