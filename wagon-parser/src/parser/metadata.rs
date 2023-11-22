use wagon_macros::match_error;

use wagon_lexer::{UnsafeNext, UnsafePeek, Spannable};
use super::{Parse, PeekLexer, ParseResult, Tokens, WagParseError, helpers::check_semi};
use wagon_lexer::productions::{Productions, GrammarType};

#[cfg(test)]
use wagon_macros::new_unspanned;

#[derive(PartialEq, Debug, Eq, Hash)]
#[cfg_attr(test, new_unspanned)]
pub struct Metadata {
	pub includes: Vec<String>,
	pub spec: Option<GrammarType>
}

impl Parse for Metadata {
    fn parse(lexer: &mut PeekLexer) -> ParseResult<Self> {
        let mut includes = Vec::new();
        while lexer.next_if_eq(&Ok(Tokens::ProductionToken(Productions::Include))).is_some() {
            match_error!(match lexer.next_unwrap() {
                Tokens::ProductionToken(Productions::Path(p)) => {includes.push(p); Ok(())}
            })?;
            check_semi(lexer)?;
        }
        if let Tokens::ProductionToken(Productions::GrammarSpec(_)) = lexer.peek_unwrap() {
        	if let Tokens::ProductionToken(Productions::GrammarSpec(s)) = lexer.next_unwrap() {
                check_semi(lexer)?;
        		Ok(Self {includes, spec: Some(s)})
        	} else {
        		Err(WagParseError::Fatal((lexer.span(), "Something went terribly wrong unwrapping the grammarspec token".to_string())))
        	}
        } else {
        	Ok(Self {includes, spec: None})
        }

    }
}