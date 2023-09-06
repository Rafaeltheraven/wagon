use crate::lexer::{UnsafePeek, UnsafeNext};
use super::{Parse, PeekLexer, ParseResult, Tokens, Spannable, WagParseError};
use crate::lexer::productions::{Productions, EbnfType};
use super::symbol::Symbol;

/*
 Chunks are symbols in () with optionally an EBNF token following it.
 If there are no (), there is only 1 symbol, which may still optionally have an EBNF token.
*/
#[derive(PartialEq, Debug)]
pub(crate) struct Chunk {
	pub(crate) symbols: Vec<Symbol>,
	pub(crate) ebnf: Option<EbnfType>
}

impl Parse for Chunk {
	fn parse(lexer: &mut PeekLexer) -> ParseResult<Self> { 
		let symbols = match lexer.peek_unwrap() {
			Tokens::ProductionToken(Productions::LPar) => {
				let mut ret = Vec::new();
				lexer.next();
				while lexer.peek_unwrap() != &Tokens::ProductionToken(Productions::RPar) {
					ret.push(Symbol::parse(lexer)?);
				}
				lexer.next();
				ret
			},
			_ => {
				vec![Symbol::parse(lexer)?]
			}
		};
		if let Tokens::ProductionToken(Productions::Ebnf(_)) = lexer.peek_unwrap() {
			if let Tokens::ProductionToken(Productions::Ebnf(x)) = lexer.next_unwrap() {
				Ok(Self {symbols, ebnf: Some(x)})
			} else { 
    			Err(WagParseError::Fatal((lexer.span(), "Something went terribly wrong. Unwrapped non-ebnf when should have unwrapped ebnf".to_string())))  
    		}
		} else {
			Ok(Self {symbols, ebnf: None})
		}
	}
}