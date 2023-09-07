use super::{Parse, PeekLexer, ParseResult, Tokens, WagParseError};
use crate::lexer::{ident::Ident, productions::{ImportType, Productions}, UnsafeNext, Spannable};

use super::rhs::Rhs;
use wagon_macros::match_error;

#[derive(PartialEq, Debug)]
pub(crate) enum Rule {
	Analytic(String, Vec<Rhs>),
	Generate(String, Vec<Rhs>),
	Import(String, ImportType, String),
	Exclude(String, Vec<String>)
}

impl Parse for Rule {
    fn parse(lexer: &mut PeekLexer) -> ParseResult<Self> {
        let ident = match_error!(match lexer.next_unwrap() {
        	Tokens::ProductionToken(Productions::Identifier(Ident::Unknown(s))) => Ok(s),
        })?;
        let resp = match_error!(match lexer.next_unwrap() {
        	Tokens::ProductionToken(Productions::Produce) => Ok(Self::Analytic(ident, Rhs::parse_sep(lexer, Tokens::ProductionToken(Productions::Alternative))?)),
        	Tokens::ProductionToken(Productions::Generate) => Ok(Self::Generate(ident, Rhs::parse_sep(lexer, Tokens::ProductionToken(Productions::Alternative))?)),
        	Tokens::ProductionToken(Productions::Import(i)) => {
        		match i {
        			ImportType::Basic | ImportType::Full | ImportType::Recursive => {
        				match lexer.next_unwrap() {
        					Tokens::ProductionToken(Productions::Identifier(Ident::Unknown(s))) => Ok(Self::Import(ident, i, s)),
        					error => Err(WagParseError::Unexpected {span: lexer.span(), offender: error, expected: vec![Tokens::ProductionToken(Productions::Identifier(Default::default())).to_string()]})
        				}
        			}
        			ImportType::Exclude => Ok(Self::Exclude(ident, String::parse_sep(lexer, Tokens::ProductionToken(Productions::Additional))?))
        		}
        	}
        });
        match_error!(match lexer.next_unwrap() {
        	Tokens::ProductionToken(Productions::Semi) => resp,
        })
    }
}