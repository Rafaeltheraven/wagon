
use std::{fmt::Display, write};

use super::{Parse, LexerBridge, ParseResult, Tokens, WagParseError, Spannable, ResultNext};
use wagon_lexer::productions::Productions;
use wagon_macros::match_error;


#[derive(PartialEq, Debug, Eq, Hash, Clone)]
/// A terminal for the grammar.
pub enum Terminal {
    /// A terminal described as a regex.
	Regex(String),
    /// A string to exactly match.
	LitString(String)
}

impl Parse for Terminal {
    fn parse(lexer: &mut LexerBridge) -> ParseResult<Self> where Self: Sized {
        match_error!(match lexer.next_result()? {
        	Tokens::ProductionToken(Productions::LitString(x)) => Ok(Self::LitString(x)),
        	Tokens::ProductionToken(Productions::LitRegex(x)) => Ok(Self::Regex(x)),
        })
    }
}

impl Display for Terminal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Terminal::Regex(r) => write!(f, "/{}/", r),
            Terminal::LitString(s) => write!(f, "'{}'", s),
        }
    }
}