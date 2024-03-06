
use derivative::Derivative;
use std::{fmt::Display, write};

use regex_automata::dfa::dense::DFA;

use super::{Parse, LexerBridge, ParseResult, Tokens, WagParseError, Spannable, ResultNext};
use wagon_lexer::productions::Productions;
use wagon_macros::match_error;


#[derive(Debug, Clone, Derivative)]
#[derivative(PartialEq, Hash)]
/// A terminal for the grammar.
///
/// # Grammar
/// `Terminal -> Regex | String;`
pub enum Terminal {
    /// A terminal described as a regex. 
	Regex(
        /// The pattern
        String, 
        #[derivative(PartialEq="ignore")]
        #[derivative(Hash="ignore")]
        /// A fully valid [`DFA`] which can either be used correctly, or serialized as needed.
        Box<DFA<Vec<u32>>> // DFA is on the heap because it is very big.
    ), 
    /// A string to exactly match.
	LitString(String)
}

impl Parse for Terminal {
    fn parse(lexer: &mut LexerBridge) -> ParseResult<Self> where Self: Sized {
        match_error!(match lexer.next_result()? {
        	Tokens::ProductionToken(Productions::LitString(x)) => Ok(Self::LitString(x)),
        	Tokens::ProductionToken(Productions::LitRegex(x)) => {
                match DFA::new(&x) {
                    Ok(dfa) => Ok(Self::Regex(x, Box::new(dfa))),
                    Err(e) => Err(WagParseError::RegexError(Box::new(e), lexer.span(), x))
                }
            },
        })
    }
}

impl Display for Terminal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Regex(r, _) => write!(f, "/{r}/"),
            Self::LitString(s) => write!(f, "'{s}'"),
        }
    }
}

impl Eq for Terminal {}