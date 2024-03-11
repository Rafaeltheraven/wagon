
use crate::parser::Tokens;

use super::{WagParseError, Spannable};
use wagon_macros::match_error;

use crate::either_token;

use super::{Parse, Ident, ResultPeek};

impl Parse for Ident {
    fn parse(lexer: &mut wagon_lexer::LexerBridge) -> super::ParseResult<Self> {
    	let next: Tokens = lexer.peek_result()?.to_owned();
    	match_error!(
	        match next {
	        	#[expect("identifier")]
	        	either_token!(Identifier(_)) => {
	        		let real = lexer.next();
	        		match real {
					    Some(Ok(x)) => {
					    	match_error!(
						    	match x {
						    		#[expect("identifier")]
						    		either_token!(Identifier(y)) => Ok(y),
						    	}
						    )
					    },
					    _ => Err(WagParseError::Fatal((lexer.span(), "Unable to properly unwrap identifier".to_string()))),
					}
	        	}
	        }
	    )
    }
}
