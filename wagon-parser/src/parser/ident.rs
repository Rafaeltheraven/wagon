use wagon_lexer::Spannable;
use wagon_lexer::productions::Productions;
use crate::parser::Tokens;
use wagon_lexer::math::Math;
use super::WagParseError;
use wagon_macros::match_error;

use crate::{either_token};
use wagon_lexer::UnsafePeek;

use super::{Parse, Ident};

impl Parse for Ident {
    fn parse(lexer: &mut wagon_lexer::PeekLexer) -> super::ParseResult<Self> {
    	let next: Tokens = lexer.peek_unwrap().to_owned();
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
