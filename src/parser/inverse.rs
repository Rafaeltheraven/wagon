use crate::lexer::UnsafePeek;
use super::{Parse, PeekLexer, ParseResult, Tokens};


use crate::lexer::{math::Math};

use super::comp::Comparison;

#[derive(PartialEq, Debug)]
pub(crate) enum Inverse {
	Not(Box<Inverse>),
	Comparison(Comparison)
}

impl Parse for Inverse {

    fn parse(lexer: &mut PeekLexer) -> ParseResult<Self> where Self: Sized {
        match lexer.peek_unwrap() {
        	Tokens::MathToken(Math::Not) => {lexer.next(); Ok(Self::Not(Box::new(Self::parse(lexer)?)))},
        	_ => Ok(Self::Comparison(Comparison::parse(lexer)?))
        }
    }

}