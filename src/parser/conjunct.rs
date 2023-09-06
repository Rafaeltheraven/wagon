use super::{Parse, PeekLexer, ParseResult, Tokens};
use crate::lexer::{math::Math};

use super::inverse::Inverse;

#[derive(PartialEq, Debug)]
pub(crate) struct Conjunct(pub(crate) Vec<Inverse>);

impl Parse for Conjunct {
    
    fn parse(lexer: &mut PeekLexer) -> ParseResult<Self> where Self: Sized {
        Ok(Self(Inverse::parse_sep(lexer, Tokens::MathToken(Math::Or))?))
    }

}