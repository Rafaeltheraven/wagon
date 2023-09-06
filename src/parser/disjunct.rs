use super::{Parse, PeekLexer, ParseResult, Tokens};
use crate::lexer::{math::Math};
use super::conjunct::Conjunct;

#[derive(PartialEq, Debug)]
pub(crate) struct Disjunct(pub(crate) Vec<Conjunct>);

impl Parse for Disjunct {
    
    fn parse(lexer: &mut PeekLexer) -> ParseResult<Self> where Self: Sized {
        Ok(Self(Conjunct::parse_sep(lexer, Tokens::MathToken(Math::And))?))
    }

}