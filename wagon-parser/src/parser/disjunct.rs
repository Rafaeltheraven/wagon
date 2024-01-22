use std::{fmt::Display, write};

use super::{Parse, LexerBridge, ParseResult, Tokens, SpannableNode};
use wagon_lexer::math::Math;
use super::conjunct::Conjunct;

#[cfg(test)]
use wagon_macros::new_unspanned;

#[derive(PartialEq, Debug, Eq, Hash, Clone)]
#[cfg_attr(test, new_unspanned)]
/// A list of [`Conjunct`] nodes, seperated by `&&`.
pub struct Disjunct(pub Vec<SpannableNode<Conjunct>>);

impl Parse for Disjunct {
    
    fn parse(lexer: &mut LexerBridge) -> ParseResult<Self> where Self: Sized {
        Ok(Self(SpannableNode::parse_sep(lexer, Tokens::MathToken(Math::And))?))
    }

}

impl Display for Disjunct {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.iter().map(std::string::ToString::to_string).collect::<Vec<_>>().join(" and "))
    }
}