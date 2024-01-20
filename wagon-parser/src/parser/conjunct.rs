use std::fmt::Display;
use super::{Parse, LexerBridge, ParseResult, Tokens, SpannableNode, ToAst, WagNode, WagIx, WagTree};
use wagon_lexer::math::Math;

use super::inverse::Inverse;

#[cfg(test)]
use wagon_macros::new_unspanned;

#[derive(PartialEq, Debug, Eq, Hash, Clone)]
#[cfg_attr(test, new_unspanned)]
/// A list of [`Inverse`] nodes. Separated by `||`.
pub struct Conjunct(pub Vec<SpannableNode<Inverse>>);

impl Parse for Conjunct {
    
    fn parse(lexer: &mut LexerBridge) -> ParseResult<Self> where Self: Sized {
        Ok(Self(SpannableNode::parse_sep(lexer, Tokens::MathToken(Math::Or))?))
    }

}

impl ToAst for Conjunct {
    fn to_ast(self, ast: &mut WagTree) -> WagIx {
        let node = WagNode::Conjunct;
        Self::add_vec_children(node, self.0, ast)
    }
}

impl Display for Conjunct {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.iter().map(std::string::ToString::to_string).collect::<Vec<_>>().join(" or "))
    }
}