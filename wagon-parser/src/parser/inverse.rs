use std::{fmt::Display, write};

use crate::firstpass::GetReqAttributes;

use super::{Parse, LexerBridge, ParseResult, Tokens, SpannableNode, Peek};

use wagon_lexer::math::Math;

use super::comp::Comparison;

use wagon_macros::new_unspanned;

#[derive(PartialEq, Debug, Eq, Hash, Clone)]
#[new_unspanned]
/// Either another `Inverse`, prepend by `!` or just a `[Comparison]`.
pub enum Inverse {
    /// `!`
	Not(Box<SpannableNode<Inverse>>),
    /// The next layer down.
	Comparison(SpannableNode<Comparison>)
}

impl Parse for Inverse {

    fn parse(lexer: &mut LexerBridge) -> ParseResult<Self> where Self: Sized {
        if lexer.next_if_eq(&Ok(Tokens::MathToken(Math::Not))).is_some() {
            Ok(Self::Not(Box::new(SpannableNode::parse(lexer)?)))
        } else {
            Ok(Self::Comparison(SpannableNode::parse(lexer)?))
        }
    }

}

impl GetReqAttributes for Inverse {
    fn get_req_attributes(&self) -> crate::firstpass::ReqAttributes {
        match self {
            Self::Not(i) => i.get_req_attributes(),
            Self::Comparison(c) => c.get_req_attributes(),
        }
    }
}

impl Display for Inverse {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Not(i) => write!(f, "not {i}"),
            Self::Comparison(c) => write!(f, "{c}"),
        }
    }
}