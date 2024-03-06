use std::{fmt::Display, write};

use crate::firstpass::{GetReqAttributes, ReqAttributes};

use super::{Parse, LexerBridge, ParseResult, Tokens, SpannableNode};
use wagon_lexer::math::Math;
use super::conjunct::Conjunct;

use wagon_macros::new_unspanned;

#[derive(PartialEq, Debug, Eq, Hash, Clone)]
#[new_unspanned]
/// A list of [`Conjunct`] nodes, seperated by `&&`.
///
/// # Grammar
/// `Disjunct -> [Conjunct] ("&&" Disjunct)?;`
pub struct Disjunct(pub Vec<SpannableNode<Conjunct>>);

impl GetReqAttributes for Disjunct {
    fn get_req_attributes(&self) -> ReqAttributes {
        let mut req = ReqAttributes::new();
        for c in &self.0 {
            req.extend(c.get_req_attributes());
        }
        req
    }
}

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