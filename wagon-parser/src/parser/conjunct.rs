use std::fmt::Display;
use crate::firstpass::{GetReqAttributes, ReqAttributes};

use super::{Parse, LexerBridge, ParseResult, Tokens, SpannableNode};
use wagon_lexer::math::Math;

use super::inverse::Inverse;

use wagon_macros::new_unspanned;

#[derive(PartialEq, Debug, Eq, Hash, Clone)]
#[new_unspanned]
/// A list of [`Inverse`] nodes. Separated by `||`.
///
/// # Grammar
/// `Conjunct -> [Inverse] ("||" Conjunct)?;`
pub struct Conjunct(pub Vec<SpannableNode<Inverse>>);

impl GetReqAttributes for Conjunct {
    fn get_req_attributes(&self) -> ReqAttributes {
        let mut req = ReqAttributes::new();
        for i in &self.0 {
            req.extend(i.get_req_attributes());
        }
        req
    }
}

impl Parse for Conjunct {
    
    fn parse(lexer: &mut LexerBridge) -> ParseResult<Self> where Self: Sized {
        Ok(Self(SpannableNode::parse_sep(lexer, Tokens::MathToken(Math::Or))?))
    }

}

impl Display for Conjunct {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.iter().map(std::string::ToString::to_string).collect::<Vec<_>>().join(" or "))
    }
}