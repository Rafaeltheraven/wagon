use wagon_macros::TokenMapper;
use std::{fmt::Display, write};

use crate::firstpass::GetReqAttributes;

use super::{Parse, LexerBridge, ParseResult, ParseOption, Tokens, SpannableNode, ResultPeek};


use wagon_lexer::math::Math;

use quote::{ToTokens, quote};

use super::helpers::TokenMapper;
use super::factor::Factor;

use wagon_macros::new_unspanned;

/*
Term -> Term Op Factor | Factor
|
V
Term -> Factor Term'
Term' -> Op Factor Term' | epsilon
*/

#[derive(PartialEq, Debug, Eq, Hash, Clone)]
#[new_unspanned]
/// A multiplication/division on any number of [`Factor`]s.
///
/// If `cont == None`, then this is just a `Factor`.
///
/// # Grammar
/// <code>[Term] -> [Factor] [TermP]?;</code>
pub struct Term {
    /// The left-hand [`Factor`].
	pub left: SpannableNode<Factor>,
    /// The optional continuation.
	pub cont: Option<TermP>
}

#[derive(PartialEq, Debug, Eq, Hash, Clone)]
#[cfg_attr(test, new_unspanned)]
/// The operator, right-hand side and possible further continuation of this [`Term`].
///
/// # Grammar
/// <code>[`TermP`] -> [Op2] [Factor] [`TermP`]?;</code>
pub struct TermP {
    /// The operator
	pub op: Op2,
    /// The right-hand side of the equation.
	pub right: SpannableNode<Factor>,
    /// The optional continuation.
	pub cont: Option<Box<TermP>>
}

impl Parse for Term {

	fn parse(lexer: &mut LexerBridge) -> ParseResult<Self> {
		Ok(Self {
			left: SpannableNode::parse(lexer)?,
			cont: TermP::parse_option(lexer)?
		})
	}
}

impl ParseOption for TermP {

	fn parse_option(lexer: &mut LexerBridge) -> ParseResult<Option<Self>> where Self: Sized {
	    if let Some(op) = Op2::token_to_enum(lexer.peek_result()?) {
	    	lexer.next();
	    	Ok(Some(Self { op, right: SpannableNode::parse(lexer)?, cont: Self::parse_option(lexer)?.map(Box::new) }))
	    } else {
	    	Ok(None)
	    }
	}
}

impl GetReqAttributes for Term {
    fn get_req_attributes(&self) -> crate::firstpass::ReqAttributes {
        let mut req = self.left.get_req_attributes();
        if let Some(cont) = &self.cont {
            req.extend(cont.get_req_attributes());
        }
        req
    }
}

impl GetReqAttributes for TermP {
    fn get_req_attributes(&self) -> crate::firstpass::ReqAttributes {
        let mut req = self.right.get_req_attributes();
        if let Some(cont) = &self.cont {
            req.extend(cont.get_req_attributes());
        }
        req
    }
}

#[derive(TokenMapper, PartialEq, Debug, Eq, Hash, Clone)]
/// The [`Term`] operators
///
/// # Grammar
/// <code>Op2 -> "*" | "/" | "//" | "%";</code>
pub enum Op2 {
    /// `*`
	Mul,
    /// `/`
	Div,
    /// `//`
	Floor,
    /// `%`
	Mod
}

impl Display for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(c) = &self.cont {
        	write!(f, "{} {}", self.left, c)
        } else {
        	write!(f, "{}", self.left)
        }
    }
}

impl Display for TermP {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(c) = &self.cont {
        	write!(f, "{} {} {}", self.op, self.right, c)
        } else {
        	write!(f, "{} {}", self.op, self.right)
        }
    }
}

impl Display for Op2 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Mul => write!(f, "*"),
            Self::Div => write!(f, "/"),
            Self::Floor => write!(f, "//"),
            Self::Mod => write!(f, "%"),
        }
    }
}

impl ToTokens for Op2 {
    fn to_tokens(&self, tokens: &mut quote::__private::TokenStream) {
        match self {
            Self::Mul => tokens.extend(quote!(std::ops::Mul::mul)),
            Self::Div => tokens.extend(quote!(std::ops::Div::div)),
            Self::Floor => unimplemented!("Not sure how to do this yet"),
            Self::Mod => tokens.extend(quote!(std::ops::Rem::rem)),
        }
    }
}
