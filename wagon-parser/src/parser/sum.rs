use std::fmt::Display;
use std::write;

use crate::firstpass::GetReqAttributes;

use super::{Parse, LexerBridge, ParseResult, ParseOption, Tokens, SpannableNode, ResultPeek};

use wagon_lexer::math::Math;

use super::term::Term;
use super::helpers::TokenMapper;
use wagon_macros::TokenMapper;
use quote::{ToTokens, quote};

use wagon_macros::new_unspanned;

#[derive(PartialEq, Debug, Eq, Hash, Clone)]
#[new_unspanned]
/// A sum of any number of [`Term`]s.
///
/// If `cont == None`, then this is just a `Term`.
///
/// # Grammar
/// <code>[Sum] -> [Term] [SumP]?;</code>
pub struct Sum {
    /// The left-hand [`Term`].
	pub left: SpannableNode<Term>,
    /// The optional continuation.
	pub cont: Option<SumP>
}

#[derive(PartialEq, Debug, Eq, Hash, Clone)]
#[new_unspanned]
/// The operator, right-hand side and possible further continuation of this sum.
///
/// # Grammar
/// <code>[SumP] -> [Op1] [Term] [SumP]?;</code>
pub struct SumP {
    /// The operator
	pub op: Op1,
    /// The right-hand [`Term`].
	pub right: SpannableNode<Term>,
    /// The optional rest of the sum
	pub cont: Option<Box<SumP>>
}

impl Parse for Sum {

	fn parse(lexer: &mut LexerBridge) -> ParseResult<Self> {
		Ok(Self {
			left: SpannableNode::parse(lexer)?,
			cont: SumP::parse_option(lexer)?
		})
	}
}

impl ParseOption for SumP {

	fn parse_option(lexer: &mut LexerBridge) -> ParseResult<Option<Self>> where Self: Sized {
	    if let Some(op) = Op1::token_to_enum(lexer.peek_result()?) {
	    	lexer.next();
	    	Ok(Some(Self { op, right: SpannableNode::parse(lexer)?, cont: Self::parse_option(lexer)?.map(Box::new) }))
	    } else {
	    	Ok(None)
	    }
	}
}

impl GetReqAttributes for Sum {
    fn get_req_attributes(&self) -> crate::firstpass::ReqAttributes {
        let mut req = self.left.get_req_attributes();
        if let Some(cont) = &self.cont {
            req.extend(cont.get_req_attributes());
        }
        req
    }
}

impl GetReqAttributes for SumP {
    fn get_req_attributes(&self) -> crate::firstpass::ReqAttributes {
        let mut req = self.right.get_req_attributes();
        if let Some(cont) = &self.cont {
            req.extend(cont.get_req_attributes());
        }
        req
    }
}

#[derive(TokenMapper, PartialEq, Debug, Eq, Hash, Clone)]
/// The sum operations
///
/// # Grammar
/// <code>[Op1] -> "+" | "-";</code>
pub enum Op1 {
    /// `+`
	Add,
    /// `-`
	Sub
}

impl Display for Sum {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(c) = &self.cont {
        	write!(f, "{} {}", self.left, c)
        } else {
        	write!(f, "{}", self.left)
        }
    }
}

impl Display for SumP {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(c) = &self.cont {
        	write!(f, "{} {} {}", self.op, self.right, c)
        } else {
        	write!(f, "{} {}", self.op, self.right)
        }
    }
}

impl Display for Op1 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Add => write!(f, "+"),
            Self::Sub => write!(f, "-"),
        }
    }
}

impl ToTokens for Op1 {
    fn to_tokens(&self, tokens: &mut quote::__private::TokenStream) {
        match self {
            Self::Add => tokens.extend(quote!(std::ops::Add::add)),
            Self::Sub => tokens.extend(quote!(std::ops::Sub::sub)),
        }
    }
}
