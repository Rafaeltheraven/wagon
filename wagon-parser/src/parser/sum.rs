use std::fmt::Display;
use std::write;

use super::{Parse, LexerBridge, ParseResult, ParseOption, Tokens, SpannableNode, ResultPeek};

use wagon_lexer::math::Math;

use super::term::Term;
use super::helpers::TokenMapper;
use wagon_macros::TokenMapper;
use quote::{ToTokens, quote};

#[cfg(test)]
use wagon_macros::new_unspanned;

#[derive(PartialEq, Debug, Eq, Hash, Clone)]
#[cfg_attr(test, new_unspanned)]
/// A sum of any number of [`Term`]s.
///
/// If `cont == None`, then this is just a `Term`.
pub struct Sum {
    /// The left-hand [`Term`].
	pub left: SpannableNode<Term>,
    /// The optional continuation.
	pub cont: Option<SumP>
}

#[derive(PartialEq, Debug, Eq, Hash, Clone)]
#[cfg_attr(test, new_unspanned)]
/// The operator, right-hand side and possible further continuation of this sum.
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

#[derive(TokenMapper, PartialEq, Debug, Eq, Hash, Clone)]
/// The sum operations
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
            Self::Add => tokens.extend(quote!(+)),
            Self::Sub => tokens.extend(quote!(-)),
        }
    }
}
