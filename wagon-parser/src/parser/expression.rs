use std::{fmt::Display, write};

use crate::firstpass::{GetReqAttributes, ReqAttributes};

use super::{Parse, LexerBridge, ParseResult, Tokens, Spannable, WagParseError, SpannableNode, ResultPeek, ResultNext};
use wagon_lexer::math::Math;

use wagon_macros::match_error;
use super::disjunct::Disjunct;

use wagon_macros::new_unspanned;

#[derive(PartialEq, Debug, Eq, Hash, Clone)]
#[new_unspanned]
/// An expression in the WAGon attribute evaluation DSL.
///
/// # Grammar
/// <span><pre>
/// [Expression] -> SubProc | If | [Disjunct];
/// SubProc -> `"$(" /[^)]*/ ")"`; // A bash-style $() expression
/// If -> "if" [Disjunct] "then" [Disjunct] ("else" [Expression])?;
/// </pre></span>
pub enum Expression {
	/// A subprocess that should do evaluation in the shell.
	Subproc(SpannableNode<String>),
	/// An if(-else) statement.
	If {
		/// If this evaluation returns true.
		this: SpannableNode<Disjunct>,
		/// Do this.
		then: SpannableNode<Disjunct>,
		/// Else, evaluate this expression.
		r#else: Option<Box<SpannableNode<Expression>>>
	},
	/// The next layer down. See [`Disjunct`].
	Disjunct(SpannableNode<Disjunct>),
}

impl Parse for Expression {

	fn parse(lexer: &mut LexerBridge) -> ParseResult<Self> { 
		match lexer.peek_result()? {
			Tokens::MathToken(Math::If) => {lexer.next(); Self::parse_if(lexer)},
			Tokens::MathToken(Math::Bash(_)) => Ok(Self::Subproc(SpannableNode::parse(lexer)?)),
			_ => Ok(Self::Disjunct(SpannableNode::parse(lexer)?))
		}
	}
}

impl Expression {

	fn parse_if(lexer: &mut LexerBridge) -> ParseResult<Self> {
		let this = SpannableNode::parse(lexer)?;
		let then = match_error!(match lexer.next_result()? {
			Tokens::MathToken(Math::Then) => SpannableNode::parse(lexer)
		})?;
		let r#else = match lexer.peek_result()? {
		    Tokens::MathToken(Math::Else) => {lexer.next(); Some(Box::new(SpannableNode::parse(lexer)?))},
		    _ => None
		};
		Ok(Self::If { this, then, r#else })
	}
}

impl GetReqAttributes for Expression {
    fn get_req_attributes(&self) -> ReqAttributes {
        match self {
		    Self::Subproc(_) => ReqAttributes::new(),
		    Self::If { this, then, r#else } => {
		    	let mut req = this.get_req_attributes();
		    	req.extend(then.get_req_attributes());
		    	if let Some(cont) = r#else {
		    		req.extend(cont.get_req_attributes());
		    	}
		    	req
		    },
		    Self::Disjunct(d) => d.get_req_attributes(),
		}
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Subproc(s) => write!(f, "$({s})"),
            Self::If { this, then, r#else } => {
            	if let Some(e) = r#else {
            		write!(f, "if {this} {{ {then} }} else {{ {e} }}")
            	} else {
            		write!(f, "if {this} {{ {then} }}")
            	}
            },
            Self::Disjunct(d) => write!(f, "{d}"),
        }
    }
}