use std::{fmt::Display, write};

use crate::{firstpass::{GetReqAttributes, ReqAttributes, RewriteToSynth}, WrapSpannable};

use super::{Parse, LexerBridge, ParseResult, Tokens, WagParseError, Ident, SpannableNode, Spannable, ResultNext};
use wagon_lexer::math::Math;

use super::expression::Expression;

use wagon_macros::new_unspanned;

#[derive(PartialEq, Debug, Eq, Hash, Clone)]
#[new_unspanned]
/// Assigns the result of an expression to an attribute.
///
/// # Grammar
/// <code>[Assignment] -> "{" ([Ident] "=" [Expression] ";")* "}";</code>
pub struct Assignment {
	/// The left-hand side.
	pub ident: SpannableNode<Ident>,
	/// The right-hand side.
	pub expr: SpannableNode<Expression>
}

impl RewriteToSynth for Assignment {
	fn rewrite_to_synth(&mut self) -> ReqAttributes {
		let span = self.ident.span();
		self.ident = self.ident.to_inner().to_synth().into_spanned(span);
		let mut req = self.expr.rewrite_to_synth();
		req.insert(self.ident.clone());
		req
	}
}

impl GetReqAttributes for Assignment {
	fn get_req_attributes(&self) -> ReqAttributes {
		let mut req = self.expr.get_req_attributes();
		req.insert(self.ident.clone());
		req
	}
}

impl Parse for Assignment {
	fn parse(lexer: &mut LexerBridge) -> ParseResult<Self> {
		let ident = SpannableNode::parse(lexer)?;
		let next = lexer.next_result()?;
		if next == Tokens::MathToken(Math::Assigns) {
			Ok(Self {ident, expr: SpannableNode::parse(lexer)?})
		} else {
			Err(WagParseError::Unexpected { span: lexer.span(), offender: next, expected: vec![Tokens::MathToken(Math::Assigns).to_string()] })
		}
	}
}

impl Display for Assignment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} = {}", self.ident, self.expr)
    }
}
