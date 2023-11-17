use std::{fmt::Display, write};

use crate::lexer::UnsafeNext;
use super::{Parse, PeekLexer, ParseResult, Tokens, WagParseError, Ident, ast::ToAst, SpannableNode};
use crate::lexer::{math::Math, Spannable};

use super::expression::Expression;

#[cfg(test)]
use wagon_macros::new_unspanned;

#[derive(PartialEq, Debug, Eq, Hash, Clone)]
#[cfg_attr(test, new_unspanned)]
pub(crate) struct Assignment {
	pub(crate) ident: SpannableNode<Ident>,
	pub(crate) expr: SpannableNode<Expression>
}

impl Parse for Assignment {
	fn parse(lexer: &mut PeekLexer) -> ParseResult<Self> {
		let ident = SpannableNode::parse(lexer)?;
		let next = lexer.next_unwrap();
		if next != Tokens::MathToken(Math::Assigns) {
			Err(WagParseError::Unexpected { span: lexer.span(), offender: next, expected: vec![Tokens::MathToken(Math::Assigns).to_string()] })
		} else {
			Ok(Self {ident, expr: SpannableNode::parse(lexer)?})
		}
	}
}

impl ToAst for Assignment {
    fn to_ast(self, ast: &mut super::ast::WagTree) -> super::ast::WagIx {
        let node = ast.add_node(super::ast::WagNode::Ident(self.ident.into_inner()));
        let child = self.expr.to_ast(ast);
        ast.add_edge(node, child, ());
        node
    }
}

impl Display for Assignment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} = {}", self.ident, self.expr)
    }
}
