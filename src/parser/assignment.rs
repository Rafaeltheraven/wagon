use crate::lexer::UnsafeNext;
use super::{Parse, PeekLexer, ParseResult, Tokens, WagParseError, ast::ToAst};
use crate::lexer::{math::Math, ident::Ident, Spannable};

use wagon_macros::match_error;
use super::expression::Expression;

#[derive(PartialEq, Debug, Eq, Hash)]
pub(crate) struct Assignment {
	pub(crate) ident: Ident,
	pub(crate) expr: Expression
}

impl Parse for Assignment {
	fn parse(lexer: &mut PeekLexer) -> ParseResult<Self> {
		let ident = match_error!(match lexer.next_unwrap() {
			Tokens::MathToken(Math::Identifier(x)) => Ok(x),
		})?;
		let next = lexer.next_unwrap();
		if next != Tokens::MathToken(Math::Assigns) {
			Err(WagParseError::Unexpected { span: lexer.span(), offender: next, expected: vec![Tokens::MathToken(Math::Assigns).to_string()] })
		} else {
			Ok(Self {ident, expr: Expression::parse(lexer)?})
		}
	}
}

impl ToAst for Assignment {
    fn to_ast(self, ast: &mut super::ast::WagTree) -> super::ast::WagIx {
        let node = ast.add_node(super::ast::WagNode::Ident(self.ident));
        let child = self.expr.to_ast(ast);
        ast.add_edge(node, child, ());
        node
    }
}