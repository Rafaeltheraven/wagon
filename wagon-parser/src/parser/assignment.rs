use std::{fmt::Display, write};

use super::{Parse, LexerBridge, ParseResult, Tokens, WagParseError, Ident, ToAst, SpannableNode, ResultNext};
use wagon_lexer::{math::Math, Spannable};

use super::expression::Expression;

#[cfg(test)]
use wagon_macros::new_unspanned;

#[derive(PartialEq, Debug, Eq, Hash, Clone)]
#[cfg_attr(test, new_unspanned)]
/// Assigns the result of an expression to an attribute.
///
/// Each assignment has the following structure: [`Ident`] = [`Expression`];
pub struct Assignment {
	/// The left-hand side.
	pub ident: SpannableNode<Ident>,
	/// The right-hand side.
	pub expr: SpannableNode<Expression>
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

impl ToAst for Assignment {
    fn to_ast(self, ast: &mut crate::ast::WagTree) -> crate::ast::WagIx {
        let node = ast.add_node(crate::ast::WagNode::Ident(self.ident.into_inner()));
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
