use super::ast::ToAst;
use super::{Parse, PeekLexer, ParseResult, Tokens};
use super::atom::Atom;
use crate::lexer::{math::Math, UnsafePeek};

#[derive(PartialEq, Debug, Eq, Hash, Clone)]
pub(crate) enum Factor {
	Primary(Atom),
	Power {
		left: Atom,
		right: Box<Factor>
	}
}

impl Parse for Factor {

	fn parse(lexer: &mut PeekLexer) -> ParseResult<Self> {
		let left = Atom::parse(lexer)?;
		if &Tokens::MathToken(Math::Pow) == lexer.peek_unwrap() {
			lexer.next();
			Ok(
				Factor::Power {
					left, 
					right: Box::new(Self::parse(lexer)?)
				}
			)
		} else {
			Ok(Factor::Primary(left))
		}
	}
}

impl ToAst for Factor {
    fn to_ast(self, ast: &mut super::ast::WagTree) -> super::ast::WagIx {
        match self {
            Factor::Primary(a) => a.to_ast(ast),
            Factor::Power { left, right } => {
            	let node = ast.add_node(super::ast::WagNode::Factor);
            	let right_node = (*right).to_ast(ast);
            	let left_node = left.to_ast(ast);
            	ast.add_edge(node, right_node, ());
            	ast.add_edge(node, left_node, ());
            	node
            },
        }
    }
}