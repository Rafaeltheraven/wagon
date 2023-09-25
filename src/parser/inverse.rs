use super::{Parse, PeekLexer, ParseResult, Tokens, ast::{ToAst, WagNode}};

use crate::lexer::{math::Math};

use super::comp::Comparison;

#[derive(PartialEq, Debug, Eq, Hash)]
pub(crate) enum Inverse {
	Not(Box<Inverse>),
	Comparison(Comparison)
}

pub(crate) enum InverseNode {
    Not,
    Comparison
}

impl Parse for Inverse {

    fn parse(lexer: &mut PeekLexer) -> ParseResult<Self> where Self: Sized {
        if lexer.next_if_eq(&Ok(Tokens::MathToken(Math::Not))).is_some() {
            Ok(Self::Not(Box::new(Self::parse(lexer)?)))
        } else {
            Ok(Self::Comparison(Comparison::parse(lexer)?))
        }
    }

}

impl ToAst for Inverse {
    fn to_ast(self, ast: &mut super::ast::WagTree) -> super::ast::WagIx {
        match self {
            Inverse::Not(b) => {
                let node = ast.add_node(WagNode::Inverse(InverseNode::Not));
                let child = (*b).to_ast(ast);
                ast.add_edge(node, child, ());
                node
            },
            Inverse::Comparison(c) => {
                let node = ast.add_node(WagNode::Inverse(InverseNode::Comparison));
                let child = c.to_ast(ast);
                ast.add_edge(node, child, ());
                node
            },
        }
    }
}