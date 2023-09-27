use crate::parser::ast::ToAst;
use super::{Parse, PeekLexer, ParseResult, ParseOption, Tokens};


use crate::lexer::{math::Math, UnsafePeek};

use super::helpers::TokenMapper;
use wagon_macros::TokenMapper;
use super::factor::Factor;

/*
Term -> Term Op Factor | Factor
|
V
Term -> Factor Term'
Term' -> Op Factor Term' | epsilon
*/

#[derive(PartialEq, Debug, Eq, Hash, Clone)]
pub(crate) struct Term {
	pub(crate) left: Factor,
	pub(crate) cont: Option<TermP>
}

#[derive(PartialEq, Debug, Eq, Hash, Clone)]
pub(crate) struct TermP {
	pub(crate) op: Op2,
	pub(crate) right: Factor,
	pub(crate) cont: Box<Option<TermP>>
}

impl Parse for Term {

	fn parse(lexer: &mut PeekLexer) -> ParseResult<Self> {
		Ok(Self {
			left: Factor::parse(lexer)?,
			cont: TermP::parse_option(lexer)?
		})
	}
}

impl ParseOption for TermP {

	fn parse_option(lexer: &mut PeekLexer) -> ParseResult<Option<Self>> where Self: Sized {
	    if let Some(op) = Op2::token_to_enum(lexer.peek_unwrap()) {
	    	lexer.next();
	    	Ok(Some(TermP { op, right: Factor::parse(lexer)?, cont: Box::new(TermP::parse_option(lexer)?) }))
	    } else {
	    	Ok(None)
	    }
	}
}

#[derive(TokenMapper, PartialEq, Debug, Eq, Hash, Clone)]
pub(crate) enum Op2 {
	Mul,
	Div,
	Floor,
	Mod
}

impl ToAst for Term {
    fn to_ast(self, ast: &mut super::ast::WagTree) -> super::ast::WagIx {
        let node = if let Some(cont) = self.cont {
        	cont.to_ast(ast)
        } else {
        	ast.add_node(super::ast::WagNode::Term(None))
        };
        let child = self.left.to_ast(ast);
        ast.add_edge(node, child, ());
        node
    }
}

impl ToAst for TermP {
    fn to_ast(self, ast: &mut super::ast::WagTree) -> super::ast::WagIx {
        let node = ast.add_node(super::ast::WagNode::Term(Some(self.op)));
        let ret = if let Some(next) = *self.cont {
        	let parent = next.to_ast(ast);
        	ast.add_edge(parent, node, ());
        	parent
        } else {
        	node
        };
        let child = self.right.to_ast(ast);
        ast.add_edge(ret, child, ());
        ret
    }
}