use std::hash::{Hash};

use super::ast::{ToAst, WagNode};
use super::{Parse, PeekLexer, ParseResult, Tokens, WagParseError, Ident, expression::Expression};
use super::helpers::{between, between_right};
use crate::either_token;
use crate::lexer::{math::Math, productions::Productions, Spannable, UnsafeNext};

use wagon_macros::match_error;
use ordered_float::NotNan;


#[derive(PartialEq, Debug, Eq, Hash, Clone)]
pub(crate) struct Dictionary(Ident, Expression);

#[derive(PartialEq, Debug, Eq, Hash, Clone)]
pub(crate) enum Atom {
	Ident(Ident),
	Dict(Dictionary),
	LitBool(bool),
	LitNum(i32),
	LitFloat(NotNan<f32>),
	LitString(String),
	Group(Expression)
}

#[derive(PartialEq, Debug)]
pub(crate) enum AtomNode {
	Ident(Ident),
	Dict(Ident),
	LitBool(bool),
	LitNum(i32),
	LitFloat(NotNan<f32>),
	LitString(String),
	Group
}

impl Parse for Atom {

	fn parse(lexer: &mut PeekLexer) -> ParseResult<Self> {
	    match_error!(match lexer.next_unwrap() {
	    	#[expect("identifier or dictionary")]
	        either_token!(Identifier(x)) => {
	        	if let Ok(inner) = between(lexer, Tokens::MathToken(Math::LBr), Tokens::MathToken(Math::RBr)) {
	        		Ok(Self::Dict(Dictionary(x, inner)))
	        	} else {
	        		Ok(Self::Ident(x))
	        	}
	        },
	        Tokens::MathToken(Math::LitBool(x)) => Ok(Self::LitBool(x)),
	        Tokens::MathToken(Math::LitInt(x)) => Ok(Self::LitNum(x)),
	        Tokens::MathToken(Math::LitFloat(x)) => Ok(Self::LitFloat(NotNan::new(x).unwrap())),
	        #[expect("string")]
	        either_token!(LitString(x)) => Ok(Self::LitString(x)),
	        Tokens::MathToken(Math::LPar) => {
	        	let resp = between_right(lexer, Tokens::MathToken(Math::RPar))?;
	        	Ok(Self::Group(resp))
	        },
	    })
	}
}

impl ToAst for Atom {
    fn to_ast(self, ast: &mut super::ast::WagTree) -> super::ast::WagIx {
        match self {
            Atom::Ident(x) => ast.add_node(WagNode::Atom(AtomNode::Ident(x))),
            Atom::LitBool(x) => ast.add_node(WagNode::Atom(AtomNode::LitBool(x))),
            Atom::LitNum(x) => ast.add_node(WagNode::Atom(AtomNode::LitNum(x))),
            Atom::LitFloat(x) => ast.add_node(WagNode::Atom(AtomNode::LitFloat(x))),
            Atom::LitString(x) => ast.add_node(WagNode::Atom(AtomNode::LitString(x))),
            Atom::Dict(Dictionary(i, e)) => {
            	let node = ast.add_node(WagNode::Atom(AtomNode::Dict(i)));
            	let child = e.to_ast(ast);
            	ast.add_edge(node, child, ());
            	node
            },
            Atom::Group(g) => {
            	let node = ast.add_node(WagNode::Atom(AtomNode::Group));
            	let child = g.to_ast(ast);
            	ast.add_edge(node, child, ());
            	node
            },
        }
    }
}
