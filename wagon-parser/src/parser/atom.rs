use std::fmt::Display;
use std::hash::Hash;
use std::write;

use super::{Parse, PeekLexer, ParseResult, Tokens, WagParseError, Ident, SpannableNode, ToAst, WagNode, WagIx, WagTree, expression::Expression};
use super::helpers::{between, between_right};
use crate::either_token;
use wagon_lexer::{math::Math, Spannable, UnsafeNext};

use wagon_macros::match_error;
use ordered_float::NotNan;

#[cfg(test)]
use wagon_macros::new_unspanned;

#[derive(PartialEq, Debug, Eq, Hash, Clone)]
/// A python-style dictionary.
///
/// Is of the form [`Ident`][[`Expression`]].
pub struct Dictionary(Ident, Expression);

impl Dictionary {
	/// Deconstruct the dictionary into it's [`Ident`] and [`Expression`].
	pub fn deconstruct(&self) -> (&Ident, &Expression) {
		(&self.0, &self.1)
	}
}

#[derive(PartialEq, Debug, Eq, Hash, Clone)]
#[cfg_attr(test, new_unspanned)]
/// The base elements of each expression.
pub enum Atom {
	/// An [`Ident`].
	Ident(Ident),
	/// A [`Dictionary`].
	Dict(Dictionary),
	/// A [`bool`].
	LitBool(bool),
	/// An [`i32`].
	LitNum(i32),
	/// An [`f32`].
	LitFloat(NotNan<f32>),
	/// A [`String`].
	LitString(String),
	/// Another full [`Expression`]. Enclosed by `()`.
	Group(SpannableNode<Expression>)
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
    fn to_ast(self, ast: &mut WagTree) -> WagIx {
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

impl Display for Atom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Atom::Ident(x) => write!(f, "{}", x),
            Atom::Dict(x) => write!(f, "{}", x),
            Atom::LitBool(x) => write!(f, "{}", x),
            Atom::LitNum(x) => write!(f, "{}", x),
            Atom::LitFloat(x) => write!(f, "{}", x),
            Atom::LitString(x) => write!(f, "\"{}\"", x),
            Atom::Group(x) => write!(f, "({})", x),
        }
    }
}

impl Display for Dictionary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}[{}]", self.0, self.1)
    }
}
