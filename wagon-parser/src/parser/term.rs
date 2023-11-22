use wagon_macros::TokenMapper;
use std::{fmt::Display, write};

use super::{Parse, PeekLexer, ParseResult, ParseOption, Tokens, SpannableNode, ToAst, WagNode, WagIx, WagTree};


use wagon_lexer::{math::Math, UnsafePeek};

use quote::{ToTokens, quote};

use super::helpers::TokenMapper;
use super::factor::Factor;

#[cfg(test)]
use wagon_macros::new_unspanned;

/*
Term -> Term Op Factor | Factor
|
V
Term -> Factor Term'
Term' -> Op Factor Term' | epsilon
*/

#[derive(PartialEq, Debug, Eq, Hash, Clone)]
#[cfg_attr(test, new_unspanned)]
pub struct Term {
	pub left: SpannableNode<Factor>,
	pub cont: Option<TermP>
}

#[derive(PartialEq, Debug, Eq, Hash, Clone)]
#[cfg_attr(test, new_unspanned)]
pub struct TermP {
	pub op: Op2,
	pub right: SpannableNode<Factor>,
	pub cont: Option<Box<TermP>>
}

impl Parse for Term {

	fn parse(lexer: &mut PeekLexer) -> ParseResult<Self> {
		Ok(Self {
			left: SpannableNode::parse(lexer)?,
			cont: TermP::parse_option(lexer)?
		})
	}
}

impl ParseOption for TermP {

	fn parse_option(lexer: &mut PeekLexer) -> ParseResult<Option<Self>> where Self: Sized {
	    if let Some(op) = Op2::token_to_enum(lexer.peek_unwrap()) {
	    	lexer.next();
	    	Ok(Some(TermP { op, right: SpannableNode::parse(lexer)?, cont: TermP::parse_option(lexer)?.map(|x| Box::new(x)) }))
	    } else {
	    	Ok(None)
	    }
	}
}

#[derive(TokenMapper, PartialEq, Debug, Eq, Hash, Clone)]
pub enum Op2 {
	Mul,
	Div,
	Floor,
	Mod
}

impl ToAst for Term {
    fn to_ast(self, ast: &mut WagTree) -> WagIx {
        let node = if let Some(cont) = self.cont {
        	cont.to_ast(ast)
        } else {
        	ast.add_node(WagNode::Term(None))
        };
        let child = self.left.to_ast(ast);
        ast.add_edge(node, child, ());
        node
    }
}

impl ToAst for TermP {
    fn to_ast(self, ast: &mut WagTree) -> WagIx {
        let node = ast.add_node(WagNode::Term(Some(self.op)));
        let ret = if let Some(next) = self.cont {
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

impl Display for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(c) = &self.cont {
        	write!(f, "{} {}", self.left, c)
        } else {
        	write!(f, "{}", self.left)
        }
    }
}

impl Display for TermP {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(c) = &self.cont {
        	write!(f, "{} {} {}", self.op, self.right, c)
        } else {
        	write!(f, "{} {}", self.op, self.right)
        }
    }
}

impl Display for Op2 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Op2::Mul => write!(f, "*"),
            Op2::Div => write!(f, "/"),
            Op2::Floor => write!(f, "//"),
            Op2::Mod => write!(f, "%"),
        }
    }
}

impl ToTokens for Op2 {
    fn to_tokens(&self, tokens: &mut quote::__private::TokenStream) {
        match self {
            Op2::Mul => tokens.extend(quote!(*)),
            Op2::Div => tokens.extend(quote!(/)),
            Op2::Floor => unimplemented!("Not sure how to do this yet"),
            Op2::Mod => tokens.extend(quote!(%)),
        }
    }
}
