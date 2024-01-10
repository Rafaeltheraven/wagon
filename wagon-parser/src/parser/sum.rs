use std::fmt::Display;
use std::write;

use super::{Parse, PeekLexer, ParseResult, ParseOption, Tokens, SpannableNode, ToAst, WagNode, WagIx, WagTree};

use wagon_lexer::{math::Math, UnsafePeek};

use super::term::Term;
use super::helpers::TokenMapper;
use wagon_macros::TokenMapper;
use quote::{ToTokens, quote};

#[cfg(test)]
use wagon_macros::new_unspanned;

#[derive(PartialEq, Debug, Eq, Hash, Clone)]
#[cfg_attr(test, new_unspanned)]
/// A sum of any number of [`Term`]s.
///
/// If `cont == None`, then this is just a `Term`.
pub struct Sum {
    /// The left-hand [`Term`].
	pub left: SpannableNode<Term>,
    /// The optional continuation.
	pub cont: Option<SumP>
}

#[derive(PartialEq, Debug, Eq, Hash, Clone)]
#[cfg_attr(test, new_unspanned)]
/// The operator, right-hand side and possible further continuation of this sum.
pub struct SumP {
    /// The operator
	pub op: Op1,
    /// The right-hand [`Term`].
	pub right: SpannableNode<Term>,
    /// The optional rest of the sum
	pub cont: Option<Box<SumP>>
}

impl Parse for Sum {

	fn parse(lexer: &mut PeekLexer) -> ParseResult<Self> {
		Ok(Self {
			left: SpannableNode::parse(lexer)?,
			cont: SumP::parse_option(lexer)?
		})
	}
}

impl ParseOption for SumP {

	fn parse_option(lexer: &mut PeekLexer) -> ParseResult<Option<Self>> where Self: Sized {
	    if let Some(op) = Op1::token_to_enum(lexer.peek_unwrap()) {
	    	lexer.next();
	    	Ok(Some(SumP { op, right: SpannableNode::parse(lexer)?, cont: SumP::parse_option(lexer)?.map(Box::new) }))
	    } else {
	    	Ok(None)
	    }
	}
}

#[derive(TokenMapper, PartialEq, Debug, Eq, Hash, Clone)]
/// The sum operations
pub enum Op1 {
    /// `+`
	Add,
    /// `-`
	Sub
}

impl ToAst for Sum {
    fn to_ast(self, ast: &mut WagTree) -> WagIx {
        let node = if let Some(cont) = self.cont {
        	cont.to_ast(ast)
        } else {
        	ast.add_node(WagNode::Sum(None))
        };
        let child =  self.left.to_ast(ast);
        ast.add_edge(node, child, ());
        node
    }
}

impl ToAst for SumP {
    fn to_ast(self, ast: &mut WagTree) -> WagIx {
        let node = ast.add_node(WagNode::Sum(Some(self.op)));
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

impl Display for Sum {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(c) = &self.cont {
        	write!(f, "{} {}", self.left, c)
        } else {
        	write!(f, "{}", self.left)
        }
    }
}

impl Display for SumP {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(c) = &self.cont {
        	write!(f, "{} {} {}", self.op, self.right, c)
        } else {
        	write!(f, "{} {}", self.op, self.right)
        }
    }
}

impl Display for Op1 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Op1::Add => write!(f, "+"),
            Op1::Sub => write!(f, "-"),
        }
    }
}

impl ToTokens for Op1 {
    fn to_tokens(&self, tokens: &mut quote::__private::TokenStream) {
        match self {
            Op1::Add => tokens.extend(quote!(+)),
            Op1::Sub => tokens.extend(quote!(-)),
        }
    }
}
