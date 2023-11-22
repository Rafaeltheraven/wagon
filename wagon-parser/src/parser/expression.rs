use std::{fmt::Display, write};

use super::{Parse, PeekLexer, ParseResult, Tokens, Spannable, WagParseError, ToAst, WagNode, WagIx, WagTree, SpannableNode};
use wagon_lexer::{math::Math, UnsafeNext, UnsafePeek};

use wagon_macros::match_error;
use super::disjunct::Disjunct;

#[cfg(test)]
use wagon_macros::new_unspanned;

#[derive(PartialEq, Debug, Eq, Hash, Clone)]
#[cfg_attr(test, new_unspanned)]
pub enum Expression {
	Subproc(SpannableNode<String>),
	If {
		this: SpannableNode<Disjunct>,
		then: SpannableNode<Disjunct>,
		r#else: Option<Box<SpannableNode<Expression>>>
	},
	Disjunct(SpannableNode<Disjunct>),
}

impl Parse for Expression {

	fn parse(lexer: &mut PeekLexer) -> ParseResult<Self> { 
		match lexer.peek_unwrap() {
			Tokens::MathToken(Math::If) => {lexer.next(); Self::parse_if(lexer)},
			Tokens::MathToken(Math::Bash(_)) => Ok(Self::Subproc(SpannableNode::parse(lexer)?)),
			_ => Ok(Self::Disjunct(SpannableNode::parse(lexer)?))
		}
	}
}

impl Expression {

	fn parse_if(lexer: &mut PeekLexer) -> ParseResult<Self> {
		let this = SpannableNode::parse(lexer)?;
		let then = match_error!(match lexer.next_unwrap() {
			Tokens::MathToken(Math::Then) => SpannableNode::parse(lexer)
		})?;
		let r#else = match lexer.peek_unwrap() {
		    Tokens::MathToken(Math::Else) => {lexer.next(); Some(Box::new(SpannableNode::parse(lexer)?))},
		    _ => None
		};
		Ok(Self::If { this, then, r#else })
	}
}

impl ToAst for Expression {
    fn to_ast(self, ast: &mut WagTree) -> WagIx {
        match self {
            Expression::Subproc(s) => ast.add_node(WagNode::SubProc(s.into_inner())),
            Expression::If { this, then, r#else } => {
            	let node = ast.add_node(WagNode::If);
            	if let Some(expr) = r#else {
            		let child = (*expr).to_ast(ast);
            		ast.add_edge(node, child, ());
            	}
            	let then_node = then.to_ast(ast);
            	let this_node = this.to_ast(ast);
            	ast.add_edge(node, then_node, ());
            	ast.add_edge(node, this_node, ());
            	node
            },
            Expression::Disjunct(d) => {
            	let node = ast.add_node(WagNode::Disjunct);
            	let child = d.to_ast(ast);
            	ast.add_edge(node, child, ());
            	node
            },
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Subproc(s) => write!(f, "$({})", s),
            Expression::If { this, then, r#else } => {
            	if let Some(e) = r#else {
            		write!(f, "if {} {{ {} }} else {{ {} }}", this, then, e)
            	} else {
            		write!(f, "if {} {{ {} }}", this, then)
            	}
            },
            Expression::Disjunct(d) => write!(f, "{}", d),
        }
    }
}