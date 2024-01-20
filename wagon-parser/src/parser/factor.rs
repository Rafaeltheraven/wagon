use std::fmt::Display;
use std::write;
use super::{Parse, LexerBridge, ParseResult, Tokens, SpannableNode, ToAst, WagNode, WagIx, WagTree, ResultPeek};
use super::atom::Atom;
use wagon_lexer::math::Math;

#[cfg(test)]
use wagon_macros::new_unspanned;

#[derive(PartialEq, Debug, Eq, Hash, Clone)]
#[cfg_attr(test, new_unspanned)]
/// A possible power equation, or just an [`Atom`].
pub enum Factor {
	/// Just an [`Atom`].
	Primary(SpannableNode<Atom>),
	/// A power equation
	Power {
		/// The left-hand side.
		left: SpannableNode<Atom>,
		/// Whatever to evaluate to the power to.
		right: Box<SpannableNode<Factor>>
	}
}

impl Parse for Factor {

	fn parse(lexer: &mut LexerBridge) -> ParseResult<Self> {
		let left = SpannableNode::parse(lexer)?;
		if &Tokens::MathToken(Math::Pow) == lexer.peek_result()? {
			lexer.next();
			Ok(
				Self::Power {
					left, 
					right: Box::new(SpannableNode::parse(lexer)?)
				}
			)
		} else {
			Ok(Self::Primary(left))
		}
	}
}

impl ToAst for Factor {
    fn to_ast(self, ast: &mut WagTree) -> WagIx {
        match self {
            Self::Primary(a) => a.to_ast(ast),
            Self::Power { left, right } => {
            	let node = ast.add_node(WagNode::Factor);
            	let right_node = (*right).to_ast(ast);
            	let left_node = left.to_ast(ast);
            	ast.add_edge(node, right_node, ());
            	ast.add_edge(node, left_node, ());
            	node
            },
        }
    }
}

impl Display for Factor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Primary(p) => write!(f, "{p}"),
            Self::Power { left, right } => write!(f, "{left}^{right}"),
        }
    }
}