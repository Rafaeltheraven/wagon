use crate::parser::Span;
use std::fmt::Display;
use std::matches;

use super::SpannableNode;
use super::{Parse, LexerBridge, ParseResult, Tokens, WagParseError, chunk::Chunk, expression::Expression, Peek, Spannable};
use super::helpers::between;

use wagon_lexer::{productions::Productions, math::Math};

use wagon_macros::new_unspanned;

#[derive(PartialEq, Debug, Eq, Hash)]
#[new_unspanned]
/// A right-hand side (AKA alternative) of a rule.
///
/// Any `Rhs` optionally has an expression that evaluates it's weight, enclosed by `[]`.
/// 
/// After the weight, it has a list of chunks (which may be empty)
///
/// # Grammar
/// <span><pre>
/// [Rhs] -> Weight? [Chunk]* `"|"` [Rhs]
///	    |  Weight? [Chunk]* `";"`
///	    ;
/// Weight -> "[" [Expression] "]";
/// </pre></span>
pub struct Rhs {
	/// The weight expression of this alternative.
	pub weight: Option<SpannableNode<Expression>>,
	/// The chunks of this alternative.
	pub chunks: Vec<SpannableNode<Chunk>>,
}

impl Parse for Rhs {
	fn parse(lexer: &mut LexerBridge) -> ParseResult<Self> {
		Ok(Self {weight: Self::parse_weight(lexer)?, chunks: Self::parse_chunks(lexer)?})
	}
}

impl Rhs {

	fn parse_weight(lexer: &mut LexerBridge) -> ParseResult<Option<SpannableNode<Expression>>> {
		match lexer.peek() {
			Some(Ok(Tokens::ProductionToken(Productions::LBr))) => Ok(Some(between(lexer, &Tokens::ProductionToken(Productions::LBr), &Tokens::MathToken(Math::RBr))?)),
			_ => Ok(None)
		}
	}

	fn parse_chunks(lexer: &mut LexerBridge) -> ParseResult<Vec<SpannableNode<Chunk>>> {
		let mut resp = Vec::new();
		if lexer.peek() == Some(&Ok(Tokens::ProductionToken(Productions::Semi))) { // If we immediately encounter a ;, this is an empty rule
			resp.push(SpannableNode::new(Chunk::empty(), lexer.span()));
		} else {
			resp.push(SpannableNode::parse(lexer)?);
			let mut check = lexer.peek();
			while check.is_some() && !matches!(check, Some(&Ok(Tokens::ProductionToken(Productions::Alternative)) | &Ok(Tokens::ProductionToken(Productions::Semi)))) {
				if matches!(check, Some(Err(_))) {
					return Err(WagParseError::Fatal((lexer.span(), "An unknown error occurred during tokenizing".to_string())))
				}
				resp.push(SpannableNode::parse(lexer)?);
				check = lexer.peek();
			}
		}
		Ok(resp)
	}

	#[cfg(test)]
	/// Automatically create an empty rule with no weight.
	pub(crate) fn empty() -> Self {
		Self {
            weight: None,
            chunks: vec![
                Chunk::empty().into()
            ]
        }
	}

	/// Automatically create a spanned empty rule with no weight.
	pub(crate) fn empty_spanned(span: Span) -> SpannableNode<Self> {
		SpannableNode::new(Self {
			weight: None,
			chunks: vec![
				Chunk::empty_spanned(span.clone())
			]
		}, span)
	}

	// #[cfg(test)]
	// /// Automatically create a rule which is just an ident. See [`Chunk::simple_ident`].
	// pub(crate) fn simple_ident(ident: &str) -> Self {
	// 	Self {
	// 		weight: None,
	// 		chunks: vec![
	// 			Chunk::simple_ident(ident).into()
	// 		]
	// 	}
	// }

	#[cfg(test)]
	/// Automatically create a rule which is just a terminal. See [`Chunk::simple_terminal`].
	pub(crate) fn simple_terminal(term: &str) -> Self {
		Self {
			weight: None,
			chunks: vec![Chunk::simple_terminal(term).into()],
		}
	}
}

use itertools::Itertools;
impl Display for Rhs {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(weight) = &self.weight {
        	write!(f, "[{weight}] ")?;
        }
        write!(f, "{}", self.chunks.iter().join(" "))
    }
}
