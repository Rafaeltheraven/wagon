use super::{Parse, PeekLexer, ParseResult, Tokens};
use super::atom::Atom;
use crate::lexer::{math::Math, UnsafePeek};

#[derive(PartialEq, Debug)]
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