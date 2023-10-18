pub(crate) mod ident;
pub(crate) mod math;
pub(crate) mod productions;

use crate::helpers::peekable::Peekable;
use logos::{Logos, Span};
use std::{fmt::{self}};
use productions::Productions;
use math::Math;
use super::gll::ident::Ident;

#[derive(Default, Debug, Clone, PartialEq)]
pub(crate) enum LexingError {
	#[default]
	UnknownError
}

#[derive(Debug)]
enum Lexer<'source> {
	Productions(logos::Lexer<'source, Productions>),
	Math(logos::Lexer<'source, Math>),
}

impl<'source> Lexer<'source> {
	fn new(s: &'source str) -> Self {
		Self::Productions(Productions::lexer(s))
	}
}

#[derive(Debug, PartialEq)]
pub(crate) enum Tokens {
	ProductionToken(Productions),
	MathToken(Math)
}

impl fmt::Display for Tokens {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Tokens::ProductionToken(t) => t.fmt(f),
            Tokens::MathToken(t) => t.fmt(f),
        }
    }
}

impl Default for Tokens {
    fn default() -> Self {
        Self::ProductionToken(Productions::Identifier(Ident::default()))
    }
}

pub(crate) struct LexerBridge<'source> {
	lexer: Lexer<'source>,
	counter: u16,
}

impl<'source> LexerBridge<'source> {
	pub(crate) fn new(s: &'source str) -> Self {
		Self { lexer: Lexer::new(s), counter: 0 }
	}
}

pub(crate) trait UnsafeNext<T, E: std::fmt::Debug>: Iterator<Item = Result<T, E>> {
	fn next_unwrap(&mut self) -> T {
		match self.next() {
		    Some(Ok(x)) => x,
		    Some(Err(e)) => panic!("Got lexing error: {:?}", e),
		    None => panic!("Expected a token, but failed")
		}
	}
}

pub(crate) trait UnsafePeek<T> {
	fn peek_unwrap(&mut self) -> &T;
}

pub(crate) trait Spannable {
	fn span(&mut self) -> Span;
}

impl<'source> UnsafeNext<Tokens, LexingError> for &mut LexerBridge<'source>{}
impl<'source> UnsafeNext<Tokens, LexingError> for Peekable<LexerBridge<'source>>{}
impl<'source> UnsafePeek<Tokens> for Peekable<LexerBridge<'source>> {
	fn peek_unwrap(&mut self) -> &Tokens {
		match self.peek() {
			Some(Ok(x)) => x,
		    Some(Err(e)) => panic!("Got lexing error: {:?}", e),
		    None => panic!("Expected a token, but failed")
		}
	}
}

trait GetInnerLexerBridge {
	fn inner_bridge(&mut self) -> &LexerBridge;
}

impl Spannable for PeekLexer<'_> {
    fn span(&mut self) -> Span {
        self.iter.span()
    }
}

impl Spannable for LexerBridge<'_> {
	fn span(&mut self) -> Span {
		self.lexer.span()
	}
}

impl Spannable for Lexer<'_> {
	fn span(&mut self) -> Span {
		match self {
			Self::Productions(l) => l.span(),
			Self::Math(l) => l.span()
		}
	}
}

trait TypeDetect {
	fn detect(inp: &str) -> Self;
}

impl<'source> Iterator for LexerBridge<'source> {
	type Item = Result<Tokens, LexingError>;
	fn next(&mut self) -> Option<Self::Item> {
        use Tokens::*;
        match &mut self.lexer {
            Lexer::Productions(prod) => {
                let result = prod.next();
                match result {
                	Some(Ok(Productions::LBr)) | Some(Ok(Productions::LCur)) => self.lexer = Lexer::Math(prod.to_owned().morph()),
                	_ => {}
                };
                result.map(|prod| prod.map(ProductionToken))
            }
            Lexer::Math(math) => {
                let result = math.next();
                match (&result, self.counter) {
                	(Some(Ok(Math::LBr)), _) => self.counter += 1,
                	(Some(Ok(Math::RBr)), 0) | (Some(Ok(Math::RCur)), _) => self.lexer = Lexer::Productions(math.to_owned().morph()),
                	(Some(Ok(Math::RBr)), _) => self.counter -= 1,
                	_ => {}
                };
                result.map(|math| math.map(MathToken))
            }
        }
    }
}

pub(crate) type PeekLexer<'source> = Peekable<LexerBridge<'source>>;

#[cfg(test)]
pub fn assert_lex<'a, Token>(
    source: &'a Token::Source,
    tokens: &[Result<Token, Token::Error>],
) where
    Token: Logos<'a> + fmt::Debug + PartialEq,
    Token::Extras: Default,
{
    let mut lex = Token::lexer(source);

    for token in tokens {
        assert_eq!(
            &lex.next().expect("Unexpected end"),
            token
        );
    }

    assert_eq!(lex.next(), None);
}


#[cfg(test)]
mod tests {

	use super::{LexerBridge, Tokens, Tokens::*};
	use crate::lexer::LexingError;
	use crate::lexer::productions::Productions;
	use crate::lexer::math::Math;
	use crate::gll::ident::Ident::*;

	#[test]
	fn test_mode_switching() {
		let s = "[3 + 2.3 - $x] 'a thing' X {y = 4 + (!z['key'] < 3);}";
		let lexer = LexerBridge::new(s);
		let results: Vec<Result<Tokens, LexingError>> = lexer.collect();
		let expect = vec![
			Ok(ProductionToken(Productions::LBr)),
			Ok(MathToken(Math::LitInt(3))),
			Ok(MathToken(Math::Add)),
			Ok(MathToken(Math::LitFloat(2.3))),
			Ok(MathToken(Math::Sub)),
			Ok(MathToken(Math::Identifier(Synth("x".to_string())))),
			Ok(MathToken(Math::RBr)),
			Ok(ProductionToken(Productions::LitString("a thing".to_string()))),
			Ok(ProductionToken(Productions::Identifier(Unknown("X".to_string())))),
			Ok(ProductionToken(Productions::LCur)),
			Ok(MathToken(Math::Identifier(Unknown("y".to_string())))),
			Ok(MathToken(Math::Assigns)),
			Ok(MathToken(Math::LitInt(4))),
			Ok(MathToken(Math::Add)),
			Ok(MathToken(Math::LPar)),
			Ok(MathToken(Math::Identifier(Inherit("z".to_string())))),
			Ok(MathToken(Math::LBr)),
			Ok(MathToken(Math::LitString("key".to_string()))),
			Ok(MathToken(Math::RBr)),
			Ok(MathToken(Math::Lt)),
			Ok(MathToken(Math::LitInt(3))),
			Ok(MathToken(Math::RPar)),
			Ok(MathToken(Math::Semi)),
			Ok(MathToken(Math::RCur))
		];
		assert_eq!(results, expect);
	}
}