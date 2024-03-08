#![warn(missing_docs)]
//! WAGon Lexers
//!
//! Provides lexers for the WAGon DSL, as well as helper iterators which can switch between lexers on the fly.
//! Most likely, all you will care about are [`LexerBridge`] and [`Tokens`].
//!
//! # Example
//! ```rust
//! let s = r#"
//! meta: "data";
//! ============
//! S -> A;
//! "#;
//! use wagon_lexer::{Tokens, LexerBridge, LexResult};
//! use wagon_ident::Ident;
//! # use wagon_lexer::productions::Productions;
//! # use wagon_lexer::math::Math;
//! # use wagon_lexer::metadata::Metadata;
//! 
//! let lexer = LexerBridge::new(s);
//! let tokens: Vec<LexResult> = lexer.collect();
//! assert_eq!(tokens, vec![
//! Ok(Tokens::MetadataToken(Metadata::Identifier("meta".into()))), 
//! Ok(Tokens::MetadataToken(Metadata::Colon)),
//! Ok(Tokens::MathToken(Math::LitString("data".to_string()))),
//! Ok(Tokens::MathToken(Math::Semi)),
//! Ok(Tokens::MetadataToken(Metadata::Delim)),
//! Ok(Tokens::ProductionToken(Productions::Identifier(Ident::Unknown("S".to_string())))),
//! Ok(Tokens::ProductionToken(Productions::Produce)),
//! Ok(Tokens::ProductionToken(Productions::Identifier(Ident::Unknown("A".to_string())))),
//! Ok(Tokens::ProductionToken(Productions::Semi))
//! ])
//! ```


pub(crate) mod ident;
/// The lexer for the Math DSL
pub mod math;
/// The Lexer for the Grammar DSL
pub mod productions;
/// The Lexer for the Metadata
pub mod metadata;

use metadata::Metadata;
use wagon_utils::{ResultNext, Peek, ResultPeek, Spannable};
use logos::Logos;
use std::{fmt::{self, Display}, collections::VecDeque, error::Error};
use productions::Productions;
use math::Math;
use wagon_ident::Ident;
use replace_with::replace_with_or_abort;
use logos::Span;

/// An Enum for any errors that may occur during lexing.
#[derive(Default, Debug, Clone, Eq, PartialEq)]
pub enum LexingError {
	/// Error for any unknown reason. Usually when a character is encountered that can not be lexed.
	#[default]
	UnknownError,
	/// Error for an unexpected character.
	UnexpectedCharacter(String, Span),
	/// Error when encountering EOF before we expect.
	UnexpectedEOF(Span),
	/// Error when parsing an int
	ParseIntError(std::num::ParseIntError, Span),
	/// Error when parsing a float
	ParseFloatError(std::num::ParseFloatError, Span)
}

/// The result of each lex step is either a token or an error.
pub type LexResult = Result<Tokens, LexingError>;

impl Display for LexingError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnknownError => write!(f, "Encountered unknown error!"),
            Self::UnexpectedCharacter(c, _) => write!(f, "Encountered unexpected character {c}"),
            Self::UnexpectedEOF(_) => write!(f, "Got EOF but expected more characters!"),
            Self::ParseIntError(p, _) => p.fmt(f),
            Self::ParseFloatError(p, _) => p.fmt(f),
        }
    }
}

impl Error for LexingError {}

impl Spannable for LexingError {
    fn span(&self) -> Span {
        match self {
            Self::UnknownError => Span::default(),
            Self::UnexpectedCharacter(_, s) | Self::UnexpectedEOF(s) | Self::ParseIntError(_, s) | Self::ParseFloatError(_, s) => s.to_owned(),
        }
    }
}

#[derive(Debug)]
struct MetaLexer<'source> {
	lexer: logos::Lexer<'source, Metadata>,
	peeked: VecDeque<Result<Metadata, LexingError>>
}

/// Peek implemented as a FIFO queue.
///
/// Any time `peek` is called, the main lexer is advanced and the result is stored in the `peeked` queue. If `next` is called,
/// it takes from `peeked` first before taking from the lexer itself.
impl Peek for MetaLexer<'_> {
    fn peek(&mut self) -> Option<&Self::Item> {
        let item = self.lexer.next()?;
        self.peeked.push_back(item);
        self.peeked.back()
    }

    fn peek_mut(&mut self) -> Option<&mut Self::Item> {
        let item = self.lexer.next()?;
        self.peeked.push_back(item);
        self.peeked.back_mut()
    }
}

impl Iterator for MetaLexer<'_> {
    type Item = Result<Metadata, LexingError>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.peeked.pop_front() {
            Some(x) => Some(x),
            None => self.lexer.next(),
        }
    }
}

impl<'source> MetaLexer<'source> {
	const fn new(lexer: logos::Lexer<'source, Metadata>) -> Self {
		Self{lexer, peeked: VecDeque::new()}
	}
}

#[derive(Debug)]
enum Lexer<'source> {
	Productions(logos::Lexer<'source, Productions>),
	Math(logos::Lexer<'source, Math>),
	Metadata(MetaLexer<'source>),
}

impl<'source> Lexer<'source> {
	fn new(s: &'source str) -> Self {
		let mut meta_lexer = MetaLexer::new(Metadata::lexer(s));
		let fst = meta_lexer.peek();
		if matches!(fst, Some(Ok(Metadata::Identifier(_)))) {
			if matches!(meta_lexer.peek(), Some(Ok(Metadata::Colon))) {
				Self::Metadata(meta_lexer)
			} else {
				Self::Productions(Productions::lexer(s))
			}
		} else if !matches!(fst, Some(Err(_))) {
			Self::Metadata(meta_lexer)
		} else {
			Self::Productions(Productions::lexer(s))
		}
	}
}

/// An enum that holds the different types of tokens for the different lexers.
#[derive(Debug, PartialEq, Clone)]
pub enum Tokens {
	/// Tokens created by the [Productions] lexer.
	ProductionToken(Productions),
	/// Tokens created by the [Math] lexer.
	MathToken(Math),
	/// Tokens created by the [Metadata] lexer.
	MetadataToken(Metadata)
}

impl fmt::Display for Tokens {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::ProductionToken(t) => t.fmt(f),
            Self::MathToken(t) => t.fmt(f),
            Self::MetadataToken(t) => t.fmt(f)
        }
    }
}

impl Default for Tokens {
    fn default() -> Self {
        Self::ProductionToken(Productions::Identifier(Ident::default()))
    }
}

/// A struct which automatically switches between the different lexers based on context.
#[allow(clippy::option_option)]
pub struct LexerBridge<'source> {
	lexer: Lexer<'source>,
	counter: u16,
	in_meta: bool,
	peeked: Option<Option<<LexerBridge<'source> as IntoIterator>::Item>>
}

impl<'source> LexerBridge<'source> {
	/// Initialize the `LexerBridge`
	#[must_use] pub fn new(s: &'source str) -> Self {
		let lexer = Lexer::new(s);
		let counter = 0;
		let in_meta = matches!(lexer, Lexer::Metadata(_));
		let peeked = None;
		Self { lexer, counter, in_meta, peeked }
	}

	/// Inspect what part of the input string the lexer is currently at
	#[must_use] pub fn slice(&self) -> &str {
		match &self.lexer {
		    Lexer::Productions(l) => l.slice(),
		    Lexer::Math(l) => l.slice(),
		    Lexer::Metadata(l) => l.lexer.slice()
		}
	}

	pub(crate) fn _morph_to_math<Token>(curr: logos::Lexer<'source, Token>) -> Lexer
	where
        Token: Logos<'source, Source = <Math as Logos<'source>>::Source>,
        Token::Extras: Into<<Math as Logos<'source>>::Extras>,
    {
		Lexer::Math(curr.morph())
	}

	pub(crate) fn _morph_to_productions<Token>(curr: logos::Lexer<'source, Token>) -> Lexer
	where
        Token: Logos<'source, Source = <Productions as Logos<'source>>::Source>,
        Token::Extras: Into<<Productions as Logos<'source>>::Extras>,
    {
		Lexer::Productions(curr.morph())
	}

	pub(crate) fn _morph_to_metadata<Token>(curr: logos::Lexer<'source, Token>) -> Lexer
	where
        Token: Logos<'source, Source = <Metadata as Logos<'source>>::Source>,
        Token::Extras: Into<<Metadata as Logos<'source>>::Extras>,
    {
		Lexer::Metadata(MetaLexer::new(curr.morph()))
	}

	/// Make the `LexerBridge` use the [Math] lexer.
	pub fn morph_to_math(&mut self) {
		replace_with_or_abort(&mut self.lexer, |lexer| match lexer {
			Lexer::Productions(prod) => Self::_morph_to_math(prod),
		    Lexer::Math(_) => lexer,
		    Lexer::Metadata(meta) => Self::_morph_to_math(meta.lexer),
		});
	}

	/// Make the `LexerBridge` use the [Productions] lexer.
	pub fn morph_to_productions(&mut self) {
		replace_with_or_abort(&mut self.lexer, |lexer| match lexer {
			Lexer::Productions(_) => lexer,
		    Lexer::Math(math) => Self::_morph_to_productions(math),
		    Lexer::Metadata(meta) => Self::_morph_to_productions(meta.lexer),
		});
	}

	/// Make the `LexerBridge` use the [Metadata] lexer.
	pub fn morph_to_metadata(&mut self) {
		replace_with_or_abort(&mut self.lexer, |lexer| match lexer {
			Lexer::Productions(prod) => Self::_morph_to_metadata(prod),
		    Lexer::Math(math) => Self::_morph_to_metadata(math),
		    Lexer::Metadata(_) => lexer,
		});
	}
}

/// Implemented like [`std::iter::Peekable`]. 
impl<'source> Peek for LexerBridge<'source> {

    fn peek(&mut self) -> Option<&Self::Item> {
    	let next = self.peeked.take().map_or_else(|| self.next(), |x| x);
	    self.peeked.get_or_insert(next).as_ref()
    }

    fn peek_mut(&mut self) -> Option<&mut Self::Item> {
        let next = self.peeked.take().map_or_else(|| self.next(), |x| x);
	    self.peeked.get_or_insert(next).as_mut()
    }
}

impl<'source> ResultNext<Tokens, LexingError> for LexerBridge<'source> {
    fn next_result(&mut self) -> Result<Tokens, LexingError> {
        match self.next() {
            Some(Err(LexingError::UnknownError)) => Err(LexingError::UnexpectedCharacter(self.slice().to_string(), self.span())),
            Some(x) => x,
            None => Err(LexingError::UnexpectedEOF(self.span())),
        }
    }
}

impl<'source> ResultPeek<Tokens, LexingError> for LexerBridge<'source> {
    fn peek_result(&mut self) -> Result<&Tokens, LexingError> {
    	let next = self.peeked.take().map_or_else(|| self.next(), |x| x);
	    let slice = self.slice().to_string();
    	let span = self.span();
	    let peek = self.peeked.get_or_insert(next).as_ref();
        match peek {
        	Some(Err(LexingError::UnknownError)) => Err(LexingError::UnexpectedCharacter(slice, span)),
            Some(Ok(x)) => Ok(x),
            Some(Err(e)) => Err(e.clone()),
            None => Err(LexingError::UnexpectedEOF(span)),
        }
    }
}

impl Spannable for LexerBridge<'_> {
	fn span(&self) -> Span {
		self.lexer.span()
	}
}

impl Spannable for Lexer<'_> {
	fn span(&self) -> Span {
		match self {
			Self::Productions(l) => l.span(),
			Self::Math(l) => l.span(),
			Self::Metadata(l) => l.lexer.span(),
		}
	}
}

trait TypeDetect {
	fn detect(inp: &str, span: Span) -> Result<Self, LexingError> where Self: Sized;
}

impl<'source> Iterator for LexerBridge<'source> {
	type Item = Result<Tokens, LexingError>;
	fn next(&mut self) -> Option<Self::Item> {
        use Tokens::{MathToken, MetadataToken, ProductionToken};
        type MorphFunc<'source> = Option<Box<dyn Fn(&mut LexerBridge<'source>)>>;
        if let Some(v) = self.peeked.take() {
        	v
        } else {
        	let (morph_func, item): (MorphFunc, Option<Result<Tokens, LexingError>>) = match &mut self.lexer {
	            Lexer::Productions(prod) => {
	            	let result = prod.next();
	                let ret_func: MorphFunc = match result {
	                	Some(Ok(Productions::LBr | Productions::LCur)) => Some(Box::new(Self::morph_to_math)),
	                	_ => None
	                };
	                (ret_func, result.map(|prod| prod.map(ProductionToken)))
	            },
	            Lexer::Math(math) => {
	            	let result = math.next();
	                let ret_func: MorphFunc = match result {
	                	Some(Ok(Math::LBr)) => {self.counter += 1; None},
	                	Some(Ok(Math::RCur)) => Some(Box::new(Self::morph_to_productions)),
	                	Some(Ok(Math::RBr)) if self.counter == 0 => Some(Box::new(Self::morph_to_productions)),
	                	Some(Ok(Math::Semi)) if self.in_meta => Some(Box::new(Self::morph_to_metadata)),
	                	Some(Ok(Math::RBr)) => {self.counter -= 1; None},
	                	_ => None
	                };
	                (ret_func, result.map(|math| math.map(MathToken)))
	            },
	            Lexer::Metadata(meta) => {
	            	let result = meta.next();
	            	let ret_func: MorphFunc = match result {
	            		Some(Ok(Metadata::Delim)) => {
	            			self.in_meta = false;
	            			Some(Box::new(Self::morph_to_productions))
	            		},
	            		Some(Ok(Metadata::Colon)) => Some(Box::new(Self::morph_to_math)),
	            		_ => None
	            	};
	            	(ret_func, result.map(|m| m.map(MetadataToken)))
	            },
	        };
	        if let Some(fun) = morph_func {
	        	fun(self);
	        }
	        item
	    }
    }
}

#[cfg(test)]
/// A helper method to assert that a lexer will encounter a given list of tokens.
///
/// # Panics
/// This assert panics if it reaches the end of the tokenstream before it's done checking all expected tokens.
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
	use super::LexingError;
	use super::productions::Productions;
	use super::math::Math;
	use super::metadata::Metadata;
	use super::Ident::*;

	use pretty_assertions::assert_eq;

	#[test]
	fn test_mode_switching() {
		let s = "[3 + 2.3 - &x] 'a thing' X {y = 4 + ($z['key'] < 3);}";
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
			Ok(MathToken(Math::Identifier(Local("z".to_string())))),
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

	#[test]
	fn test_mode_switching_complex() {
		let s = r#"
		include some::path;
		left: "right";
		true_val: true;
		===================
		S -> [2] A;
		"#;
		let lexer = LexerBridge::new(s);
		let results: Vec<Result<Tokens, LexingError>> = lexer.collect();
		let expect = vec![
			Ok(MetadataToken(Metadata::Include)),
			Ok(MetadataToken(Metadata::Path("some::path".to_string()))),
			Ok(MetadataToken(Metadata::Semi)),
			Ok(MetadataToken(Metadata::Identifier("left".into()))),
			Ok(MetadataToken(Metadata::Colon)),
			Ok(MathToken(Math::LitString("right".to_string()))),
			Ok(MathToken(Math::Semi)),
			Ok(MetadataToken(Metadata::Identifier("true_val".into()))),
			Ok(MetadataToken(Metadata::Colon)),
			Ok(MathToken(Math::LitBool(true))),
			Ok(MathToken(Math::Semi)),
			Ok(MetadataToken(Metadata::Delim)),
			Ok(ProductionToken(Productions::Identifier(Unknown("S".to_string())))),
			Ok(ProductionToken(Productions::Produce)),
			Ok(ProductionToken(Productions::LBr)),
			Ok(MathToken(Math::LitInt(2))),
			Ok(MathToken(Math::RBr)),
			Ok(ProductionToken(Productions::Identifier(Unknown("A".to_string())))),
			Ok(ProductionToken(Productions::Semi))
		];
		assert_eq!(results, expect);
	}
}