use crate::{lexer::{UnsafeNext}, string_vec};
use super::{Parse, PeekLexer, ParseResult, Tokens, Spannable, WagParseError};

impl Parse for String {
    fn parse(lexer: &mut PeekLexer) -> ParseResult<Self> {
        Ok(lexer.next_unwrap().to_string())
    }
}

pub(super) trait TokenMapper {
	fn token_to_enum(token: &Tokens) -> Option<Self> where Self: Sized;
}

fn __between_right<T>(lexer: &mut PeekLexer, right: Tokens, fun: Box<dyn FnOnce(&mut PeekLexer) -> ParseResult<T>>) -> ParseResult<T> {
	let resp = fun(lexer)?;
	let token = lexer.next_unwrap();
	if token == right {
		Ok(resp)
	} else {
		Err(WagParseError::Unexpected { span: lexer.span(), offender: token, expected: string_vec!(right) })
	}
}

fn __between<T>(lexer: &mut PeekLexer, left: Tokens, right: Tokens, fun: Box<dyn FnOnce(&mut PeekLexer) -> ParseResult<T>>) -> ParseResult<T> {
	let token = lexer.next_unwrap();
	if token == left {
		__between_right(lexer, right, fun)
	} else {
		Err(WagParseError::Unexpected { span: lexer.span(), offender: token, expected: string_vec!(left) })
	}
}

pub(super) fn between_right<T: Parse>(lexer: &mut PeekLexer, right: Tokens) -> ParseResult<T> {
	__between_right(lexer, right, Box::new(|x| T::parse(x)))
}

pub(super) fn between<T: Parse>(lexer: &mut PeekLexer, left: Tokens, right: Tokens) -> ParseResult<T> {
	__between(lexer, left, right, Box::new(|x| T::parse(x)))
}

pub(super) fn between_sep<T: Parse + std::fmt::Debug>(lexer: &mut PeekLexer, left: Tokens, right: Tokens, sep: Tokens) -> ParseResult<Vec<T>> {
	__between(lexer, left, right, Box::new(|x| T::parse_sep(x, sep)))
}

#[macro_export] 
macro_rules! either_token {
    ($variant:ident($($arg:tt)*)) => {
        Tokens::ProductionToken(Productions::$variant($($arg)*)) | Tokens::MathToken(Math::$variant($($arg)*))
    };
}