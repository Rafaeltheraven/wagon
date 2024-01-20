use wagon_lexer::productions::Productions;
use wagon_utils::string_vec;
use super::{Parse, LexerBridge, ParseResult, Tokens, Spannable, WagParseError, ToAst, WagNode, WagIx, WagTree, ResultNext, Peek, ResultPeek};

impl Parse for String {
    fn parse(lexer: &mut LexerBridge) -> ParseResult<Self> {
        Ok(lexer.next_result()?.to_string())
    }
}

impl ToAst for String {
	fn to_ast(self, ast: &mut WagTree) -> WagIx {
		ast.add_node(WagNode::Generic(self))
	}
}

/// A helper trait to quickly convert from any [`Tokens`] into an instance of something else.
pub(super) trait TokenMapper {
	fn token_to_enum(token: &Tokens) -> Option<Self> where Self: Sized;
}

type ParseFunc<T> = Box<dyn FnOnce(&mut LexerBridge) -> ParseResult<T>>;

fn __between_right<T>(lexer: &mut LexerBridge, right: Tokens, fun: ParseFunc<T>) -> ParseResult<T> {
	let resp = fun(lexer)?;
	let span = lexer.span();
	let token = lexer.peek_result()?;
	if token == &right {
		lexer.next();
		Ok(resp)
	} else {
		Err(WagParseError::Unexpected { span, offender: token.clone(), expected: string_vec!(right) })
	}
}

fn __between<T>(lexer: &mut LexerBridge, left: Tokens, right: Tokens, fun: ParseFunc<T>) -> ParseResult<T> {
	let span = lexer.span();
	let token = lexer.peek_result()?;
	if token == &left {
		lexer.next();
		__between_right(lexer, right, fun)
	} else {
		Err(WagParseError::Unexpected { span, offender: token.clone(), expected: string_vec!(left) })
	}
}

/// Parse a node, terminated by a final right [`Tokens`].
pub(super) fn between_right<T: Parse>(lexer: &mut LexerBridge, right: Tokens) -> ParseResult<T> {
	__between_right(lexer, right, Box::new(|x| T::parse(x)))
}

/// Parse a node that is wrapped between a left [`Tokens`] and a right [`Tokens`].
pub(super) fn between<T: Parse>(lexer: &mut LexerBridge, left: Tokens, right: Tokens) -> ParseResult<T> {
	__between(lexer, left, right, Box::new(|x| T::parse(x)))
}

/// Parse multiple nodes wrapped between left and right [`Tokens`] and separated by (another) [`Tokens`].
pub(super) fn between_sep<T: Parse>(lexer: &mut LexerBridge, left: Tokens, right: Tokens, sep: Tokens) -> ParseResult<Vec<T>> {
	__between(lexer, left, right, Box::new(|x| T::parse_sep(x, sep)))
}

/// A macro that automatically expands to allow either a [`wagon_lexer::Tokens::ProductionToken`] or a [`wagon_lexer::Tokens::MathToken`].
#[macro_export] 
macro_rules! either_token {
    ($variant:ident($($arg:tt)*)) => {
        wagon_lexer::Tokens::ProductionToken(wagon_lexer::productions::Productions::$variant($($arg)*)) | wagon_lexer::Tokens::MathToken(wagon_lexer::math::Math::$variant($($arg)*))
    };
    ($variant:ident) => {
    	wagon_lexer::Tokens::ProductionToken(wagon_lexer::productions::Productions::$variant) | wagon_lexer::Tokens::MathToken(wagon_lexer::math::Math::$variant)
    };
}

/// The same as [`either_token!`] but as a reference.
#[macro_export]
macro_rules! either_token_ref {
	($variant:ident($($arg:tt)*)) => {
        &wagon_lexer::Tokens::ProductionToken(wagon_lexer::productions::Productions::$variant($($arg)*)) | &wagon_lexer::Tokens::MathToken(wagon_lexer::math::Math::$variant($($arg)*))
    };
    ($variant:ident) => {
    	&wagon_lexer::Tokens::ProductionToken(wagon_lexer::productions::Productions::$variant) | &wagon_lexer::Tokens::MathToken(wagon_lexer::math::Math::$variant)
    };
}

/// A macro that automatically expands to allow either a [`wagon_lexer::Tokens::ProductionToken`], a [`wagon_lexer::Tokens::MathToken`] or a [`wagon_lexer::Tokens::MetadataToken`].
#[macro_export] 
macro_rules! any_token {
    ($variant:ident($($arg:tt)*)) => {
        either_token!($variant) | wagon_lexer::Tokens::MetadataToken(wagon_lexer::metadata::Metadata::$variant($($arg)*))
    };
    ($variant:ident) => {
    	either_token!($variant) | wagon_lexer::Tokens::MetadataToken(wagon_lexer::metadata::Metadata::$variant)
    };
}

/// Check if there's a `;` token, return an error otherwise.
pub(super) fn check_semi(lexer: &mut LexerBridge) -> Result<(), WagParseError> {
	if lexer.next_if(|x| matches!(x, Ok(wagon_lexer::Tokens::ProductionToken(wagon_lexer::productions::Productions::Semi)
| wagon_lexer::Tokens::MathToken(wagon_lexer::math::Math::Semi) |
wagon_lexer::Tokens::MetadataToken(wagon_lexer::metadata::Metadata::Semi)))).is_none() {
    	Err(WagParseError::Unexpected { span: lexer.span(), offender: lexer.next_result()?, expected: string_vec![Tokens::ProductionToken(Productions::Semi)] })
    } else {
    	Ok(())
    }
}

/// Check if there's a `:` token, return an error otherwise.
pub(super) fn check_colon(lexer: &mut LexerBridge) -> Result<(), WagParseError> {
	if lexer.next_if(|x| matches!(x, Ok(either_token!(Colon)))).is_none() {
    	Err(WagParseError::Unexpected { span: lexer.span(), offender: lexer.next_result()?, expected: string_vec![Tokens::ProductionToken(Productions::Colon)] })
    } else {
    	Ok(())
    }
}