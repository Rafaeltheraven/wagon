use wagon_lexer::{UnsafePeek, UnsafeNext, productions::Productions};
use wagon_utils::string_vec;
use super::{Parse, PeekLexer, ParseResult, Tokens, Spannable, WagParseError, ToAst, WagNode, WagIx, WagTree};

impl Parse for String {
    fn parse(lexer: &mut PeekLexer) -> ParseResult<Self> {
        Ok(lexer.next_unwrap().to_string())
    }
}

impl ToAst for String {
	fn to_ast(self, ast: &mut WagTree) -> WagIx {
		ast.add_node(WagNode::Generic(self))
	}
}

pub(super) trait TokenMapper {
	fn token_to_enum(token: &Tokens) -> Option<Self> where Self: Sized;
}

fn __between_right<T>(lexer: &mut PeekLexer, right: Tokens, fun: Box<dyn FnOnce(&mut PeekLexer) -> ParseResult<T>>) -> ParseResult<T> {
	let resp = fun(lexer)?;
	let span = lexer.span();
	let token = lexer.peek_unwrap();
	if token == &right {
		lexer.next();
		Ok(resp)
	} else {
		Err(WagParseError::Unexpected { span, offender: token.clone(), expected: string_vec!(right) })
	}
}

fn __between<T>(lexer: &mut PeekLexer, left: Tokens, right: Tokens, fun: Box<dyn FnOnce(&mut PeekLexer) -> ParseResult<T>>) -> ParseResult<T> {
	let span = lexer.span();
	let token = lexer.peek_unwrap();
	if token == &left {
		lexer.next();
		__between_right(lexer, right, fun)
	} else {
		Err(WagParseError::Unexpected { span, offender: token.clone(), expected: string_vec!(left) })
	}
}

pub(super) fn between_right<T: Parse>(lexer: &mut PeekLexer, right: Tokens) -> ParseResult<T> {
	__between_right(lexer, right, Box::new(|x| T::parse(x)))
}

pub(super) fn between<T: Parse>(lexer: &mut PeekLexer, left: Tokens, right: Tokens) -> ParseResult<T> {
	__between(lexer, left, right, Box::new(|x| T::parse(x)))
}

pub(super) fn between_sep<T: Parse>(lexer: &mut PeekLexer, left: Tokens, right: Tokens, sep: Tokens) -> ParseResult<Vec<T>> {
	__between(lexer, left, right, Box::new(|x| T::parse_sep(x, sep)))
}

#[macro_export] 
macro_rules! either_token {
    ($variant:ident($($arg:tt)*)) => {
        wagon_lexer::Tokens::ProductionToken(wagon_lexer::productions::Productions::$variant($($arg)*)) | wagon_lexer::Tokens::MathToken(wagon_lexer::math::Math::$variant($($arg)*))
    };
    ($variant:ident) => {
    	wagon_lexer::Tokens::ProductionToken(wagon_lexer::productions::Productions::$variant) | wagon_lexer::Tokens::MathToken(wagon_lexer::math::Math::$variant)
    };
}


#[macro_export]
macro_rules! either_token_ref {
	($variant:ident($($arg:tt)*)) => {
        &wagon_lexer::Tokens::ProductionToken(wagon_lexer::productions::Productions::$variant($($arg)*)) | &wagon_lexer::Tokens::MathToken(wagon_lexer::math::Math::$variant($($arg)*))
    };
    ($variant:ident) => {
    	&wagon_lexer::Tokens::ProductionToken(wagon_lexer::productions::Productions::$variant) | &wagon_lexer::Tokens::MathToken(wagon_lexer::math::Math::$variant)
    };
}

#[macro_export] 
macro_rules! any_token {
    ($variant:ident($($arg:tt)*)) => {
        either_token!($variant) | wagon_lexer::Tokens::MetadataToken(wagon_lexer::metadata::Metadata::$variant($($arg)*))
    };
    ($variant:ident) => {
    	either_token!($variant) | wagon_lexer::Tokens::MetadataToken(wagon_lexer::metadata::Metadata::$variant)
    };
}

pub(super) fn check_semi(lexer: &mut PeekLexer) -> Result<(), WagParseError> {
	if lexer.next_if(|x| matches!(x, Ok(any_token!(Semi)))).is_none() {
    	Err(WagParseError::Unexpected { span: lexer.span(), offender: lexer.next_unwrap(), expected: string_vec![Tokens::ProductionToken(Productions::Semi)] })
    } else {
    	Ok(())
    }
}

pub(super) fn check_colon(lexer: &mut PeekLexer) -> Result<(), WagParseError> {
	if lexer.next_if(|x| matches!(x, Ok(either_token!(Colon)))).is_none() {
    	Err(WagParseError::Unexpected { span: lexer.span(), offender: lexer.next_unwrap(), expected: string_vec![Tokens::ProductionToken(Productions::Colon)] })
    } else {
    	Ok(())
    }
}