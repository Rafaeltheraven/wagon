use super::{TypeDetect, LexingError};
use logos::Logos;
use wagon_macros::inherit_from_base;
use logos_display::{Debug, Display};
use wagon_utils::rem_first_and_last_char;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
/// Enum to specify what type of import this is.
pub enum ImportType {
	/// `<-`
	Basic,
	/// `<=`
	Full,
	/// `<<`
	Recursive,
	/// `</`
	Exclude
}

impl TypeDetect for ImportType {
	fn detect(inp: &str, span: logos::Span) -> Result<Self, LexingError> {
	    match inp.chars().last() {
	    	Some('-') => Ok(ImportType::Basic),
	    	Some('=') => Ok(ImportType::Full),
	    	Some('<') => Ok(ImportType::Recursive),
	    	Some('/') => Ok(ImportType::Exclude),
	    	Some(x) => Err(LexingError::UnexpectedCharacter(x.to_string(), span)),
	    	None => Err(LexingError::UnexpectedEOF(span))
	    }
	}
}

impl Default for ImportType {
    fn default() -> Self {
        Self::Basic
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
/// Enum to specify a type of EBNF operator.
pub enum EbnfType {
	/// `+`
	Some,
	/// `*`
	Many,
	/// `?`
	Maybe
}

impl TypeDetect for EbnfType {
	fn detect(inp: &str, span: logos::Span) -> Result<Self, LexingError> {
	    match inp.chars().next() {
	    	Some('+') => Ok(EbnfType::Some),
	    	Some('*') => Ok(EbnfType::Many),
	    	Some('?') => Ok(EbnfType::Maybe),
	    	Some(x)   => Err(LexingError::UnexpectedCharacter(x.to_string(), span)),
	    	None      => Err(LexingError::UnexpectedEOF(span))
	    }
	}
}

#[inherit_from_base]
/// Lexer for the grammar DSL.
pub enum Productions {

	#[token("->")]
	/// `->`
	Produce,

	#[token("=>")]
	/// `=>`
	Generate,

	#[token("|")]
	/// `|`
	Alternative,

	#[token("&")]
	/// `&`
	Additional,

	#[display_override("Import")]
	#[regex("<(-|=|<|/)", |lex| ImportType::detect(lex.slice(), lex.span()))]
	/// Any of the possible [ImportType] arrows.
	Import(ImportType),

	#[display_override("Regex")]
	#[regex(r#"/[^\*]([^/\\]|\\.)*/"#, |lex| rem_first_and_last_char(lex.slice()))]
	/// A regular expression (defined as any string between two `/`).
	LitRegex(String),

	#[display_override("EBNF Operator")]
	#[regex(r#"(\*|\+|\?)"#, |lex| EbnfType::detect(lex.slice(), lex.span()))]
	/// Any of the possible [EbnfType] operators.
	Ebnf(EbnfType),
}

#[cfg(test)]
mod tests {
	use wagon_ident::Ident;
	use crate::{assert_lex, LexingError};
	use std::assert_eq;

	use logos::Logos;
	use super::Productions::{self};
	use super::ImportType::*;
	use super::EbnfType;

	#[test]
	fn test_quoted_string_double() {
		let s = r#""This is \" a 'with single quotes inside' string \\\" ""#;
		let mut lex = Productions::lexer(s);
		assert_eq!(lex.next(), Some(Ok(Productions::LitString("This is \\\" a 'with single quotes inside' string \\\\\\\" ".to_string()))));

		let s2 = r#""This one should \\" fail" before this"#;
		let mut lex = Productions::lexer(s2);
		assert_eq!(lex.next(), Some(Ok(Productions::LitString("This one should \\\\".to_string()))));
		assert_eq!(lex.next(), Some(Ok(Productions::Identifier(Ident::Unknown("fail".to_string())))));
		assert_eq!(lex.next(), Some(Err(LexingError::UnknownError)));
	}

	#[test]
	fn test_quoted_string_single() {
		let s = r#"'This is \' a "with double quotes inside" string \\\' '"#;
		let mut lex = Productions::lexer(s);
		assert_eq!(lex.next(), Some(Ok(Productions::LitString("This is \\' a \"with double quotes inside\" string \\\\\\' ".to_string()))));

		let s2 = r"'This one should \\' fail' before this";
		let mut lex = Productions::lexer(s2);
		assert_eq!(lex.next(), Some(Ok(Productions::LitString("This one should \\\\".to_string()))));
		assert_eq!(lex.next(), Some(Ok(Productions::Identifier(Ident::Unknown("fail".to_string())))));
		assert_eq!(lex.next(), Some(Err(LexingError::UnknownError)));
	}

	#[test]
	fn test_identifier_matching() {
		let s = "&synthesized *inherited $local unknown";
		let expect = &[
			Ok(Productions::Identifier(Ident::Synth("synthesized".to_string()))),
			Ok(Productions::Identifier(Ident::Inherit("inherited".to_string()))),
			Ok(Productions::Identifier(Ident::Local("local".to_string()))),
			Ok(Productions::Identifier(Ident::Unknown("unknown".to_string())))
		];
		assert_lex(s, expect);
	}

	#[test]
	fn test_regex() {
		let s = r"/[a-z][A-Z][^\/]/";
		let expect = &[Ok(Productions::LitRegex("[a-z][A-Z][^\\/]".to_string()))];
		assert_lex(s, expect);
	}

	#[test]
	fn test_imports() {
		let s = "<- <= << </";
		let expect = &[
			Ok(Productions::Import(Basic)),
			Ok(Productions::Import(Full)),
			Ok(Productions::Import(Recursive)),
			Ok(Productions::Import(Exclude))
		];
		assert_lex(s, expect);
	}

	#[test]
	fn test_ebnf() {
		let s = "* + ?";
		let expect = &[
			Ok(Productions::Ebnf(EbnfType::Many)),
			Ok(Productions::Ebnf(EbnfType::Some)),
			Ok(Productions::Ebnf(EbnfType::Maybe))
		];
		assert_lex(s, expect);
	}

	#[test]
	fn test_simple() {
		let s = "S -> 'a' S | ";
		let expect = &[
			Ok(Productions::Identifier(Ident::Unknown("S".to_string()))),
			Ok(Productions::Produce),
			Ok(Productions::LitString("a".to_string())),
			Ok(Productions::Identifier(Ident::Unknown("S".to_string()))),
			Ok(Productions::Alternative)
		];
		assert_lex(s, expect)
	}
}