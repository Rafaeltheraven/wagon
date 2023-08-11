
use super::TypeDetect;
use logos::Logos;
use wagon_macros::inherit_from_base;
use logos_display::Display;
use crate::helpers::rem_first_and_last_char;
use super::ident::{Ident};

#[derive(Debug, PartialEq, Eq, Clone)]
pub(crate) enum GrammarType {
	Conversational,
	Generative,
	General
}

impl TypeDetect for GrammarType {
	fn detect(inp: &str) -> Self {
	    if inp.starts_with('c') {
	    	GrammarType::Conversational
	    } else if inp.starts_with("ge") {
	    	GrammarType::Generative
	    } else {
	    	GrammarType::General
	    }
	}
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub(crate) enum ImportType {
	Basic,
	Full,
	Recursive,
	Exclude
}

impl TypeDetect for ImportType {
	fn detect(inp: &str) -> Self {
	    match inp.chars().last().unwrap() {
	    	'-' => ImportType::Basic,
	    	'=' => ImportType::Full,
	    	'<' => ImportType::Recursive,
	    	'/' => ImportType::Exclude,
	    	_ => panic!("Tried to match type of import arrow, got unknown type: {}", inp)
	    }
	}
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub(crate) enum EbnfType {
	Some,
	Many,
	Maybe
}

impl TypeDetect for EbnfType {
	fn detect(inp: &str) -> Self {
	    match inp.chars().next().unwrap() {
	    	'+' => EbnfType::Some,
	    	'*' => EbnfType::Many,
	    	'?' => EbnfType::Maybe,
	    	_ => panic!("Tried to match type of ebnf expansion, got unknown type: {}", inp)
	    }
	}
}

#[inherit_from_base]
pub(crate) enum Productions {

	#[token("->")]
	Produce,

	#[token("=>")]
	Generate,

	#[token("::")]
	Colons,

	#[token("|")]
	Alternative,

	#[token("&")]
	Additional,

	#[token("include")]
	Include,

	#[regex("<(-|=|<|/)", |lex| ImportType::detect(lex.slice()))]
	Import(ImportType),

	#[regex(r#"/([^/\\]|\\.)*/"#, |lex| rem_first_and_last_char(lex.slice()))]
	LitRegex(String),

	#[regex("((conversational|generative)\\s+)?grammar", |lex| GrammarType::detect(lex.slice()))]
	GrammarSpec(GrammarType),

	#[regex(r#"(\*|\+|\?)"#, |lex| EbnfType::detect(lex.slice()))]
	Ebnf(EbnfType)
}

#[cfg(test)]
mod tests {
	use crate::lexer::ident::Ident;
	use crate::lexer::assert_lex;
	use std::assert_eq;

	use logos::Logos;
	use super::Productions::{self};
	use super::ImportType::*;
	use super::GrammarType::*;
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
		assert_eq!(lex.next(), Some({
		  <Productions as Logos>::Error::default();
		  Err(())
		}));

	}

	#[test]
	fn test_quoted_string_single() {
		let s = r#"'This is \' a "with double quotes inside" string \\\' '"#;
		let mut lex = Productions::lexer(s);
		assert_eq!(lex.next(), Some(Ok(Productions::LitString("This is \\' a \"with double quotes inside\" string \\\\\\' ".to_string()))));

		let s2 = r#"'This one should \\' fail' before this"#;
		let mut lex = Productions::lexer(s2);
		assert_eq!(lex.next(), Some(Ok(Productions::LitString("This one should \\\\".to_string()))));
		assert_eq!(lex.next(), Some(Ok(Productions::Identifier(Ident::Unknown("fail".to_string())))));
		assert_eq!(lex.next(), Some({
		  <Productions as Logos>::Error::default();
		  Err(())
		}));
	}

	#[test]
	fn test_identifier_matching() {
		let s = "$synthesized !inherited unknown";
		let expect = &[
			Ok(Productions::Identifier(Ident::Synth("synthesized".to_string()))),
			Ok(Productions::Identifier(Ident::Inherit("inherited".to_string()))),
			Ok(Productions::Identifier(Ident::Unknown("unknown".to_string())))
		];
		assert_lex(s, expect);
	}

	#[test]
	fn test_regex() {
		let s = r#"/[a-z][A-Z][^\/]/"#;
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
	fn test_grammar_types() {
		let s = "conversational grammar generative grammar grammar";
		let expect = &[
			Ok(Productions::GrammarSpec(Conversational)),
			Ok(Productions::GrammarSpec(Generative)),
			Ok(Productions::GrammarSpec(General))
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
}