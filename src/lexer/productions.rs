use logos::Logos;
use wagon_macros::inherit_from_base;
use crate::helpers::rem_first_and_last_char;
use super::ident::{Ident, detect_ident_type};

#[inherit_from_base]
pub(crate) enum Productions {

	#[token("->")]
	Produce,

	#[token("=>")]
	Generate,

	#[token("<-")]
	ImportBasic,

	#[token("<=")]
	ImportFull,

	#[token("<<")]
	ImportRecursive,

	#[token("|")]
	Alternative,

	#[regex("/([^/\\\\]|\\\\.)*/", |lex| rem_first_and_last_char(lex.slice()))]
	LitRegex(String)
}

#[cfg(test)]
mod tests {
	use crate::lexer::ident::Ident;
	use crate::helpers::assert_lex;
	use std::assert_eq;

	use logos::Logos;
	use super::Productions::{self};

	#[test]
	fn test_quoted_string_double() {
		let s = r#""This is \" a 'with single quotes inside' string \\\" ""#;
		let mut lex = Productions::lexer(s);
		assert_eq!(lex.next(), Some(Ok(Productions::LitString("This is \\\" a 'with single quotes inside' string \\\\\\\" ".to_string()))));

		let s2 = r#""This one should \\" fail" before this"#;
		let mut lex = Productions::lexer(s2);
		assert_eq!(lex.next(), Some(Ok(Productions::LitString("This one should \\\\".to_string()))));
		assert_eq!(lex.next(), Some(Ok(Productions::Identifier(Ident::Unknown("fail".to_string())))));
		assert_eq!(lex.next(), Some(Err(<Productions as Logos>::Error::default())));

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
		assert_eq!(lex.next(), Some(Err(<Productions as Logos>::Error::default())));
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
}