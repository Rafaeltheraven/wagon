use logos::Logos;
use wagon_macros::inherit_from_base;
use crate::helpers::rem_first_and_last_char;

#[inherit_from_base]
enum Productions {

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
	LitRegex(String),
}

#[cfg(test)]
mod tests {
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
		assert_eq!(lex.next(), Some(Ok(Productions::Ident("fail".to_string()))));
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
		assert_eq!(lex.next(), Some(Ok(Productions::Ident("fail".to_string()))));
		assert_eq!(lex.next(), Some(Err(<Productions as Logos>::Error::default())));
	}
}