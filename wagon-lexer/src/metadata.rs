
use crate::{Logos, LexingError};
use logos_display::{Debug, Display};
use wagon_macros::inherit_from_base;

#[derive(Eq)]
#[inherit_from_base(filter(Identifier, LitString, LBr, RBr, LCur, RCur, LPar, RPar, Colon, Comma))]
/// Lexer for the Metadata DSL.
pub enum Metadata {
	#[display_override("Metadata Key")]
	#[regex(r#"[a-zA-Z][a-zA-Z0-9]*:"#, |lex| wagon_utils::rem_last_char(lex.slice()))]
	/// An identifier, followed by a `:`. 
	Key(String),

	#[display_override("Metadata Delimiter")]
	#[regex(r#"===+"#)]
	/// 3 or more `=`
	Delim,

	#[token("include")]
	/// `include`
	Include,
}

#[cfg(test)]
mod tests {
	use crate::assert_lex;
	use super::Metadata;

	#[test]
	fn test_include() {
		let s = "include some::path;";
		let s2 = "include ::another;";
		let expect = &[
			Ok(Metadata::Include),
			Ok(Metadata::Path("some::path".to_string())),
			Ok(Metadata::Semi)
		];
		let expect2 = &[
			Ok(Metadata::Include),
			Ok(Metadata::Path("::another".to_string())),
			Ok(Metadata::Semi)
		];
		assert_lex(s, expect);
		assert_lex(s2, expect2);
	}

}