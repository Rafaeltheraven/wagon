use super::{TypeDetect, LexingError};
use logos::Logos;
use wagon_macros::inherit_from_base;
use logos_display::{Debug, Display};

use wagon_utils::{rem_first_and_last_char, rem_first_char};

#[inherit_from_base]
/// Lexer for the Math DSL.
pub enum Math {
	#[token("!")]
	/// `!`
	Not,

	#[token("&&")]
	/// `&&`
	And,

	#[token("||")]
	/// `||`
	Or,

	#[token("**")]
	/// `**`
	Pow,

	#[token("*")]
	/// `*`
	Mul,

	#[token("/")]
	/// `/`
	Div,

	#[token("//")]
	/// `//`
	Floor,

	#[token("%")]
	/// `%`
	Mod,

	#[token("+")]
	/// `+`
	Add,

	#[token("-")]
	/// `-`
	Sub,

	#[token("==")]
	/// `==`
	Eq,

	#[token("!=")]
	/// `!=`
	Neq,

	#[token("<=")]
	/// `<=`
	Lte,

	#[token(">=")]
	/// `>=`
	Gte,

	#[token("in")]
	/// `in`
	In,

	#[token("if")]
	/// `if`
	If,

	#[token("then")]
	/// `then`
	Then,

	#[token("else")]
	/// `else`
	Else,

	#[token("=")]
	/// `=`
	Assigns,

	#[display_override("Subprocess")]
	#[regex(r#"\$\(([^\)\\]|\\.)*\)"#, |lex| rem_first_and_last_char(&rem_first_char(lex.slice())))]
	/// A bash style subprocess. Defined as any string surrounded by `$()`
	Bash(String),

	#[token("false", |_| false)]
    #[token("true", |_| true)]
    /// Either `true` or `false`
    LitBool(bool),

    #[display_override("Integer")]
    #[regex("(-?[0-9]+)", |lex| lex.slice().parse::<i32>().map_err(|x| LexingError::ParseIntError(x, lex.span())))]
    /// Any integer
	LitInt(i32),

	#[display_override("Float")]
	#[regex("(-?[0-9]+\\.[0-9]+)", |lex| lex.slice().parse::<f32>().map_err(|x| LexingError::ParseFloatError(x, lex.span())))]
	/// Any float
	LitFloat(f32),
}

#[cfg(test)]
mod tests {

	
	use wagon_ident::Ident;
	use crate::assert_lex;
	use super::Math::{*};

	#[test]
	fn test_pure_math() {
		let s = "5*7.43**2 + (3 % -2)";
		let expect = &[
			Ok(LitInt(5)),
			Ok(Mul),
			Ok(LitFloat(7.43)),
			Ok(Pow), 
			Ok(LitInt(2)),
			Ok(Add), 
			Ok(LPar),
			Ok(LitInt(3)),
			Ok(Mod), 
			Ok(LitInt(-2)),
			Ok(RPar),
		];
		assert_lex(s, expect);
	}

	#[test]
	fn test_pure_bool() {
		let s = "!(true && false || x)";
		let expect = &[
			Ok(Not),
			Ok(LPar),
			Ok(LitBool(true)),
			Ok(And),
			Ok(LitBool(false)),
			Ok(Or),
			Ok(Identifier(Ident::Unknown("x".to_string()))),
			Ok(RPar),
		];
		assert_lex(s, expect);
	}

	#[test]
	fn test_complex_combination() {
		let s = "0.5 * ((2 + 3 < 4 + x) && (x != 2) || '4' in y)";
		let expect = &[
			Ok(LitFloat(0.5)),
			Ok(Mul),
			Ok(LPar),
			Ok(LPar),
			Ok(LitInt(2)),
			Ok(Add),
			Ok(LitInt(3)),
			Ok(Lt),
			Ok(LitInt(4)),
			Ok(Add),
			Ok(Identifier(Ident::Unknown("x".to_string()))),
			Ok(RPar),
			Ok(And),
			Ok(LPar),
			Ok(Identifier(Ident::Unknown("x".to_string()))),
			Ok(Neq),
			Ok(LitInt(2)),
			Ok(RPar),
			Ok(Or),
			Ok(LitString("4".to_string())),
			Ok(In),
			Ok(Identifier(Ident::Unknown("y".to_string()))),
			Ok(RPar),
		];
		assert_lex(s, expect);
	}

}