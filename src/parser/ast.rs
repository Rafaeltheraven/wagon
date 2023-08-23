
use std::{path::PathBuf};
use crate::lexer::UnsafePeek;
use crate::lexer::{productions::{GrammarType, ImportType, EbnfType, Productions}, ident::Ident, Tokens, math::Math, PeekLexer, UnsafeNext};
use wagon_macros::{match_error, TokenMapper};

trait Parse {

	fn parse(lexer: &mut PeekLexer) -> Result<Self, String> where Self: Sized;

	fn parse_sep(lexer: &mut PeekLexer, join: Tokens) -> Result<Vec<Self>, String> where Self: Sized {
		let mut res = Vec::new();
		res.push(Self::parse(lexer)?);
		while lexer.peek_unwrap() == &join {
			lexer.next();
			res.push(Self::parse(lexer)?)
		}
		Ok(res)
	}
}

trait ParseOption {

	fn parse_option(lexer: &mut PeekLexer) -> Result<Option<Self>, String> where Self: Sized;
}

trait TokenMapper {
	fn token_to_enum(token: &Tokens) -> Option<Self> where Self: Sized;
}

fn __between<T: Parse>(lexer: &mut PeekLexer, right: Tokens) -> Result<T, String> {
	let resp = T::parse(lexer)?;
	let token = lexer.next_unwrap();
	if token == right {
		Ok(resp)
	} else {
		Err(format!("Expected {:?}, got {:?}", right, token))
	}
}

fn between<T: Parse>(lexer: &mut PeekLexer, left: Tokens, right: Tokens) -> Result<T, String> {
	let token = lexer.peek_unwrap();
	if token == &left {
		__between(lexer, right)
	} else {
		Err(format!("Expected {:?}, got {:?}", left, token))
	}
}

macro_rules! in_enum_set {
    ($check:expr, ($($variant:path),+)) => {
        match $check {
        	$($variant => true,)+
        	_ => false
        }
    };
}

macro_rules! either_token {
    ($variant:ident($($arg:tt)*)) => {
        Tokens::ProductionToken(Productions::$variant($($arg)*)) | Tokens::MathToken(Math::$variant($($arg)*))
    };
}

pub(crate) struct Wag {
	metadata: Metadata,
	grammar: Vec<Rule>,
}

struct Metadata {
	includes: Vec<PathBuf>,
	spec: Option<GrammarType>
}

enum Rule {
	General(String, Vec<Rhs>),
	Generate(String, Vec<Rhs>),
	Import(ImportType, String)
}

struct Rhs {
	weight: Option<Expression>,
	chunks: Vec<Chunk>,
	block: Vec<Assignment>,
	label: Option<String>
}

struct Assignment {
	ident: Ident,
	expr: Expression
}

struct Chunk {
	symbols: Vec<Symbol>,
	ebnf: Option<EbnfType>
}



enum Symbol {
	NonTerminal(Ident),
	Terminal(Terminal)
}

impl Parse for Symbol {
    fn parse(lexer: &mut PeekLexer) -> Result<Self, String> where Self: Sized {
        match lexer.peek_unwrap() {
        	Tokens::ProductionToken(Productions::Identifier(_)) => {
        		if let Tokens::ProductionToken(Productions::Identifier(x)) = std::mem::take(&mut lexer.next_unwrap()) {
        			Ok(Self::NonTerminal(x))
        		}
        		else { 
        			Err(format!("{:?}", "Something went terribly wrong, unwrapped non-identifier when should have unwrapped identifier"))  
        		}
        	},
        	_ => Ok(Self::Terminal(Terminal::parse(lexer)?))
        }
    }
}

enum Terminal {
	Regex(String),
	LitString(String)
}

impl Parse for Terminal {
    fn parse(lexer: &mut PeekLexer) -> Result<Self, String> where Self: Sized {
        match_error!(match lexer.next_unwrap() {
        	Tokens::ProductionToken(Productions::LitString(x)) => Ok(Self::LitString(x)),
        	Tokens::ProductionToken(Productions::LitRegex(x)) => Ok(Self::Regex(x)),
        })
    }
}

enum Expression {
	Subproc(String),
	If {
		this: Disjunct,
		then: Disjunct,
		r#else: Option<Box<Expression>>
	},
	Disjunct(Disjunct),
}

impl Parse for Expression {

	fn parse(lexer: &mut PeekLexer) -> Result<Self, String> { 
		match lexer.peek_unwrap() {
			Tokens::MathToken(Math::If) => {lexer.next(); Self::parse_if(lexer)},
			Tokens::MathToken(Math::Bash(expr)) => {let resp = Ok(Self::Subproc(expr.to_string())); lexer.next(); resp},
			_ => Ok(Self::Disjunct(Disjunct::parse(lexer)?))
		}
	}
}

impl Expression {

	fn parse_if(lexer: &mut PeekLexer) -> Result<Self, String> {
		let this = Disjunct::parse(lexer)?;
		let then = match_error!(match lexer.next_unwrap() {
			Tokens::MathToken(Math::Then) => Disjunct::parse(lexer)
		})?;
		let r#else = match lexer.peek_unwrap() {
		    Tokens::MathToken(Math::Else) => {lexer.next(); Some(Box::new(Expression::parse(lexer)?))},
		    _ => None
		};
		Ok(Self::If { this: this, then: then, r#else: r#else })
	}

}

struct Disjunct(Vec<Conjunct>);

impl Parse for Disjunct {
    
    fn parse(lexer: &mut PeekLexer) -> Result<Self, String> where Self: Sized {
        Ok(Self(Conjunct::parse_sep(lexer, Tokens::MathToken(Math::And))?))
    }

}

struct Conjunct(Vec<Inverse>);

impl Parse for Conjunct {
    
    fn parse(lexer: &mut PeekLexer) -> Result<Self, String> where Self: Sized {
        Ok(Self(Inverse::parse_sep(lexer, Tokens::MathToken(Math::Or))?))
    }

}

enum Inverse {
	Not(Box<Inverse>),
	Comparison(Comparison)
}

impl Parse for Inverse {

    fn parse(lexer: &mut PeekLexer) -> Result<Self, String> where Self: Sized {
        match lexer.peek_unwrap() {
        	Tokens::MathToken(Math::Not) => {lexer.next(); Ok(Self::Not(Box::new(Self::parse(lexer)?)))},
        	_ => Ok(Self::Comparison(Comparison::parse(lexer)?))
        }
    }

}

struct Comparison {
	sum: Sum,
	comp: Option<Comp>
}

struct Comp {
	op: CompOp,
	right: Sum
}

#[derive(TokenMapper)]
enum CompOp {
	Eq,
	Neq,
	Lte,
	Lt,
	Gte,
	Gt,
	In
}

impl Parse for Comparison {

    fn parse(lexer: &mut PeekLexer) -> Result<Self, String> where Self: Sized {
        Ok(Self { sum: Sum::parse(lexer)?, comp: Comp::parse_option(lexer)? })
    }

}

impl ParseOption for Comp {

    fn parse_option(lexer: &mut PeekLexer) -> Result<Option<Self>, String> where Self: Sized {
        if let Some(op) = CompOp::token_to_enum(lexer.peek_unwrap()) {
        	lexer.next();
        	Ok(Some(Self { op: op, right: Sum::parse(lexer)?}))
        } else {
        	Ok(None)
        }
    }

}

struct Sum {
	left: Term,
	cont: Option<SumP>
}

struct SumP {
	op: Op1,
	right: Term,
	cont: Box<Option<SumP>>
}

impl Parse for Sum {

	fn parse(lexer: &mut PeekLexer) -> Result<Self, String> {
		Ok(Self {
			left: Term::parse(lexer)?,
			cont: SumP::parse_option(lexer)?
		})
	}
}

impl ParseOption for SumP {

	fn parse_option(lexer: &mut PeekLexer) -> Result<Option<Self>, String> where Self: Sized {
	    if let Some(op) = Op1::token_to_enum(lexer.peek_unwrap()) {
	    	lexer.next();
	    	Ok(Some(SumP { op: op, right: Term::parse(lexer)?, cont: Box::new(SumP::parse_option(lexer)?) }))
	    } else {
	    	Ok(None)
	    }
	}
}

#[derive(TokenMapper)]
enum Op1 {
	Add,
	Sub
}

/*
Term -> Term Op Factor | Factor
|
V
Term -> Factor Term'
Term' -> Op Factor Term' | epsilon
*/

struct Term {
	left: Factor,
	cont: Option<TermP>
}

struct TermP {
	op: Op2,
	right: Factor,
	cont: Box<Option<TermP>>
}

impl Parse for Term {

	fn parse(lexer: &mut PeekLexer) -> Result<Self, String> {
		Ok(Self {
			left: Factor::parse(lexer)?,
			cont: TermP::parse_option(lexer)?
		})
	}
}

impl ParseOption for TermP {

	fn parse_option(lexer: &mut PeekLexer) -> Result<Option<Self>, String> where Self: Sized {
	    if let Some(op) = Op2::token_to_enum(lexer.peek_unwrap()) {
	    	lexer.next();
	    	Ok(Some(TermP { op: op, right: Factor::parse(lexer)?, cont: Box::new(TermP::parse_option(lexer)?) }))
	    } else {
	    	Ok(None)
	    }
	}
}

#[derive(TokenMapper)]
enum Op2 {
	Mul,
	Div,
	Floor,
	Mod
}

enum Factor {
	Primary(Atom),
	Power {
		left: Atom,
		right: Box<Factor>
	}
}

impl Parse for Factor {

	fn parse(lexer: &mut PeekLexer) -> Result<Self, String> {
		let left = Atom::parse(lexer)?;
		if &Tokens::MathToken(Math::Pow) == lexer.peek_unwrap() {
			lexer.next();
			Ok(
				Factor::Power {
					left: left, 
					right: Box::new(Self::parse(lexer)?)
				}
			)
		} else {
			Ok(Factor::Primary(left))
		}
	}
}

struct Dictionary(Ident, Expression);

enum Atom {
	Ident(Ident),
	Dict(Dictionary),
	LitBool(bool),
	LitNum(i32),
	LitFloat(f32),
	LitString(String),
	Group(Expression)
}

impl Parse for Atom {

	fn parse(lexer: &mut PeekLexer) -> Result<Self, String> {
	    match_error!(match lexer.next_unwrap() {
	    	#[expect("identifier or dictionary")]
	        either_token!(Identifier(x)) => {
	        	if let Ok(inner) = between(lexer, Tokens::MathToken(Math::LBr), Tokens::MathToken(Math::RBr)) {
	        		Ok(Self::Dict(Dictionary(x, inner)))
	        	} else {
	        		Ok(Self::Ident(x))
	        	}
	        },
	        Tokens::MathToken(Math::LitBool(x)) => Ok(Self::LitBool(x)),
	        Tokens::MathToken(Math::LitInt(x)) => Ok(Self::LitNum(x)),
	        Tokens::MathToken(Math::LitFloat(x)) => Ok(Self::LitFloat(x)),
	        #[expect("string")]
	        either_token!(LitString(x)) => Ok(Self::LitString(x)),
	        Tokens::MathToken(Math::LPar) => {
	        	let resp = __between(lexer, Tokens::MathToken(Math::RPar))?;
	        	Ok(Self::Group(resp))
	        },
	    })
	}
}
