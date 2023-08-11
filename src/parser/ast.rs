
use std::{path::PathBuf};
use crate::lexer::{productions::{GrammarType, ImportType, EbnfType, Productions}, ident::Ident, Tokens, math::Math, LexerBridge};
use wagon_macros::match_error;

trait Parse {

	fn parse(lexer: &mut LexerBridge) -> Result<Self, String> where Self: Sized;
}

macro_rules! in_enum_set {
    ($check:expr, ($($variant:path),+)) => {
        match $check {
        	$($variant(_) => true,)+
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

enum Terminal {
	Regex(String),
	LitString(String)
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

	fn parse(_: &mut LexerBridge) -> Result<Self, String> { todo!() }
}

struct Disjunct(Vec<Conjunct>);

struct Conjunct(Vec<Inverse>);

enum Inverse {
	Not(Box<Inverse>),
	Comparison(Comparison)
}

struct Comparison {
	sum: Sum,
	comp: Vec<Comp>
}

enum Comp {
	Eq(Sum),
	Neq(Sum),
	Lte(Sum),
	Lt(Sum),
	Gte(Sum),
	Gt(Sum),
	In(Sum),
	NotIn(Sum)
}

enum Sum {
	Term(Term),
	Operation {
		left: Box<Sum>,
		op: Op1,
		right: Term
	}
}

enum Op1 {
	Add,
	Sub
}

enum Term {
	Factor(Factor),
	Operation {
		left: Box<Term>,
		op: Op2,
		right: Factor
	}
}

enum Op2 {
	Mul,
	Div,
	Floor,
	Mod
}

enum Factor {
	Primary(Primary),
	Power {
		left: Primary,
		right: Box<Factor>
	}
}

enum Primary {
	Dict(Dictionary),
	Atom(Atom)
}

enum Atom {
	Ident(Ident),
	LitBool(bool),
	LitNum(i32),
	LitFloat(f32),
	LitString(String),
	Group(Expression)
}

impl Parse for Atom {

	fn parse(lexer: &mut LexerBridge) -> Result<Self, String> {
		let token: Tokens = lexer.next_unwrap();
	    match_error!(match token {
	    	#[expect("identifier")]
	        either_token!(Identifier(x)) => Ok(Self::Ident(x)),
	        Tokens::MathToken(Math::LitBool(x)) => Ok(Self::LitBool(x)),
	        Tokens::MathToken(Math::LitInt(x)) => Ok(Self::LitNum(x)),
	        Tokens::MathToken(Math::LitFloat(x)) => Ok(Self::LitFloat(x)),
	        #[expect("string")]
	        either_token!(LitString(x)) => Ok(Self::LitString(x)),
	        Tokens::MathToken(Math::LPar) => {
	        	let resp = Expression::parse(lexer)?;
	        	match lexer.next_unwrap() {
	        	    Tokens::MathToken(Math::RPar) => Ok(Self::Group(resp)),
	        	    _ => Err(format!("Expected ')', got {:?}", token))
	        	}
	        }
	    })
	}
}

struct Dictionary(Ident, Expression);

