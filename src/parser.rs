#[macro_use]
pub(crate) mod wag;
pub(crate) mod ast;

mod assignment;
mod atom;
mod chunk;
mod comp;
mod conjunct;
mod disjunct;
mod expression;
mod factor;
mod helpers;
mod inverse;
mod metadata;
mod rhs;
mod rule;
mod sum;
mod symbol;
mod term;
mod terminal;

use std::{error::Error, fmt::Display};
use logos::Span;
use crate::{lexer::{LexerBridge, PeekLexer, Tokens, UnsafeNext, Spannable}, helpers::comma_separated_with_or};
use crate::string_vec;
use crate::helpers::peekable::Peekable;
use self::wag::Wag;

pub struct Parser<'source> {
	lexer: PeekLexer<'source>
}

impl<'source> Parser<'source> {
	pub fn new(data: &'source str) -> Self {
		Self {
			lexer: Peekable::new(LexerBridge::new(data))
		}
	}

	pub(crate) fn parse(&mut self) -> ParseResult<Wag> {
		Wag::parse(&mut self.lexer)
	}
}

type ParseResult<T> = Result<T, WagParseError>;

#[derive(PartialEq, Debug)]
pub(crate) enum WagParseError {
	Unexpected {
		span: Span,
		offender: Tokens,
		expected: Vec<String>
	},
	Fatal((Span, String))
}

impl Error for WagParseError {}
impl Display for WagParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    	match self {
	        WagParseError::Unexpected { span: _, offender, expected } => write!{f, "unexpected token {:?}, expected {:#?}", offender, comma_separated_with_or(expected)},
	        WagParseError::Fatal((_, msg)) => write!(f, "Fatal exception: {:?}", msg),
    	}
    }
}

trait Parse {

	fn parse(lexer: &mut PeekLexer) -> ParseResult<Self> where Self: Sized;

	fn parse_sep(lexer: &mut PeekLexer, join: Tokens) -> ParseResult<Vec<Self>> where Self: Sized {
		let mut res = Vec::new();
		res.push(Self::parse(lexer)?);
		while lexer.next_if(|x| x.as_ref() == Ok(&join)).is_some() {
			res.push(Self::parse(lexer)?)
		}
		Ok(res)
	}

	fn parse_sep_end(lexer: &mut PeekLexer, join: Tokens, end: Tokens) -> ParseResult<Vec<Self>> where Self: Sized {
		let mut res = Vec::new();
		res.push(Self::parse(lexer)?);
		let mut done = false;
		while !done {
			if lexer.next_if(|x| x.as_ref() == Ok(&join)).is_some() {
				if lexer.next_if(|x| x.as_ref() == Ok(&end)).is_some() {
					done = true;
				} else {
					res.push(Self::parse(lexer)?);
				}
			} else if lexer.next_if(|x| x.as_ref() == Ok(&end)).is_some() {
				done = true;
			} else {
				return Err(WagParseError::Unexpected{ offender: lexer.next_unwrap(), expected: string_vec![join, end], span: lexer.span()})
			}
		}
		Ok(res)
	}
}

trait ParseOption {

	fn parse_option(lexer: &mut PeekLexer) -> ParseResult<Option<Self>> where Self: Sized;
}

#[cfg(test)]
mod tests {

    use super::assignment::Assignment;
    use super::atom::Atom;
    use super::comp::Comparison;
    use super::conjunct::Conjunct;
    use super::disjunct::Disjunct;
    use super::expression::Expression;
    use super::factor::Factor;
    use super::inverse::Inverse;
    use super::term::Term;
    use super::{Parser, Wag, metadata::Metadata, rule::Rule, rhs::Rhs, chunk::Chunk, symbol::Symbol, terminal::Terminal, sum::Sum};
    use crate::lexer::Tokens;
    use crate::lexer::productions::GrammarType;
    use crate::lexer::ident::Ident;
    use crate::string_vec;

    use logos::Span;
    use pretty_assertions::assert_eq;

    #[test]
	fn test_example_wag() {
		let input = r#"
		include activities::other;

		conversational grammar;

		start -> setup activity* 'stop'; /* a comment */
		setup -> greet? getname;
		greet -> ('hello' {hello = true;})+ | "good morning";
		greet => 'greetings human!' 
		| [0.3] "What is your name? ";
		"#;
		let mut parser = Parser::new(input);
		let output = parser.parse();
		let expected = Wag { 
			metadata: Metadata {
				includes: vec!["activities::other".to_string()],
				spec: Some(GrammarType::Conversational)
			}, 
			grammar: vec![
				Rule::Analytic("start".to_string(), vec![
					Rhs { 
						weight: None, 
						chunks: vec![
							Chunk { 
								symbols: vec![
									Symbol::NonTerminal(Ident::Unknown("setup".to_string()))
								],
								ebnf: None 
							},
							Chunk {
								symbols: vec![
									Symbol::NonTerminal(Ident::Unknown("activity".to_string()))
								],
								ebnf: Some(crate::lexer::productions::EbnfType::Many)
							},
							Chunk {
								symbols: vec![
									Symbol::Terminal(Terminal::LitString("stop".to_string()))
								],
								ebnf: None
							}
						]
					}
				]),
				Rule::Analytic("setup".to_string(), vec![
					Rhs {
						weight: None,
						chunks: vec![
							Chunk {
								symbols: vec![
									Symbol::NonTerminal(Ident::Unknown("greet".to_string()))
								],
								ebnf: Some(crate::lexer::productions::EbnfType::Maybe)
							},
							Chunk {
								symbols: vec![
									Symbol::NonTerminal(Ident::Unknown("getname".to_string()))
								],
								ebnf: None
							}
						]
					}
				]),
				Rule::Analytic("greet".to_string(), vec![
					Rhs {
						weight: None,
						chunks: vec![
							Chunk {
								symbols: vec![
									Symbol::Terminal(Terminal::LitString("hello".to_string())),
									Symbol::Assignment(vec![
										Assignment { 
											ident: Ident::Unknown("hello".to_string()), 
											expr: Expression::Disjunct(
												Disjunct(vec![
													Conjunct(vec![
														Inverse::Comparison(
															Comparison {
																sum: Sum {
																	left: Term { 
																		left: Factor::Primary(
																			Atom::LitBool(true)
																		), 
																		cont: None 
																	},
																	cont: None
																},
																comp: None
															}
														)
													])
												])
											)
										}
									])
								],
								ebnf: Some(crate::lexer::productions::EbnfType::Some)
							}
						]
					},
					Rhs {
						weight: None,
						chunks: vec![
							Chunk {
								symbols: vec![
									Symbol::Terminal(Terminal::LitString("good morning".to_string()))
								],
								ebnf: None
							}
						]
					}
				]),
				Rule::Generate("greet".to_string(), vec![
					Rhs {
						weight: None,
						chunks: vec![
							Chunk {
								symbols: vec![
									Symbol::Terminal(Terminal::LitString("greetings human!".to_string()))
								],
								ebnf: None
							}
						]
					},
					Rhs {
						weight: Some(
							Expression::Disjunct(
								Disjunct(vec![
									Conjunct(vec![
										Inverse::Comparison(
											Comparison {
												sum: Sum {
													left: Term { 
														left: Factor::Primary(
															Atom::LitFloat(0.3)
														), 
														cont: None 
													},
													cont: None
												},
												comp: None
											}
										)
									])
								])
							)
						),
						chunks: vec![
							Chunk {
								symbols: vec![
									Symbol::Terminal(Terminal::LitString("What is your name? ".to_string()))
								],
								ebnf: None
							}
						]
					}
				])
			]
		};
		assert_eq!(Ok(expected), output);
	}

	#[test]
	fn test_parse_error() {
		let input = r#"
		include activities::other;

		conversational grammar

		start -> setup activity* 'stop'; /* a comment */
		setup -> greet? getname;
		greet -> ('hello' {hello = true;})+ | "good morning";
		greet => 'greetings human!' 
		| [0.3] "What is your name? ";
		"#;
		let mut parser = Parser::new(input);
		let output = parser.parse();
		let expected = Err(crate::parser::WagParseError::Unexpected { 
			span: Span {start: 59, end: 64}, 
			offender: Tokens::ProductionToken(crate::lexer::productions::Productions::Identifier(Ident::Unknown("start".to_string()))), 
			expected: string_vec![
				Tokens::ProductionToken(crate::lexer::productions::Productions::Semi)
			]
		});
		assert_eq!(expected, output);
	}

}