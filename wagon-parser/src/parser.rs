#[macro_use]
/// The full WAG tree.
pub mod wag;
/// An assignment.
pub mod assignment;
/// An atom.
pub mod atom;
/// A chunk.
pub mod chunk;
/// A comparison.
pub mod comp;
/// `||`
pub mod conjunct;
/// `&&`
pub mod disjunct;
/// An expression.
pub mod expression;
/// `**`
pub mod factor;
/// Helper methods.
pub mod helpers;
/// `!`
pub mod inverse;
/// Metadata for the WAG.
pub mod metadata;
/// The right-hand-side of a rule.
pub mod rhs;
/// A rule.
pub mod rule;
/// `+` or `-`.
pub mod sum;
/// An individual symbol.
pub mod symbol;
/// `*` or `/`.
pub mod term;
/// A terminal.
pub mod terminal;
mod ident;

use std::{error::Error, fmt::Display, write};
use self::wag::Wag;
use crate::firstpass::{WagCheckError, Rewrite};
use crate::{SpannableNode, MsgAndSpan};

use ordered_float::FloatIsNan;
use wagon_ident::Ident;
use wagon_utils::{Peek, comma_separated_with_or, string_vec, ResultNext, ResultPeek};
use wagon_lexer::{LexerBridge, Tokens, Spannable, Span, LexingError};

/// The main parser struct.
///
/// Uses a [`LexerBridge`] internally.
/// # Example
/// ```
/// use wagon_parser::parser::Parser;
/// 
/// let s = "S -> A;";
/// let mut parser = Parser::new(s);
/// assert!(parser.parse().is_ok())
pub struct Parser<'source> {
	lexer: LexerBridge<'source>
}

impl<'source> Parser<'source> {
	/// Given an input string, construct a parser.
	#[must_use] pub fn new(data: &'source str) -> Self {
		Self {
			lexer: LexerBridge::new(data)
		}
	}

	/// Start parsing and return a result.
	///
	/// # Errors
	/// Returns a [`WagParseError`] if any error occurs during parsing.
	pub fn parse(&mut self) -> ParseResult<Wag> {
		Wag::parse(&mut self.lexer)
	}
}

/// Any parse will either return the node we are trying to parse, or a [`WagParseError`].
pub type ParseResult<T> = Result<T, WagParseError>;

#[derive(PartialEq, Debug)]
/// Any of the various errors that can occur during parsing.
pub enum WagParseError {
	/// An unexpected character was encountered.
	Unexpected {
		/// The span info for this character.
		span: Span,
		/// The token we found.
		offender: Tokens,
		/// String representations for the tokens we expected to see.
		expected: Vec<String>
	},
	/// Something horrible happened that we do not have a specific error for.
	Fatal((Span, String)),
	/// A wrapper around [`WagCheckError`].
	CheckError(WagCheckError),
	/// A wrapper around [`LexingError`].
	LexError(LexingError),
	/// Expected a float but got a NaN
	FloatError(FloatIsNan, Span),
	/// Non-valid regex
	RegexError(Box<regex_syntax::Error>, Span), // Regex errors are big so we're allocating it on the heap
}

impl From<WagCheckError> for WagParseError {
    fn from(value: WagCheckError) -> Self {
        Self::CheckError(value)
    }
}

impl From<LexingError> for WagParseError {
	fn from(value: LexingError) -> Self {
		Self::LexError(value)
	}
}

impl Error for WagParseError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::Fatal(_) | Self::Unexpected { .. } => None,
            Self::CheckError(e) => Some(e),
            Self::LexError(e) => Some(e),
            Self::FloatError(e, _) => Some(e),
            Self::RegexError(e, _) => Some(e),
        }
    }
}
impl Display for WagParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    	let (head, msg) = self.msg();
    	write!(f, "{head}: {msg}")
    }
}

impl MsgAndSpan for WagParseError {
	fn span(self) -> Span {
		match self {
		    Self::CheckError(check) => check.span(),
		    Self::LexError(lex) => lex.span(),
		    Self::FloatError(_, span) | Self::Fatal((span, _)) | Self::Unexpected { span, .. } | Self::RegexError(_, span) => span,
		}
	}

	fn msg(&self) -> (String, String) {
		match self {
		    Self::Unexpected { span, offender, expected } => ("Unexpected Token".to_string(), 
		    	format!("Encountered token {:?} at position {:?}. Expected {:#?}", offender, span, comma_separated_with_or(expected))),
	        Self::Fatal((_, msg)) => ("Fatal Exception".to_string(), msg.to_string()),
	        Self::CheckError(err) => err.msg(),
    		Self::LexError(lex) => ("Lexing Error".to_string(), lex.to_string()),
    		Self::FloatError(e, _) => ("Error converting floating point".to_string(), e.to_string()),
    		Self::RegexError(e, _) => ("Regex Parse Error".to_string(), e.to_string()),
		}
	}
}

/// The main trait for parsing.
///
/// Any node that can be parsed must implement this trait.
pub trait Parse {

	/// Given a lexer, try to parse a valid instance of this node.
	///
	/// # Errors
	/// Should return a [`WagParseError`] if the parsing fails.
	fn parse(lexer: &mut LexerBridge) -> ParseResult<Self> where Self: Sized;

	/// Parse multiple instances of this node, separated by a [`Tokens`].
	///
	/// # Errors
	/// Should return a [`WagParseError`] if the parsing fails.
	fn parse_sep(lexer: &mut LexerBridge, join: Tokens) -> ParseResult<Vec<Self>> where Self: Sized {
		let mut res = Vec::new();
		res.push(Self::parse(lexer)?);
		while lexer.next_if(|x| x.as_ref() == Ok(&join)).is_some() {
			res.push(Self::parse(lexer)?);
		}
		Ok(res)
	}

	/// Parse multiple instances of this node, separated by a [`Tokens`] end ended by a (possibly different) [`Tokens`].
	///
	/// # Errors
	/// Should return a [`WagParseError`] if the parsing fails.
	fn parse_sep_end(lexer: &mut LexerBridge, join: Tokens, end: Tokens) -> ParseResult<Vec<Self>> where Self: Sized {
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
				return Err(WagParseError::Unexpected{ offender: lexer.next_result()?, expected: string_vec![join, end], span: lexer.span()})
			}
		}
		Ok(res)
	}
}

/// Optionally parse the node.
///
/// Sometimes, we want to try parsing a node, but don't care if we fail in some ways, but do care in others. 
/// In that case, we should implement this trait and return `Ok(None)` if the failure doesn't matter and `Err` if it does.
trait ParseOption {

	fn parse_option(lexer: &mut LexerBridge) -> ParseResult<Option<Self>> where Self: Sized;
}

#[cfg(test)]
mod tests {

    use wagon_lexer::math::Math;
	use std::collections::BTreeMap;

    use wagon_lexer::productions::EbnfType;
	use crate::firstpass::FirstPassState;
	use super::Parse;
	use super::LexerBridge;
	use super::sum::SumP;
	use ordered_float::NotNan;
	use wagon_macros::unspanned_tree;
	use super::Rewrite;
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
    use wagon_lexer::Tokens;
    use wagon_ident::Ident;
    use super::chunk::ChunkP;
    use super::string_vec;

    use wagon_lexer::Span;
    use pretty_assertions::assert_eq;

    #[test]
    #[allow(clippy::too_many_lines)]
	fn test_example_wag() {
		let input = r#"
		include activities::other;
		type: "conversational";
		====================

		start -> setup activity* 'stop'; /* a comment */
		setup -> greet? getname | ;
		greet -> ('hello' {hello = true;})+ | "good morning";
		greet => 'greetings human!' 
		| [0.3] "What is your name? ";
		getname -> ;
		"#;
		let mut lexer = LexerBridge::new(input);
		let output = Wag::parse(&mut lexer);
		let expected = unspanned_tree!(Wag { 
			metadata: Metadata {
				includes: vec!["activities::other".to_string()],
				mappings: BTreeMap::from([("type".to_string(), Atom::LitString("conversational".to_string()))])
			}, 
			grammar: vec![
				Rule::Analytic("start".to_string(), Vec::new(), vec![
					Rhs { 
						weight: None,
						chunks: vec![
							Chunk { 
								chunk: ChunkP::Unit(Symbol::simple_ident("setup")),
								ebnf: None 
							},
							Chunk {
								chunk: ChunkP::Unit(Symbol::simple_ident("activity")),
								ebnf: Some(EbnfType::Many)
							},
							Chunk {
								chunk: ChunkP::Unit(Symbol::simple_terminal("stop")),
								ebnf: None
							}
						]
					}
				]),
				Rule::Analytic("setup".to_string(), Vec::new(), vec![
					Rhs {
						weight: None,
						chunks: vec![
							Chunk {
								chunk: ChunkP::Unit(Symbol::simple_ident("greet")),
								ebnf: Some(EbnfType::Maybe)
							},
							Chunk {
								chunk: ChunkP::Unit(Symbol::simple_ident("getname")),
								ebnf: None
							}
						]
					},
					Rhs::empty()
				]),
				Rule::Analytic("greet".to_string(), Vec::new(), vec![
					Rhs {
						weight: None,
						chunks: vec![
							Chunk {
								chunk: ChunkP::Group(vec![
									Chunk { chunk: ChunkP::Unit(Symbol::Terminal(Terminal::LitString("hello".to_string()))), ebnf: None },
									Chunk { chunk: ChunkP::Unit(Symbol::Assignment(vec![
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
									])), ebnf: None
									}
								]),
								ebnf: Some(EbnfType::Some)
							}
						]
					},
					Rhs {
						weight: None,
						chunks: vec![
							Chunk {
								chunk: ChunkP::Unit(Symbol::Terminal(Terminal::LitString("good morning".to_string()))),
								ebnf: None
							}
						]
					}
				]),
				Rule::Generate("greet".to_string(), Vec::new(), vec![
					Rhs {
						weight: None,
						chunks: vec![
							Chunk {
								chunk: ChunkP::Unit(Symbol::Terminal(Terminal::LitString("greetings human!".to_string()))),
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
															Atom::LitFloat(NotNan::new(0.3).unwrap())
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
								chunk: ChunkP::Unit(Symbol::Terminal(Terminal::LitString("What is your name? ".to_string()))),
								ebnf: None
							}
						]
					}
				]),
				Rule::Analytic("getname".to_string(), Vec::new(), vec![
					Rhs::empty()
				])
			]
		});
		assert_eq!(Ok(expected), output);
	}

	#[test]
	#[allow(clippy::too_many_lines)]
	fn test_example_wag2() {
		let input = r"
		S -> {$x = 0; $y = 0;} X<$x, $y>;
		X<*y, &x> -> 'a' {*y = *y + 1; &x = &x + 1;} B;
		B -> 'b';
		";
		let mut lexer = LexerBridge::new(input);
		let output = Wag::parse(&mut lexer);
		let expected = unspanned_tree!(Wag {
			metadata: Metadata { includes: vec![], mappings: BTreeMap::new() },
			grammar: vec![
				Rule::Analytic("S".to_string(), Vec::new(), vec![
					Rhs {
						weight: None,
						chunks: vec![
							Chunk { 
								chunk: ChunkP::Unit(Symbol::Assignment(vec![
									Assignment { 
										ident: Ident::Local("x".to_string()), 
										expr: Expression::Disjunct(
											Disjunct(vec![
												Conjunct(vec![
													Inverse::Comparison(
														Comparison {
															sum: Sum {
																left: Term { 
																	left: Factor::Primary(
																		Atom::LitNum(0)
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
									},
									Assignment { 
										ident: Ident::Local("y".to_string()), 
										expr: Expression::Disjunct(
											Disjunct(vec![
												Conjunct(vec![
													Inverse::Comparison(
														Comparison {
															sum: Sum {
																left: Term { 
																	left: Factor::Primary(
																		Atom::LitNum(0)
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
								])),
								ebnf: None, 
							},
							Chunk {
								chunk: ChunkP::Unit(Symbol::NonTerminal(Ident::Unknown("X".to_string()), vec![Ident::Local("x".to_string()), Ident::Local("y".to_string())])),
								ebnf: None,
							}
						]
					}
				]),
				Rule::Analytic("X".to_string(), vec![Ident::Inherit("y".to_string()), Ident::Synth("x".to_string())], vec![
					Rhs { 
						weight: None,
						chunks: vec![
							Chunk::simple_terminal("a"),
							Chunk { 
								chunk: ChunkP::Unit(Symbol::Assignment(vec![
									Assignment { 
										ident: Ident::Inherit("y".to_string()), 
										expr: Expression::Disjunct(
											Disjunct(vec![
												Conjunct(vec![
													Inverse::Comparison(
														Comparison {
															sum: Sum {
																left: Term { 
																	left: Factor::Primary(
																		Atom::Ident(Ident::Inherit("y".to_string()))
																	), 
																	cont: None 
																},
																cont: Some(SumP { 
																	op: crate::parser::sum::Op1::Add, 
																	right: Term {
																		left: Factor::Primary(
																			Atom::LitNum(1)
																		), 
																		cont: None 
																	}, 
																	cont: None
																})
															},
															comp: None
														}
													)
												])
											])
										)
									},
									Assignment { 
										ident: Ident::Synth("x".to_string()), 
										expr: Expression::Disjunct(
											Disjunct(vec![
												Conjunct(vec![
													Inverse::Comparison(
														Comparison {
															sum: Sum {
																left: Term { 
																	left: Factor::Primary(
																		Atom::Ident(Ident::Synth("x".to_string()))
																	), 
																	cont: None 
																},
																cont: Some(SumP { 
																	op: crate::parser::sum::Op1::Add, 
																	right: Term {
																		left: Factor::Primary(
																			Atom::LitNum(1)
																		), 
																		cont: None 
																	}, 
																	cont: None
																})
															},
															comp: None
														}
													)
												])
											])
										)
									}
								])),
								ebnf: None, 
							},
							Chunk::simple_ident("B")
						] 
					}
				]),
				Rule::Analytic("B".to_string(), Vec::new(), vec![
					Rhs::simple_terminal("b")
				])
			]
		});
		assert_eq!(Ok(expected), output);
	}

	#[test]
	fn test_simple_empty_alt() {
		let input = "S -> 'a' | ;";
		let mut lexer = LexerBridge::new(input);
		let output = Wag::parse(&mut lexer);
		let expected = unspanned_tree!(Wag {
		    metadata: Metadata { includes: vec![], mappings: BTreeMap::new() },
		    grammar: vec![
		    	Rule::Analytic("S".to_string(), Vec::new(), vec![
		    		Rhs { 
		    			weight: None,
		    			chunks: vec![Chunk::simple_terminal("a")] 
		    		},
		    		Rhs::empty()
		    	])
		    ],
		});
		assert_eq!(Ok(expected), output);
	}

	#[test]
	fn test_parse_error() {
		let input = r#"
		include activities::other;
		type: conversational
		==========================

		start -> setup activity* 'stop'; /* a comment */
		setup -> greet? getname;
		greet -> ('hello' {hello = true;})+ | "good morning";
		greet => 'greetings human!' 
		| [0.3] "What is your name? ";
		"#;
		let mut parser = Parser::new(input);
		let output = parser.parse();
		let expected = Err(crate::parser::WagParseError::Unexpected { 
			span: Span {start: 55, end: 57}, 
			offender: Tokens::MathToken(Math::Eq), 
			expected: string_vec![
				Tokens::MathToken(Math::Semi)
			]
		});
		assert_eq!(expected, output);
	}

	#[test]
	fn test_simple_rewrite_maybe() {
		let input = r"
		A -> X Y?;
		";
		let mut parser = Parser::new(input);
		let mut output = parser.parse().unwrap();
		output.rewrite(0, &mut FirstPassState::default()).unwrap();
		let expected = unspanned_tree!(Wag { 
			metadata: Metadata { includes: Vec::new(), mappings: BTreeMap::new() }, 
			grammar: vec![
				Rule::Analytic("A·0·1".to_string(), Vec::new(), vec![
					Rhs {
						weight: None,
						chunks: vec![Chunk::simple_ident("Y")]
					},
					Rhs::empty()
				]),
				Rule::Analytic("A".to_string(), Vec::new(), vec![
					Rhs { 
						weight: None,
						chunks: vec![
							Chunk::simple_ident("X"),
							Chunk::simple_ident("A·0·1")
						] 
					}
				]),
			]
		});
		assert_eq!(expected, output);
	}

	#[test]
	fn test_simple_rewrite_many() {
		let input = r"
		A -> X Y*;
		";
		let mut parser = Parser::new(input);
		let mut output = parser.parse().unwrap();
		output.rewrite(0, &mut FirstPassState::default()).unwrap();
		let expected = unspanned_tree!(Wag { 
			metadata: Metadata { includes: Vec::new(), mappings: BTreeMap::new() }, 
			grammar: vec![
				Rule::Analytic("A·0·1".to_string(), Vec::new(), vec![
					Rhs {
						weight: None,
						chunks: vec![
							Chunk::simple_ident("Y"),
							Chunk::simple_ident("A·0·1")
						]
					},
					Rhs::empty()
				]),
				Rule::Analytic("A".to_string(), Vec::new(), vec![
					Rhs { 
						weight: None,
						chunks: vec![
							Chunk::simple_ident("X"),
							Chunk::simple_ident("A·0·1")
						] 
					}
				]),
			]
		});
		assert_eq!(expected, output);
	}

	#[test]
	fn test_simple_rewrite_some() {
		let input = r"
		A -> X Y+;
		";
		let mut parser = Parser::new(input);
		let mut output = parser.parse().unwrap();
		output.rewrite(0, &mut FirstPassState::default()).unwrap();
		let expected = unspanned_tree!(Wag { 
			metadata: Metadata { includes: Vec::new(), mappings: BTreeMap::new() }, 
			grammar: vec![
				Rule::Analytic("A·0·1·p".to_string(), Vec::new(), vec![
					Rhs {
						weight: None,
						chunks: vec![
							Chunk::simple_ident("Y"),
							Chunk::simple_ident("A·0·1·p")
						]
					},
					Rhs::empty()
				]),
				Rule::Analytic("A·0·1".to_string(), Vec::new(), vec![
					Rhs {
						weight: None,
						chunks: vec![
							Chunk::simple_ident("Y"),
							Chunk::simple_ident("A·0·1·p")
						]
					}
				]),
				Rule::Analytic("A".to_string(), Vec::new(), vec![
					Rhs { 
						weight: None,
						chunks: vec![
							Chunk::simple_ident("X"),
							Chunk::simple_ident("A·0·1")
						] 
					}
				]),
			]
		});
		assert_eq!(expected, output);
	}

	#[test]
	fn test_simple_group() {
		let input = r"
		A -> (B C?)+;
		";
		let mut parser = Parser::new(input);
		let mut output = parser.parse().unwrap();
		output.rewrite(0, &mut FirstPassState::default()).unwrap();
		let expected = unspanned_tree!(Wag {
			metadata: Metadata { includes: Vec::new(), mappings: BTreeMap::new() }, 
			grammar: vec![
				Rule::Analytic("A·0·0_0·0·1".to_string(), Vec::new(), vec![
					Rhs {
						weight: None,
						chunks: vec![Chunk::simple_ident("C")]
					},
					Rhs::empty()
				]),
				Rule::Analytic("A·0·0_0".to_string(), Vec::new(), vec![
					Rhs {
						weight: None,
						chunks: vec![
							Chunk::simple_ident("B"),
							Chunk::simple_ident("A·0·0_0·0·1")
						]
					}
				]),
				Rule::Analytic("A·0·0·p".to_string(), Vec::new(), vec![
					Rhs {
						weight: None,
						chunks: vec![
							Chunk::simple_ident("A·0·0_0"),
							Chunk::simple_ident("A·0·0·p")
						]
					},
					Rhs::empty()
				]),
				Rule::Analytic("A·0·0".to_string(), Vec::new(), vec![
					Rhs {
						weight: None,
						chunks: vec![
							Chunk::simple_ident("A·0·0_0"),
							Chunk::simple_ident("A·0·0·p")
						]
					},
				]),
				Rule::Analytic("A".to_string(), Vec::new(), vec![
					Rhs {
						weight: None,
						chunks: vec![Chunk::simple_ident("A·0·0")]
					}
				])
			]
		});
		assert_eq!(expected, output);
	}

	#[test]
	fn test_complex_rewrite() {
		let input = r"
		A -> ((X Y)+ Z?)+;
		";
		let mut parser = Parser::new(input);
		let mut output = parser.parse().unwrap();
		output.rewrite(0, &mut FirstPassState::default()).unwrap();
		let expected = unspanned_tree!(Wag {
			metadata: Metadata { includes: Vec::new(), mappings: BTreeMap::new() }, 
			grammar: vec![
				Rule::Analytic("A·0·0_0·0·0_1".to_string(), Vec::new(), vec![
	                Rhs {
	                    weight: None,
	                    chunks: vec![
	                        Chunk::simple_ident("X"),
	                        Chunk::simple_ident("Y")
	                    ],
	                },
	            ]),
		        Rule::Analytic("A·0·0_0·0·0·p".to_string(), Vec::new(), vec![
	                Rhs {
	                    weight: None,
	                    chunks: vec![
	                        Chunk::simple_ident("A·0·0_0·0·0_1"),
	                        Chunk::simple_ident("A·0·0_0·0·0·p")
	                    ],
	                },
	                Rhs::empty()
	            ]),
		        Rule::Analytic("A·0·0_0·0·0".to_string(), Vec::new(), vec![
	                Rhs {
	                    weight: None,
	                    chunks: vec![
	                        Chunk::simple_ident("A·0·0_0·0·0_1"),
	                        Chunk::simple_ident("A·0·0_0·0·0·p")
	                    ],
	                },
	            ]),
		        Rule::Analytic("A·0·0_0·0·1".to_string(), Vec::new(), vec![
	                Rhs {
	                    weight: None,
	                    chunks: vec![
	                        Chunk::simple_ident("Z")
	                    ],
	                },
	                Rhs::empty()
	            ]),
		        Rule::Analytic("A·0·0_0".to_string(), Vec::new(), vec![
	                Rhs {
	                    weight: None,
	                    chunks: vec![
	                        Chunk::simple_ident("A·0·0_0·0·0"),
	                        Chunk::simple_ident("A·0·0_0·0·1")
	                    ],
	                },
	            ]),
		        Rule::Analytic("A·0·0·p".to_string(), Vec::new(), vec![
	                Rhs {
	                    weight: None,
	                    chunks: vec![
	                        Chunk::simple_ident("A·0·0_0"),
	                        Chunk::simple_ident("A·0·0·p")
	                    ],
	                },
	                Rhs::empty()
	            ]),
		        Rule::Analytic("A·0·0".to_string(), Vec::new(), vec![
	                Rhs {
	                    weight: None,
	                    chunks: vec![
	                        Chunk::simple_ident("A·0·0_0"),
	                        Chunk::simple_ident("A·0·0·p")
	                    ],
	                },
	            ]),
		        Rule::Analytic("A".to_string(), Vec::new(), vec![
	                Rhs {
	                    weight: None,
	                    chunks: vec![
	                        Chunk::simple_ident("A·0·0")
	                    ],
	                },
	            ]),
		    ]
		});
		assert_eq!(expected, output);
	}

	#[test]
	fn test_rewrite_group_no_ebnf() {
		let input = r"
		A -> (B C);
		";
		let mut parser = Parser::new(input);
		let mut output = parser.parse().unwrap();
		output.rewrite(0, &mut FirstPassState::default()).unwrap();
		let expected = unspanned_tree!(Wag {
			metadata: Metadata { includes: Vec::new(), mappings: BTreeMap::new() }, 
			grammar: vec![
				Rule::Analytic("A".to_string(), Vec::new(), vec![
					Rhs {
						weight: None,
						chunks: vec![
							Chunk {
								chunk: ChunkP::Group(vec![
									Chunk::simple_ident("B"),
									Chunk::simple_ident("C")
								]),
								ebnf: None
							}
						]
					}
				])
			]
		});
		assert_eq!(expected, output);
	}

	#[test]
	fn test_rewrite_conflict() {
		let input = r"
		A -> B;
		A -> C;
		A -> ;
		";
		let mut parser = Parser::new(input);
		let mut output = parser.parse().unwrap();
		output.rewrite(0, &mut FirstPassState::default()).unwrap();
		let expected = unspanned_tree!(Wag {
			metadata: Metadata { includes: Vec::new(), mappings: BTreeMap::new() }, 
			grammar: vec![
				Rule::Analytic("A".to_string(), Vec::new(), vec![
					Rhs::simple_ident("B"),
					Rhs::simple_ident("C"),
					Rhs::empty()
				])
			]
		});
		assert_eq!(expected, output);
	}

}