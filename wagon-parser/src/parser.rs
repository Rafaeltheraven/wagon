#[macro_use]
pub mod wag;
pub mod assignment;
pub mod atom;
pub mod chunk;
pub mod comp;
pub mod conjunct;
pub mod disjunct;
pub mod expression;
pub mod factor;
pub mod helpers;
pub mod inverse;
pub mod metadata;
pub mod rhs;
pub mod rule;
pub mod sum;
pub mod symbol;
pub mod term;
pub mod terminal;
mod ident;

use std::{error::Error, fmt::Display, write};
use self::wag::Wag;
use crate::firstpass::{WagCheckError, Rewrite};
use crate::ast::{ToAst, WagNode, WagIx, WagTree};
use crate::SpannableNode;

use wagon_ident::Ident;
use wagon_utils::{peekable::Peekable, comma_separated_with_or, string_vec};
use wagon_lexer::{LexerBridge, PeekLexer, Tokens, UnsafeNext, Spannable, Span};

pub struct Parser<'source> {
	lexer: PeekLexer<'source>
}

impl<'source> Parser<'source> {
	pub fn new(data: &'source str) -> Self {
		Self {
			lexer: Peekable::new(LexerBridge::new(data))
		}
	}

	pub fn parse(&mut self) -> ParseResult<Wag> {
		Wag::parse(&mut self.lexer)
	}
}

pub type ParseResult<T> = Result<T, WagParseError>;

#[derive(PartialEq, Debug)]
pub enum WagParseError {
	Unexpected {
		span: Span,
		offender: Tokens,
		expected: Vec<String>
	},
	Fatal((Span, String)),
	CheckError(WagCheckError)
}

impl From<WagCheckError> for WagParseError {
    fn from(value: WagCheckError) -> Self {
        Self::CheckError(value)
    }
}

impl Error for WagParseError {}
impl Display for WagParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    	let (head, msg) = self.msg();
    	write!(f, "{}: {}", head, msg)
    }
}

impl WagParseError {
	pub fn msg_and_span(self) -> ((String, String), Span) {
		let msg = self.msg();
		let span = self.span();
		(msg, span)
	}

	fn span(self) -> Span {
		match self {
		    WagParseError::Unexpected { span, .. } => span,
		    WagParseError::Fatal((span, _)) => span,
		    WagParseError::CheckError(check) => check.span()
		}
	}

	fn msg(&self) -> (String, String) {
		match self {
		    WagParseError::Unexpected { span, offender, expected } => ("Unexpected Token".to_string(), 
		    	format!{"Encountered token {:?} at position {:?}. Expected {:#?}", offender, span, comma_separated_with_or(expected)}),
	        WagParseError::Fatal((_, msg)) => ("Fatal Exception".to_string(), msg.to_string()),
	        WagParseError::CheckError(err) => err.msg()
		}
	}
}

pub trait Parse {

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

    use wagon_lexer::productions::{EbnfType, Productions};
	use crate::firstpass::FirstPassState;
    use super::Peekable;
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
    use wagon_lexer::productions::GrammarType;
    use wagon_ident::Ident;
    use super::chunk::ChunkP;
    use super::string_vec;

    use wagon_lexer::Span;
    use pretty_assertions::assert_eq;

    #[test]
	fn test_example_wag() {
		let input = r#"
		include activities::other;

		conversational grammar;

		start -> setup activity* 'stop'; /* a comment */
		setup -> greet? getname | ;
		greet -> ('hello' {hello = true;})+ | "good morning";
		greet => 'greetings human!' 
		| [0.3] "What is your name? ";
		getname -> ;
		"#;
		let mut lexer = Peekable::new(LexerBridge::new(input));
		let output = Wag::parse(&mut lexer);
		let expected = unspanned_tree!(Wag { 
			metadata: Metadata {
				includes: vec!["activities::other".to_string()],
				spec: Some(GrammarType::Conversational)
			}, 
			grammar: vec![
				Rule::Analytic("start".to_string(), Vec::new(), vec![
					Rhs { 
						weight: None,
						probability: None,
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
						probability: None,
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
						probability: None,
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
						probability: None,
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
						probability: None,
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
						probability: None,
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
	fn test_example_wag2() {
		let input = r#"
		S -> {$x = 0; $y = 0;} X($x, $y);
		X(*y, &x) -> 'a' {*y = *y + 1; &x = &x + 1;} B;
		B -> 'b';
		"#;
		let mut lexer = Peekable::new(LexerBridge::new(input));
		let output = Wag::parse(&mut lexer);
		let expected = unspanned_tree!(Wag {
			metadata: Metadata { includes: vec![], spec: None },
			grammar: vec![
				Rule::Analytic("S".to_string(), Vec::new(), vec![
					Rhs {
						weight: None,
						probability: None,
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
						probability: None,
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
		let mut lexer = Peekable::new(LexerBridge::new(input));
		let output = Wag::parse(&mut lexer);
		let expected = unspanned_tree!(Wag {
		    metadata: Metadata { includes: vec![], spec: None },
		    grammar: vec![
		    	Rule::Analytic("S".to_string(), Vec::new(), vec![
		    		Rhs { 
		    			weight: None,
		    			probability: None, 
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
			offender: Tokens::ProductionToken(Productions::Identifier(Ident::Unknown("start".to_string()))), 
			expected: string_vec![
				Tokens::ProductionToken(Productions::Semi)
			]
		});
		assert_eq!(expected, output);
	}

	#[test]
	fn test_simple_rewrite_maybe() {
		let input = r#"
		A -> X Y?;
		"#;
		let mut parser = Parser::new(input);
		let mut output = parser.parse().unwrap();
		output.rewrite(0, &mut FirstPassState::default()).unwrap();
		let expected = unspanned_tree!(Wag { 
			metadata: Metadata { includes: Vec::new(), spec: None }, 
			grammar: vec![
				Rule::Analytic("A·0·1".to_string(), Vec::new(), vec![
					Rhs {
						weight: None,
						probability: None,
						chunks: vec![Chunk::simple_ident("Y")]
					},
					Rhs::empty()
				]),
				Rule::Analytic("A".to_string(), Vec::new(), vec![
					Rhs { 
						weight: None,
						probability: None, 
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
		let input = r#"
		A -> X Y*;
		"#;
		let mut parser = Parser::new(input);
		let mut output = parser.parse().unwrap();
		output.rewrite(0, &mut FirstPassState::default()).unwrap();
		let expected = unspanned_tree!(Wag { 
			metadata: Metadata { includes: Vec::new(), spec: None }, 
			grammar: vec![
				Rule::Analytic("A·0·1".to_string(), Vec::new(), vec![
					Rhs {
						weight: None,
						probability: None,
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
						probability: None,
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
		let input = r#"
		A -> X Y+;
		"#;
		let mut parser = Parser::new(input);
		let mut output = parser.parse().unwrap();
		output.rewrite(0, &mut FirstPassState::default()).unwrap();
		let expected = unspanned_tree!(Wag { 
			metadata: Metadata { includes: Vec::new(), spec: None }, 
			grammar: vec![
				Rule::Analytic("A·0·1·p".to_string(), Vec::new(), vec![
					Rhs {
						weight: None,
						probability: None,
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
						probability: None,
						chunks: vec![
							Chunk::simple_ident("Y"),
							Chunk::simple_ident("A·0·1·p")
						]
					}
				]),
				Rule::Analytic("A".to_string(), Vec::new(), vec![
					Rhs { 
						weight: None,
						probability: None,
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
		let input = r#"
		A -> (B C?)+;
		"#;
		let mut parser = Parser::new(input);
		let mut output = parser.parse().unwrap();
		output.rewrite(0, &mut FirstPassState::default()).unwrap();
		let expected = unspanned_tree!(Wag {
			metadata: Metadata { includes: Vec::new(), spec: None }, 
			grammar: vec![
				Rule::Analytic("A·0·0_0·0·1".to_string(), Vec::new(), vec![
					Rhs {
						weight: None,
						probability: None,
						chunks: vec![Chunk::simple_ident("C")]
					},
					Rhs::empty()
				]),
				Rule::Analytic("A·0·0_0".to_string(), Vec::new(), vec![
					Rhs {
						weight: None,
						probability: None,
						chunks: vec![
							Chunk::simple_ident("B"),
							Chunk::simple_ident("A·0·0_0·0·1")
						]
					}
				]),
				Rule::Analytic("A·0·0·p".to_string(), Vec::new(), vec![
					Rhs {
						weight: None,
						probability: None,
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
						probability: None,
						chunks: vec![
							Chunk::simple_ident("A·0·0_0"),
							Chunk::simple_ident("A·0·0·p")
						]
					},
				]),
				Rule::Analytic("A".to_string(), Vec::new(), vec![
					Rhs {
						weight: None,
						probability: None,
						chunks: vec![Chunk::simple_ident("A·0·0")]
					}
				])
			]
		});
		assert_eq!(expected, output);
	}

	#[test]
	fn test_complex_rewrite() {
		let input = r#"
		A -> ((X Y)+ Z?)+;
		"#;
		let mut parser = Parser::new(input);
		let mut output = parser.parse().unwrap();
		output.rewrite(0, &mut FirstPassState::default()).unwrap();
		let expected = unspanned_tree!(Wag {
			metadata: Metadata { includes: Vec::new(), spec: None }, 
			grammar: vec![
				Rule::Analytic("A·0·0_0·0·0_1".to_string(), Vec::new(), vec![
	                Rhs {
	                    weight: None,
	                    probability: None,
	                    chunks: vec![
	                        Chunk::simple_ident("X"),
	                        Chunk::simple_ident("Y")
	                    ],
	                },
	            ]),
		        Rule::Analytic("A·0·0_0·0·0·p".to_string(), Vec::new(), vec![
	                Rhs {
	                    weight: None,
	                    probability: None,
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
	                    probability: None,
	                    chunks: vec![
	                        Chunk::simple_ident("A·0·0_0·0·0_1"),
	                        Chunk::simple_ident("A·0·0_0·0·0·p")
	                    ],
	                },
	            ]),
		        Rule::Analytic("A·0·0_0·0·1".to_string(), Vec::new(), vec![
	                Rhs {
	                    weight: None,
	                    probability: None,
	                    chunks: vec![
	                        Chunk::simple_ident("Z")
	                    ],
	                },
	                Rhs::empty()
	            ]),
		        Rule::Analytic("A·0·0_0".to_string(), Vec::new(), vec![
	                Rhs {
	                    weight: None,
	                    probability: None,
	                    chunks: vec![
	                        Chunk::simple_ident("A·0·0_0·0·0"),
	                        Chunk::simple_ident("A·0·0_0·0·1")
	                    ],
	                },
	            ]),
		        Rule::Analytic("A·0·0·p".to_string(), Vec::new(), vec![
	                Rhs {
	                    weight: None,
	                    probability: None,
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
	                    probability: None,
	                    chunks: vec![
	                        Chunk::simple_ident("A·0·0_0"),
	                        Chunk::simple_ident("A·0·0·p")
	                    ],
	                },
	            ]),
		        Rule::Analytic("A".to_string(), Vec::new(), vec![
	                Rhs {
	                    weight: None,
	                    probability: None,
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
		let input = r#"
		A -> (B C);
		"#;
		let mut parser = Parser::new(input);
		let mut output = parser.parse().unwrap();
		output.rewrite(0, &mut FirstPassState::default()).unwrap();
		let expected = unspanned_tree!(Wag {
			metadata: Metadata { includes: Vec::new(), spec: None }, 
			grammar: vec![
				Rule::Analytic("A".to_string(), Vec::new(), vec![
					Rhs {
						weight: None,
						probability: None,
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
		let input = r#"
		A -> B;
		A -> C;
		A -> ;
		"#;
		let mut parser = Parser::new(input);
		let mut output = parser.parse().unwrap();
		output.rewrite(0, &mut FirstPassState::default()).unwrap();
		let expected = unspanned_tree!(Wag {
			metadata: Metadata { includes: Vec::new(), spec: None }, 
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