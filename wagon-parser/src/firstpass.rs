use std::fmt::Display;
use wagon_lexer::Spannable;
use crate::{SpannableNode, ErrorReport, Span};
use std::{collections::HashMap, error::Error};

use indexmap::IndexSet;
use wagon_ident::Ident;

pub(crate) type ReqAttributes = IndexSet<SpannableNode<Ident>>;

/// Any node that can be rewritten in a different way for any reason should implement this trait.
pub(crate) trait Rewrite<T> {
	fn rewrite(&mut self, depth: usize, state: &mut FirstPassState) -> FirstPassResult<T>;
}

/// A state object to keep track of things during rewriting.
#[derive(Debug, Default)]
pub(crate) struct FirstPassState {
	parameter_map: HashMap<String, IndexSet<SpannableNode<Ident>>>
}

/// After rewriting/typechecking, we either return something or we have a [`WagCheckError`].
pub type FirstPassResult<T> = Result<T, WagCheckError>;

#[derive(PartialEq, Eq, Debug)]
/// Any error that can occur during the rewriting/checking process.
pub enum WagCheckError {
	/// A rule wants multiple parameters, but they are the exact same.
	DuplicateParameters(String, SpannableNode<Ident>),
	/// Two alternative instances of a rule want different parameters.
	DisparateParameters {
		/// The non-terminal which has the issue.
		terminal: String,
		/// The specific [`Ident`] which caused the issue.
		offender: Vec<SpannableNode<Ident>>,
		/// The [`Ident`] we expected to see.
		expected: Vec<SpannableNode<Ident>>,
		/// The span information of this node.
		span: Span
	},
}

impl Error for WagCheckError{}

impl ErrorReport for WagCheckError {
	fn msg(&self) -> (String, String) {
		match self {
		    Self::DuplicateParameters(nt, i) => ("Duplicate Parameter!".to_string(), format!("Nonterminal {nt} uses parameter {i} multiple times. This makes no sense.")),
    		Self::DisparateParameters { terminal, offender, expected, ..} => ("Disparate Parameters!".to_string(), format!("Instance of terminal {terminal} uses parameters {offender:?} while it was defined earlier as {expected:?}. This is unsupported")),
		}
	}

	fn span(self) -> Span {
		match self {
		    Self::DuplicateParameters(_, i) => i.span(),
    		Self::DisparateParameters { span, .. } => span,
		}
	}
}

impl Display for WagCheckError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    	let (head, msg) = self.msg();
    	write!(f, "{head}: {msg}")
    }
}

impl FirstPassState {
	pub(crate) fn add_parameter(&mut self, nt: String, param: SpannableNode<Ident>) -> FirstPassResult<()> {
		if let Some(params) = self.parameter_map.get_mut(&nt) {
			if params.contains(&param) {
				Err(WagCheckError::DuplicateParameters(nt, param))
			} else {
				params.insert(param);
				Ok(())
			}
		} else {
			let mut set = IndexSet::new();
			set.insert(param);
			self.parameter_map.insert(nt, set);
			Ok(())
		}
	}

	// pub(crate) fn get_parameters(&self, s: &str) -> Option<&IndexSet<SpannableNode<Ident>>> {
	// 	self.parameter_map.get(s)
	// }
}

pub(crate) trait GetReqAttributes {
	fn get_req_attributes(&self) -> ReqAttributes;
}

#[cfg(test)]
mod test {

	use crate::BTreeMap;
	use crate::parser::{Parser, wag::Wag, metadata::Metadata, rule::Rule, rhs::Rhs, chunk::{Chunk, ChunkP}};
	use super::{FirstPassState, Rewrite};
	use wagon_macros::unspanned_tree;

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
				Rule::Analytic("A·0·0··0·0·1".to_string(), Vec::new(), vec![
					Rhs {
						weight: None,
						chunks: vec![Chunk::simple_ident("C")]
					},
					Rhs::empty()
				]),
				Rule::Analytic("A·0·0··0".to_string(), Vec::new(), vec![
					Rhs {
						weight: None,
						chunks: vec![
							Chunk::simple_ident("B"),
							Chunk::simple_ident("A·0·0··0·0·1")
						]
					}
				]),
				Rule::Analytic("A·0·0·p".to_string(), Vec::new(), vec![
					Rhs {
						weight: None,
						chunks: vec![
							Chunk::simple_ident("A·0·0··0"),
							Chunk::simple_ident("A·0·0·p")
						]
					},
					Rhs::empty()
				]),
				Rule::Analytic("A·0·0".to_string(), Vec::new(), vec![
					Rhs {
						weight: None,
						chunks: vec![
							Chunk::simple_ident("A·0·0··0"),
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
				Rule::Analytic("A·0·0··0·0·0··1".to_string(), Vec::new(), vec![
	                Rhs {
	                    weight: None,
	                    chunks: vec![
	                        Chunk::simple_ident("X"),
	                        Chunk::simple_ident("Y")
	                    ],
	                },
	            ]),
		        Rule::Analytic("A·0·0··0·0·0·p".to_string(), Vec::new(), vec![
	                Rhs {
	                    weight: None,
	                    chunks: vec![
	                        Chunk::simple_ident("A·0·0··0·0·0··1"),
	                        Chunk::simple_ident("A·0·0··0·0·0·p")
	                    ],
	                },
	                Rhs::empty()
	            ]),
		        Rule::Analytic("A·0·0··0·0·0".to_string(), Vec::new(), vec![
	                Rhs {
	                    weight: None,
	                    chunks: vec![
	                        Chunk::simple_ident("A·0·0··0·0·0··1"),
	                        Chunk::simple_ident("A·0·0··0·0·0·p")
	                    ],
	                },
	            ]),
		        Rule::Analytic("A·0·0··0·0·1".to_string(), Vec::new(), vec![
	                Rhs {
	                    weight: None,
	                    chunks: vec![
	                        Chunk::simple_ident("Z")
	                    ],
	                },
	                Rhs::empty()
	            ]),
		        Rule::Analytic("A·0·0··0".to_string(), Vec::new(), vec![
	                Rhs {
	                    weight: None,
	                    chunks: vec![
	                        Chunk::simple_ident("A·0·0··0·0·0"),
	                        Chunk::simple_ident("A·0·0··0·0·1")
	                    ],
	                },
	            ]),
		        Rule::Analytic("A·0·0·p".to_string(), Vec::new(), vec![
	                Rhs {
	                    weight: None,
	                    chunks: vec![
	                        Chunk::simple_ident("A·0·0··0"),
	                        Chunk::simple_ident("A·0·0·p")
	                    ],
	                },
	                Rhs::empty()
	            ]),
		        Rule::Analytic("A·0·0".to_string(), Vec::new(), vec![
	                Rhs {
	                    weight: None,
	                    chunks: vec![
	                        Chunk::simple_ident("A·0·0··0"),
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