
use crate::lexer::{UnsafePeek, UnsafeNext, ident::Ident};
use super::{Parse, PeekLexer, ParseResult, Tokens, Spannable, WagParseError, ast::ToAst, rule::Rule, rhs::Rhs, Rewrite};
use crate::lexer::productions::{Productions, EbnfType};
use super::symbol::Symbol;

/*
 Chunks are symbols in () with optionally an EBNF token following it.
 If there are no (), there is only 1 symbol, which may still optionally have an EBNF token.
*/
#[derive(PartialEq, Debug, Eq, Hash)]
pub(crate) struct Chunk {
	pub(crate) chunk: ChunkP,
	pub(crate) ebnf: Option<EbnfType>
}

#[derive(PartialEq, Debug, Eq, Hash)]
pub(crate) enum ChunkP {
	Unit(Symbol),
	Group(Vec<Chunk>)
}

impl Chunk {

	pub(crate) fn simple_ident(ident: String) -> Self {
		Self {
            ebnf: None,
            chunk: ChunkP::Unit(Symbol::NonTerminal(Ident::Unknown(ident)))
        }
	}

	fn rewrite_ebnf(ebnf: &mut EbnfType, ident: String, symbol: Symbol, rule_func: fn(String, Vec<Rhs>) -> Rule, rules: &mut Vec<Rule>) {
		let chunks = match ebnf {
            crate::lexer::productions::EbnfType::Some => {
                let helper_ident = format!("{}-p", ident);
                rules.push(rule_func(helper_ident.clone(), 
                    vec![
                        Rhs {
                            weight: None,
                            chunks: vec![
                                Self {
                                    ebnf: None,
                                    chunk: ChunkP::Unit(symbol.clone())
                                },
                                Self::simple_ident(helper_ident.clone())
                            ]
                        },
                        Rhs::empty()
                    ]
                ));
                vec![
                    Rhs {
                        weight: None,
                        chunks: vec![
                            Self {
                                ebnf: None,
                                chunk: ChunkP::Unit(symbol)
                            },
                            Self::simple_ident(helper_ident)
                        ]
                    }
                ]
            },
            crate::lexer::productions::EbnfType::Many => {
                vec![
                    Rhs {
                        weight: None,
                        chunks: vec![
                            Self {
                                ebnf: None,
                                chunk: ChunkP::Unit(symbol)
                            },
                            Self::simple_ident(ident.clone())
                        ]
                    },
                    Rhs::empty()
                ]
            },
            crate::lexer::productions::EbnfType::Maybe => {
                vec![
                    Rhs {
                        weight: None,
                        chunks: vec![
                            Self {
                                ebnf: None,
                                chunk: ChunkP::Unit(symbol)
                            }
                        ]
                    },
                    Rhs::empty()
                ]
            },
        };
        rules.push(rule_func(ident, chunks));
	}

	pub(crate) fn rewrite(&mut self, ident: String, rule_func: fn(String, Vec<Rhs>) -> Rule, depth: usize) -> Vec<Rule> {
		let mut rules = Vec::new();
		match self {
            Self { ebnf: None, .. } => {}
            Self { ebnf: Some(e), chunk: ChunkP::Unit(u)} => {
                let yanked = std::mem::replace(u, Symbol::NonTerminal(Ident::Unknown(ident.clone())));
                Self::rewrite_ebnf(e, ident, yanked, rule_func, &mut rules);
                self.ebnf = None;
            },
            Self { ebnf: Some(e), chunk: ChunkP::Group(g)} => {
                let new_ident = format!("{}^{}", ident, depth);
                let mut new_rule = rule_func(new_ident.clone(), vec![Rhs { weight: None, chunks: std::mem::take(g) }]);
                rules.extend(new_rule.rewrite(depth+1));
                rules.push(new_rule);
            	let symbol = Symbol::NonTerminal(Ident::Unknown(new_ident));
            	Self::rewrite_ebnf(e, ident.clone(), symbol, rule_func, &mut rules);
            	self.ebnf = None;
            	self.chunk = ChunkP::Unit(Symbol::NonTerminal(Ident::Unknown(ident)));
            }
        };
        rules
	}

}

impl Parse for Chunk {
	fn parse(lexer: &mut PeekLexer) -> ParseResult<Self> { 
		let chunk = match lexer.peek_unwrap() {
			Tokens::ProductionToken(Productions::LPar) => {
				let mut ret = Vec::new();
				lexer.next();
				while lexer.peek_unwrap() != &Tokens::ProductionToken(Productions::RPar) {
					ret.push(Chunk::parse(lexer)?);
				}
				lexer.next();
				ChunkP::Group(ret)
			},
			Tokens::ProductionToken(Productions::Semi) => { // Empty rule
				return Ok(Self { chunk: ChunkP::Unit(Symbol::Epsilon), ebnf: None })
			}
			_ => {
				ChunkP::Unit(Symbol::parse(lexer)?)
			}
		};
		if let Tokens::ProductionToken(Productions::Ebnf(_)) = lexer.peek_unwrap() {
			if let Tokens::ProductionToken(Productions::Ebnf(x)) = lexer.next_unwrap() {
				Ok(Self {chunk, ebnf: Some(x)})
			} else { 
    			Err(WagParseError::Fatal((lexer.span(), "Something went terribly wrong. Unwrapped non-ebnf when should have unwrapped ebnf".to_string())))  
    		}
		} else {
			Ok(Self {chunk, ebnf: None})
		}
	}
}

impl ToAst for Chunk {
    fn to_ast(self, ast: &mut super::ast::WagTree) -> super::ast::WagIx {
    	let node = super::ast::WagNode::Chunk(self.ebnf);
    	match self.chunk {
	        ChunkP::Unit(c) => {
	        	let ix = ast.add_node(node);
	        	let child = c.to_ast(ast);
	        	ast.add_edge(ix, child, ());
	        	ix
	        },
	        ChunkP::Group(g) => Self::add_vec_children(node, g, ast),
	    }
    }
}