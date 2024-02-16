
use std::fmt::Display;

use crate::WrapSpannable;
use crate::firstpass::{FirstPassResult, FirstPassState, GetReqAttributes, ReqAttributes};
use wagon_lexer::{productions::{Productions, EbnfType}, Span};
use super::CallingArgs;
use super::{Parse, LexerBridge, ParseResult, Tokens, Spannable, WagParseError, Ident, Rewrite, rule::Rule, rhs::Rhs, symbol::Symbol, SpannableNode, ResultPeek, ResultNext};

use wagon_macros::new_unspanned;

#[derive(PartialEq, Debug, Eq, Hash, Clone)]
#[new_unspanned]
/// A chunk of an [`Rhs`].
///
/// Chunks are symbols in () with optionally an EBNF token following it.
/// If there are no (), there is only 1 symbol, which may still optionally have an EBNF token.
pub struct Chunk {
    /// The actual chunk part.
	pub chunk: ChunkP,
    /// The possible EBNF operator.
	pub ebnf: Option<EbnfType>,
}

#[derive(PartialEq, Debug, Eq, Hash, Clone)]
#[new_unspanned]
/// The actual chunk part.
pub enum ChunkP {
    /// Just a [`Symbol`].
	Unit(SpannableNode<Symbol>),
    /// A group of [`Chunk`]s. Enclosed with `()`.
	Group(Vec<SpannableNode<Chunk>>)
}

type RuleConstructor = fn(String, Vec<SpannableNode<Ident>>, Vec<SpannableNode<Rhs>>) -> Rule;

impl Chunk {

	fn rewrite_ebnf(
            ebnf: &EbnfType, 
            ident: String,
            args: Vec<SpannableNode<Ident>>, 
            symbol: SpannableNode<Symbol>, 
            span: &Span, 
            rule_func: RuleConstructor, 
            rules: &mut Vec<SpannableNode<Rule>>
        ) {
        let chunks: Vec<SpannableNode<Rhs>> = match ebnf {
            EbnfType::Some => {
                let helper_ident = format!("{ident}·p");
                rules.push(SpannableNode::new(rule_func(helper_ident.clone(), args.clone(),
                    vec![
                        SpannableNode::new(Rhs {
                            weight: None,
                            chunks: vec![
                                SpannableNode::new(Self {
                                    ebnf: None,
                                    chunk: ChunkP::Unit(symbol.clone())
                                }, span.clone()),
                                Self::simple_ident_spanned_with_args(&helper_ident, span.clone(), args.clone())
                            ]
                        }, span.clone()),
                        Rhs::empty_spanned(span.clone())
                    ]
                ), span.to_owned()));
                vec![
                    SpannableNode::new(Rhs {
                        weight: None,
                        chunks: vec![
                            SpannableNode::new(Self {
                                ebnf: None,
                                chunk: ChunkP::Unit(symbol)
                            }, span.clone()),
                            Self::simple_ident_spanned_with_args(&helper_ident, span.clone(), args.clone())
                        ]
                    }, span.clone())
                ]
            },
            EbnfType::Many => {
                vec![
                    SpannableNode::new(Rhs {
                        weight: None,
                        chunks: vec![
                            SpannableNode::new(Self {
                                ebnf: None,
                                chunk: ChunkP::Unit(symbol)
                            }, span.clone()),
                            Self::simple_ident_spanned_with_args(&ident, span.clone(), args.clone())
                        ]
                    }, span.clone()),
                    Rhs::empty_spanned(span.clone())
                ]
            },
            EbnfType::Maybe => {
                vec![
                    SpannableNode::new(Rhs {
                        weight: None,
                        chunks: vec![
                            SpannableNode::new(Self {
                                ebnf: None,
                                chunk: ChunkP::Unit(symbol)
                            }, span.clone())
                        ]
                    }, span.clone()),
                    Rhs::empty_spanned(span.clone())
                ]
            },
        };
        rules.push(SpannableNode::new(rule_func(ident, args, chunks), span.to_owned()));
	}

    /// Rewrite according to the EBNF operator.
    ///
    /// If there is no EBNF operator, we do nothing.
    /// If there is, we extract the chunk that it operates on and rewrite it as a new, separate rule. We do this recursively.
    /// At the end, all EBNF operators are replaced by references to the new rules and we return a list of new rules to add to the grammar.
	pub(crate) fn rewrite(
            &mut self, 
            ident: String,
            span: &Span, 
            rule_func: RuleConstructor, 
            depth: usize, 
            state: &mut FirstPassState
        ) -> FirstPassResult<(Vec<SpannableNode<Rule>>, ReqAttributes)> {
		let mut rules = Vec::new();
        let required_args = if let Some(e) = std::mem::take(&mut self.ebnf) {
            match self {
                Self { chunk: ChunkP::Unit(u), ..} => {
                    Self::rewrite_unit_ebnf(u, &e, rule_func, depth, ident, span, &mut rules)
                },
                Self { chunk: ChunkP::Group(g), ..} => {
                    let real_g = std::mem::take(g);
                    self.rewrite_group(real_g, Some(&e), rule_func, depth, ident, span, state, &mut rules)?
                }
            }
        } else {
            match self {
                Self { chunk: ChunkP::Unit(u), ..} => {
                    Self::rewrite_unit_no_ebnf(u, depth)
                },
                Self { chunk: ChunkP::Group(g), ..} => {
                    let real_g = std::mem::take(g);
                    self.rewrite_group(real_g, None, rule_func, depth, ident, span, state, &mut rules)?
                }
            } 
        };
        Ok((rules, required_args))
	}

    fn rewrite_unit_ebnf(u: &mut SpannableNode<Symbol>, e: &EbnfType, rule_func: RuleConstructor, depth: usize, ident: String, span: &Span, rules: &mut Vec<SpannableNode<Rule>>) -> ReqAttributes {
        let calling_args = u.to_inner().calling_args();
        let req_args = u.get_req_attributes();
        let mut yanked = std::mem::replace(u, 
            SpannableNode::new(
                Symbol::NonTerminal(
                    SpannableNode::new(Ident::Unknown(ident.clone()), span.clone()), 
                    CallingArgs::new(),
                ), 
                span.clone()
            )
        );
        let symbol_args = yanked.to_inner_mut().rewrite().into_iter().collect();
        if let Symbol::NonTerminal(_, v) = u.to_inner_mut() {
            let main_args = if depth > 0 {
                let mut as_synth = CallingArgs::with_capacity(calling_args.len());
                for i in &calling_args {
                    let s = i.to_inner().extract_string();
                    as_synth.push(SpannableNode::new(Ident::Synth(s.to_string()), i.span()));
                }
                as_synth
            } else {
                calling_args
            };
            let _ = std::mem::replace(v, main_args);
        }
        Self::rewrite_ebnf(e, ident, symbol_args, yanked, span, rule_func, rules);
        req_args
    }

    fn rewrite_unit_no_ebnf(u: &mut SpannableNode<Symbol>, depth: usize) -> ReqAttributes {
        let req = u.get_req_attributes();
        if depth > 0 {
            u.to_inner_mut().rewrite();
        }
        req
    }

    #[allow(clippy::too_many_arguments)]
    fn rewrite_group(
            &mut self, 
            g: Vec<SpannableNode<Self>>, 
            e: Option<&EbnfType>, 
            rule_func: RuleConstructor, 
            depth: usize, 
            ident: String, 
            span: &Span, 
            state: &mut FirstPassState, 
            rules: &mut Vec<SpannableNode<Rule>>
        ) -> FirstPassResult<ReqAttributes> {
        let new_ident = if e.is_some() {
            format!("{ident}··{depth}")
        } else {
            ident.clone()
        };
        let mut new_rule = SpannableNode::new(
            rule_func(
                new_ident.clone(), 
                CallingArgs::new(), // Should be as synthesized
                vec![
                    Rhs { weight: None, chunks: g }.into_spanned(span.clone())
                ]
            ), 
            span.clone()
        );
        let (new_rules, req_args) = new_rule.rewrite(depth+1, state)?;
        let mut as_synth = CallingArgs::with_capacity(req_args.len());
        for i in &req_args {
            let s = i.to_inner().extract_string();
            as_synth.push(SpannableNode::new(Ident::Synth(s.to_string()), i.span()));
        }
        match new_rule.to_inner_mut() {
            Rule::Analytic(_, v, _) | Rule::Generate(_, v, _) => {
                let _ = std::mem::replace(v, as_synth.clone());
            },
            _ => {}
        }
        let symbol_args = if depth > 0 {
            as_synth.clone()
        } else {
            req_args.iter().cloned().collect()
        };
        self.chunk = ChunkP::Unit(Symbol::simple_ident_spanned_with_args(&ident, span.clone(), symbol_args)); // Should be as expected
        if let Some(eb) = e {
            let symbol = Symbol::simple_ident_spanned_with_args(&new_ident, span.clone(), as_synth.clone()); // Should be as synthesized
            Self::rewrite_ebnf(eb, ident, as_synth, symbol, span, rule_func, rules); // Should be as synthesized
        }
        rules.push(new_rule);
        rules.extend(new_rules);
        Ok(req_args)
    }

    /// Check if this chunk is a terminal
    // pub(crate) fn is_terminal(&self) -> bool {
    //     match &self.chunk {
    //         ChunkP::Unit(s) => s.node.is_terminal(),
    //         ChunkP::Group(_) => false,
    //     }
    // }

    /// Automatically create a `Chunk` that is just a terminal. See [`Symbol::simple_terminal`].
    #[must_use] 
    pub fn simple_terminal(term: &str) -> Self {
        Self { 
            ebnf: None, 
            chunk: ChunkP::Unit(Symbol::simple_terminal(term).into()) 
        }
    }

    /// Automatically create a `Chunk` that is just an ident. See [`Symbol::simple_ident`].
    #[must_use] 
    pub fn simple_ident(ident: &str) -> Self {
        Self {
            ebnf: None,
            chunk: ChunkP::Unit(Symbol::simple_ident(ident).into())
        }
    }

    /// Automatically create a spanned `Chunk` that is just an ident.
    // pub(crate) fn simple_ident_spanned(ident: &str, span: Span) -> SpannableNode<Self> {
    //     Self::simple_ident_spanned_with_args(ident, span, Vec::new())
    // }

    pub(crate) fn simple_ident_spanned_with_args(ident: &str, span: Span, args: Vec<SpannableNode<Ident>>) -> SpannableNode<Self> {
        SpannableNode::new(Self {
            ebnf: None,
            chunk: ChunkP::Unit(Symbol::simple_ident_spanned_with_args(ident, span.clone(), args))
        }, span)
    }

    /// Automatically create an empty chunk.
    #[must_use] 
    pub fn empty() -> Self {
        Self {
            ebnf: None,
            chunk: ChunkP::Unit(Symbol::Epsilon.into())
        }
    }

    /// Automatically create a spanned empty chunk.
    pub(crate) fn empty_spanned(span: Span) -> SpannableNode<Self> {
        SpannableNode::new(
            Self {
                ebnf: None,
                chunk: ChunkP::Unit(SpannableNode::new(Symbol::Epsilon, span.clone()))
            }, span
        )
    }

    /// Extract all the symbols that are in this chunk.
    #[must_use] 
    pub fn extract_symbols(self) -> Vec<SpannableNode<Symbol>> {
        match self {
            Self {chunk: ChunkP::Unit(s), ..} => vec![s],
            Self {chunk: ChunkP::Group(g), ..} => {
                let mut ret = Vec::with_capacity(g.len());
                for chunk in g {
                    ret.extend(chunk.into_inner().extract_symbols());
                }
                ret
            }
        }
    }
}

impl Parse for Chunk {
	fn parse(lexer: &mut LexerBridge) -> ParseResult<Self> { 
		let chunk = match lexer.peek_result()? {
			Tokens::ProductionToken(Productions::LPar) => {
				let mut ret = Vec::new();
				lexer.next();
				while lexer.peek_result()? != &Tokens::ProductionToken(Productions::RPar) {
					ret.push(SpannableNode::parse(lexer)?);
				}
				lexer.next();
				ChunkP::Group(ret)
			},
			Tokens::ProductionToken(Productions::Semi) => { // Empty rule
				return Ok(Self::empty())
			}
			_ => {
				ChunkP::Unit(SpannableNode::parse(lexer)?)
			}
		};
		if let Tokens::ProductionToken(Productions::Ebnf(_)) = lexer.peek_result()? {
			if let Tokens::ProductionToken(Productions::Ebnf(x)) = lexer.next_result()? {
				Ok(Self {chunk, ebnf: Some(x)})
			} else { 
    			Err(WagParseError::Fatal((lexer.span(), "Something went terribly wrong. Unwrapped non-ebnf when should have unwrapped ebnf".to_string())))  
    		}
		} else {
			Ok(Self {chunk, ebnf: None})
		}
	}
}

impl GetReqAttributes for Chunk {
    fn get_req_attributes(&self) -> ReqAttributes {
        match &self.chunk {
            ChunkP::Unit(s) => s.get_req_attributes(),
            ChunkP::Group(g) => {
                let mut req = ReqAttributes::new();
                for c in g {
                    req.extend(c.get_req_attributes());
                }
                req
            },
        }
    }
}

use itertools::Itertools;
impl Display for Chunk {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(ebnf) = &self.ebnf {
            write!(f, "{}{ebnf}", self.chunk)
        } else {
            write!(f, "{}", self.chunk)
        }
    }
}

impl Display for ChunkP {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unit(s) => write!(f, "{s}"),
            Self::Group(g) => write!(f, "({})", g.iter().join(" ")),
        }
    }
}