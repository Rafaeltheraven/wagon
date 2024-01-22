
use crate::WrapSpannable;
use crate::firstpass::{FirstPassResult, FirstPassState};
use wagon_lexer::{productions::{Productions, EbnfType}, Span};
use super::{Parse, LexerBridge, ParseResult, Tokens, Spannable, WagParseError, Ident, Rewrite, rule::Rule, rhs::Rhs, symbol::Symbol, SpannableNode, ResultPeek, ResultNext};

#[cfg(test)]
use wagon_macros::new_unspanned;

#[derive(PartialEq, Debug, Eq, Hash, Clone)]
#[cfg_attr(test, new_unspanned)]
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
#[cfg_attr(test, new_unspanned)]
/// The actual chunk part.
pub enum ChunkP {
    /// Just a [`Symbol`].
	Unit(SpannableNode<Symbol>),
    /// A group of [`Chunk`]s. Enclosed with `()`.
	Group(Vec<SpannableNode<Chunk>>)
}

type RuleConstructor = fn(String, Vec<SpannableNode<Ident>>, Vec<SpannableNode<Rhs>>) -> Rule;

impl Chunk {

	fn rewrite_ebnf(ebnf: &EbnfType, ident: String, args: Vec<SpannableNode<Ident>>, symbol: SpannableNode<Symbol>, span: &Span, rule_func: RuleConstructor, rules: &mut Vec<SpannableNode<Rule>>) {
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
                                Self::simple_ident_spanned(&helper_ident, span.clone())
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
                            Self::simple_ident_spanned(&helper_ident, span.clone())
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
                            Self::simple_ident_spanned(&ident, span.clone())
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
	pub(crate) fn rewrite(&mut self, ident: String, args: Vec<SpannableNode<Ident>>, span: &Span, rule_func: RuleConstructor, depth: usize, state: &mut FirstPassState) -> FirstPassResult<Vec<SpannableNode<Rule>>> {
		let mut rules = Vec::new();
        if let Some(e) = std::mem::take(&mut self.ebnf) {
            match self {
                Self { chunk: ChunkP::Unit(u), ..} => {
                    let yanked = std::mem::replace(u, 
                        SpannableNode::new(
                            Symbol::NonTerminal(
                                SpannableNode::new(Ident::Unknown(ident.clone()), span.clone()), 
                                args.clone()
                            ), 
                            span.clone()
                        )
                    );
                    Self::rewrite_ebnf(&e, ident, args, yanked, span, rule_func, &mut rules);
                },
                Self { chunk: ChunkP::Group(g), ..} => {
                    let new_ident = format!("{ident}_{depth}");
                    let mut new_rule = SpannableNode::new(rule_func(new_ident.clone(), args.clone(), vec![Rhs { weight: None, chunks: std::mem::take(g) }.into_spanned(span.clone())]), span.clone());
                    rules.extend(new_rule.rewrite(depth+1, state)?);
                    rules.push(new_rule);
                    let symbol = Symbol::NonTerminal(Ident::Unknown(new_ident).into_spanned(span.clone()), args.clone()).into_spanned(span.clone());
                    Self::rewrite_ebnf(&e, ident.clone(), args.clone(), symbol, span, rule_func, &mut rules);
                    self.chunk = ChunkP::Unit(Symbol::NonTerminal(Ident::Unknown(ident).into_spanned(span.clone()), args).into_spanned(span.clone()));
                }
            };
        }
        Ok(rules)
	}

    /// Check if this chunk is a terminal
    // pub(crate) fn is_terminal(&self) -> bool {
    //     match &self.chunk {
    //         ChunkP::Unit(s) => s.node.is_terminal(),
    //         ChunkP::Group(_) => false,
    //     }
    // }

    #[cfg(test)]
    /// Automatically create a `Chunk` that is just a terminal. See [`Symbol::simple_terminal`].
    pub(crate) fn simple_terminal(term: &str) -> Self {
        Self { 
            ebnf: None, 
            chunk: ChunkP::Unit(Symbol::simple_terminal(term).into()) 
        }
    }

    #[cfg(test)]
    /// Automatically create a `Chunk` that is just an ident. See [`Symbol::simple_ident`].
    pub(crate) fn simple_ident(ident: &str) -> Self {
        Self {
            ebnf: None,
            chunk: ChunkP::Unit(Symbol::simple_ident(ident).into())
        }
    }

    /// Automatically create a spanned `Chunk` that is just an ident. 
    pub(crate) fn simple_ident_spanned(ident: &str, span: Span) -> SpannableNode<Self> {
        SpannableNode::new(Self {
            ebnf: None,
            chunk: ChunkP::Unit(Symbol::simple_ident_spanned(ident, span.clone()))
        }, span)
    }

    /// Automatically create an empty chunk.
    pub(crate) fn empty() -> Self {
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
    pub(crate) fn extract_symbols(self) -> Vec<SpannableNode<Symbol>> {
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
