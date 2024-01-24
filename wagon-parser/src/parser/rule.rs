use super::{Parse, LexerBridge, ParseResult, Tokens, WagParseError, helpers::{check_semi, between_sep}, Rewrite, SpannableNode, Peek, ResultNext};
use wagon_lexer::{productions::{ImportType, Productions}, Spannable};
use crate::firstpass::{FirstPassState, FirstPassResult};

use super::rhs::Rhs;
use super::Ident;

use wagon_macros::match_error;

#[cfg(test)]
use wagon_macros::new_unspanned;

#[derive(PartialEq, Debug, Eq, Hash)]
#[cfg_attr(test, new_unspanned)]
/// A single rule in the WAG grammar. 
///
/// A rule is of the form: [`Ident`] {ARROW} [`Rhs`] | [`Rhs`] | ...;
pub enum Rule {
    /// An analytic rule (`->`).
	Analytic(String, Vec<SpannableNode<Ident>>, Vec<SpannableNode<Rhs>>),
    /// A generative rule (`=>`).
	Generate(String, Vec<SpannableNode<Ident>>, Vec<SpannableNode<Rhs>>),
    /// An import rule (`<-/=/<`).
	Import(String, ImportType, String),
    /// An import exclude rule (`</`).
	Exclude(String, Vec<SpannableNode<String>>)
}

impl Parse for Rule {
    fn parse(lexer: &mut LexerBridge) -> ParseResult<Self> {
        let ident = match_error!(match lexer.next_result()? {
        	Tokens::ProductionToken(Productions::Identifier(wagon_ident::Ident::Unknown(s))) => Ok(s),
        })?;
        let args = if lexer.peek() == Some(&Ok(Tokens::ProductionToken(Productions::Lt))) {
            between_sep(lexer, &Tokens::ProductionToken(Productions::Lt), &Tokens::ProductionToken(Productions::Gt), Tokens::ProductionToken(Productions::Comma))?
        } else {
            Vec::new()
        };
        let resp = match_error!(match lexer.next_result()? {
        	Tokens::ProductionToken(Productions::Produce) => {
                let rhs = SpannableNode::parse_sep(lexer, Tokens::ProductionToken(Productions::Alternative))?;
                Ok(Self::Analytic(ident, args, rhs))
            },
        	Tokens::ProductionToken(Productions::Generate) => {
                let rhs = SpannableNode::parse_sep(lexer, Tokens::ProductionToken(Productions::Alternative))?;
                Ok(Self::Generate(ident, args, rhs))
            },
        	Tokens::ProductionToken(Productions::Import(i)) => {
        		match i {
        			ImportType::Basic | ImportType::Full | ImportType::Recursive => {
        				match lexer.next_result()? {
        					Tokens::ProductionToken(Productions::Identifier(wagon_ident::Ident::Unknown(s))) => {
                                Ok(Self::Import(ident, i, s))
                            },
        					error => Err(WagParseError::Unexpected {
                                span: lexer.span(), 
                                offender: error, 
                                expected: vec![Tokens::ProductionToken(Productions::Identifier(Ident::default())).to_string()]
                            })
        				}
        			}
        			ImportType::Exclude => {
                        Ok(Self::Exclude(ident, SpannableNode::parse_sep(lexer, Tokens::ProductionToken(Productions::Additional))?))
                    }
        		}
        	}
        });
        check_semi(lexer)?;
        resp
    }
}

/*
Ident format:

{BASE}·{alt}·{chunk}             - Default
                    ·p           - Helper for '+'
                    [_{chunk}]+  - Deeper layers of recursive EBNF
                               - - Default again but at this layer

*/
/// Convert every [`Chunk`] for every alternative into it's own separate `Rule`.
impl Rewrite<Vec<Self>> for SpannableNode<Rule> {
    fn rewrite(&mut self, depth: usize, state: &mut FirstPassState) -> FirstPassResult<Vec<Self>> {
        match &mut self.node {
            Rule::Analytic(s, args, rhs) => {
                let mut rules = Vec::new();
                for (i, alt) in rhs.iter_mut().enumerate() {
                    for (j, chunk) in alt.node.chunks.iter_mut().enumerate() {
                        let ident = format!("{s}·{i}·{j}");
                        let (chunk_node, span) = chunk.deconstruct();
                        rules.extend(chunk_node.rewrite(ident, args.clone(), span, Rule::Analytic, depth, state)?);
                    }
                }
                for arg in args {
                    state.add_parameter(s.clone(), arg.clone())?;
                }
                Ok(rules)
            },
            Rule::Generate(s, args, rhs) => {
                let mut rules = Vec::new();
                for (i, alt) in rhs.iter_mut().enumerate() {
                    for (j, chunk) in alt.node.chunks.iter_mut().enumerate() {
                        let ident = format!("{s}·{i}·{j}");
                        let (chunk_node, span) = chunk.deconstruct();
                        rules.extend(chunk_node.rewrite(ident, args.clone(), span, Rule::Generate, depth, state)?);
                    }
                }
                for arg in args {
                    state.add_parameter(s.clone(), arg.clone())?;
                }
                Ok(rules)
            },
            Rule::Import(..) => todo!(),
            Rule::Exclude(..) => todo!(),
        }
    }
}
