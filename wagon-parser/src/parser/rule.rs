use std::fmt::Display;

use super::{helpers::{check_semi, between_sep}, LexerBridge, Parse, ParseResult, Peek, ResultNext, Rewrite, SpannableNode, Tokens, WagParseError};
use wagon_lexer::{productions::{ImportType, Productions}, Spannable};
use crate::firstpass::{FirstPassResult, FirstPassState, ReqAttributes};

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
                    [··{chunk}]+ - Deeper layers of recursive EBNF
                               - - Default again but at this layer

*/
/// Convert every [`Chunk`] for every alternative into it's own separate `Rule`.
impl Rewrite<(Vec<Self>, ReqAttributes)> for SpannableNode<Rule> {
    fn rewrite(&mut self, depth: usize, state: &mut FirstPassState) -> FirstPassResult<(Vec<Self>, ReqAttributes)> {
        match &mut self.node {
            Rule::Analytic(s, args, rhs) => {
                let mut rules = Vec::new();
                let mut req_attrs = ReqAttributes::new();
                for (i, alt) in rhs.iter_mut().enumerate() {
                    for (j, chunk) in alt.node.chunks.iter_mut().enumerate() {
                        let ident = format!("{s}·{i}·{j}");
                        let (chunk_node, span) = chunk.deconstruct();
                        let (new_rules, new_attrs) = chunk_node.rewrite(ident, span, Rule::Analytic, depth, state)?;
                        rules.extend(new_rules);
                        req_attrs.extend(new_attrs);
                    }
                }
                for arg in args {
                    state.add_parameter(s.clone(), arg.clone())?;
                }
                Ok((rules, req_attrs))
            },
            Rule::Generate(s, args, rhs) => {
                let mut rules = Vec::new();
                let mut req_attrs = ReqAttributes::new();
                for (i, alt) in rhs.iter_mut().enumerate() {
                    for (j, chunk) in alt.node.chunks.iter_mut().enumerate() {
                        let ident = format!("{s}·{i}·{j}");
                        let (chunk_node, span) = chunk.deconstruct();
                        let (new_rules, new_attrs) = chunk_node.rewrite(ident, span, Rule::Analytic, depth, state)?;
                        rules.extend(new_rules);
                        req_attrs.extend(new_attrs);
                    }
                }
                for arg in args {
                    state.add_parameter(s.clone(), arg.clone())?;
                }
                Ok((rules, req_attrs))
            },
            Rule::Import(..) => todo!(),
            Rule::Exclude(..) => todo!(),
        }
    }
}

use wagon_utils::comma_separated;
use itertools::Itertools;
impl Display for Rule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Analytic(s, args, rhs) => {
                if args.is_empty() {
                    writeln!(f, "{s} -> {};", rhs.iter().join(" | "))
                } else {
                    writeln!(f, "{s}<{}> -> {};", comma_separated(args), rhs.iter().join(" | "))
                }
            },
            Self::Generate(s, args, rhs) => {
                if args.is_empty() {
                    writeln!(f, "{s} <= {};", rhs.iter().join(" | "))
                } else {
                    writeln!(f, "{s}<{}> <= {};", comma_separated(args), rhs.iter().join(" | "))
                }
            },
            Self::Import(s1, imp, s2) => {
                writeln!(f, "{s1} {imp} {s2};")
            },
            Self::Exclude(s, ex) => {
                writeln!(f, "{s} </ {}", ex.iter().join(" & "))
            },
        }
    }
}