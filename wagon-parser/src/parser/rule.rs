use std::fmt::Display;

use super::{helpers::{check_semi, between_sep}, LexerBridge, Parse, ParseResult, Peek, ResultNext, Rewrite, SpannableNode, Spannable, Tokens, WagParseError};
use wagon_lexer::productions::{ImportType, Productions};
use crate::firstpass::{FirstPassResult, FirstPassState, ReqAttributes};

use super::rhs::Rhs;
use super::Ident;

use wagon_macros::match_error;

use wagon_macros::new_unspanned;

#[derive(PartialEq, Debug, Eq, Hash)]
#[new_unspanned]
/// A single rule in the WAG grammar. 
///
/// # Grammar
/// <span><pre>
/// [Rule] -> [Ident] RuleType;
/// RuleType -> RealRule | ImportRule;
/// RealRule -> NTArgs? RuleArrow [Rhs];
/// RuleArrow -> `"->" | "=>"`;
/// ImportRule -> ImportArrow Identifier;
/// ImportArrow -> `"<-" | "<=" | "<<" | "</"`;
///
/// NTArgs -> "<" AttrIdentifierList ">";
/// AttrIdentifierList -> [Ident] "," AttrIdentifierList | [Ident];
/// </pre></span>
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
                let mut gen_ident: String = String::from("GEN_");
                gen_ident.push_str(&ident);
                let rhs = SpannableNode::parse_sep(lexer, Tokens::ProductionToken(Productions::Alternative))?;
                Ok(Self::Generate(gen_ident, args, rhs))
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
                    [··{depth}]+ - Deeper layers of recursive EBNF
                               - - Default again but at this layer

*/
/// Convert every [`Chunk`](super::chunk::Chunk) inside a group or with an ebnf operator into it's own separate rule.
///
/// # Rewrite rules
/// ## `?`
/// |  original  |             rewrite             |
/// |:----------:|:-------------------------------:|
/// | `A -> B?;` | `A -> A·0·0;` `A·0·0 -> B \| ;` |
/// ## `*`
/// |  original  |                rewrite                |
/// |:----------:|:-------------------------------------:|
/// | `A -> B*;` | `A -> A·0·0;` `A·0·0 -> B A·0·0 \| ;` |
/// ## `+`
/// |  original  |                             rewrite                             |
/// |:----------:|:---------------------------------------------------------------:|
/// | `A -> B+;` | `A -> A·0·0;` `A·0·0 -> B A·0·0·p;` `A·0·0·p -> B A·0·0·p \| ;` |
/// ## `()` (groups)
/// |   original  |           rewrite           |
/// |:-----------:|:---------------------------:|
/// | `A -> (B);` | `A -> A·0·0;` `A·0·0 -> B;` |
/// 
/// # Helper rules and the meaning of ·
/// · is a character allowed in identifiers in Rust because of backwards compatible ISO reasons. It is thus
/// a really useful reserved character to make sure no collisions occur with user input.
/// When factoring out EBNF or groups, we want to create helper rules and use the · character to denote the nature of each helper rule.
/// 
/// ## Format of helper rules
/// The format of a helper rule is `{BASE}·{alt}·{chunk}`. {BASE} is the identifier for the rule we are creating
/// the helper for. {alt} is which alternative of the rule we are creating it for, and chunk is the number of the chunk we are rewriting. 
/// 
/// Additionally, we optionally append `·p` specifically as an additional helper rule for `+` operators. 
///
/// ## Recursion
/// Because of groups, it is possible that we need to recursively keep rewriting chunks/rules. The recursion depth is denoted by ··{depth}
/// and when you see it, you know we've gone a layer deeper.
///
/// ## Attributes
/// Any attributes that are passed to a chunk with an ebnf, or that are using inside a grouped chunk, must be available in the helper rules.
/// Additionally, any modifications made to these attributes must be passed upwards as if the helper rules were completely inlined. As such,
/// we pass all the required attributes encountered in the chunks as they were originally written to the first helper rule. From them on, we
/// treat each attribute as synthesized, so that any changes will be properly passed up to the original calling rule. 
impl Rewrite<(Vec<Self>, ReqAttributes)> for SpannableNode<Rule> {
    fn rewrite(&mut self, depth: usize, state: &mut FirstPassState) -> FirstPassResult<(Vec<Self>, ReqAttributes)> {
        match &mut self.node {
            Rule::Analytic(s, args, rhs) => {
                let mut rules = Vec::new(); // All the new rules we create because of rewriting.
                let mut req_attrs = ReqAttributes::new(); // All the attributes required for this rule.
                for (i, alt) in rhs.iter_mut().enumerate() {
                    for (j, chunk) in alt.node.chunks.iter_mut().enumerate() {
                        let ident = format!("{s}·{i}·{j}"); // Construct an identifier for this helper rule.
                        let (chunk_node, span) = chunk.deconstruct();
                        let (new_rules, new_attrs) = chunk_node.rewrite(ident, span, Rule::Analytic, depth, state)?; // Rewrite the chunk.
                        rules.extend(new_rules); // Add any new rules we encountered to the list.
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
                    writeln!(f, "{s} => {};", rhs.iter().join(" | "))
                } else {
                    writeln!(f, "{s}<{}> => {};", comma_separated(args), rhs.iter().join(" | "))
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