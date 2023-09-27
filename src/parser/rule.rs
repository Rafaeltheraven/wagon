use super::{Parse, PeekLexer, ParseResult, Tokens, WagParseError, ast::ToAst, helpers::check_semi, Rewrite};
use crate::lexer::{ident::Ident, productions::{ImportType, Productions}, UnsafeNext, Spannable};

use super::rhs::Rhs;
use wagon_macros::match_error;

#[derive(PartialEq, Debug, Eq, Hash)]
pub(crate) enum Rule {
	Analytic(String, Vec<Rhs>),
	Generate(String, Vec<Rhs>),
	Import(String, ImportType, String),
	Exclude(String, Vec<String>)
}

#[derive(PartialEq, Debug)]
pub(crate) enum Arrow {
    Analytic,
    Generate,
    Import(ImportType, String),
    Exclude
}

impl Parse for Rule {
    fn parse(lexer: &mut PeekLexer) -> ParseResult<Self> {
        let ident = match_error!(match lexer.next_unwrap() {
        	Tokens::ProductionToken(Productions::Identifier(Ident::Unknown(s))) => Ok(s),
        })?;
        let resp = match_error!(match lexer.next_unwrap() {
        	Tokens::ProductionToken(Productions::Produce) => Ok(Self::Analytic(ident, Rhs::parse_sep(lexer, Tokens::ProductionToken(Productions::Alternative))?)),
        	Tokens::ProductionToken(Productions::Generate) => Ok(Self::Generate(ident, Rhs::parse_sep(lexer, Tokens::ProductionToken(Productions::Alternative))?)),
        	Tokens::ProductionToken(Productions::Import(i)) => {
        		match i {
        			ImportType::Basic | ImportType::Full | ImportType::Recursive => {
        				match lexer.next_unwrap() {
        					Tokens::ProductionToken(Productions::Identifier(Ident::Unknown(s))) => Ok(Self::Import(ident, i, s)),
        					error => Err(WagParseError::Unexpected {span: lexer.span(), offender: error, expected: vec![Tokens::ProductionToken(Productions::Identifier(Default::default())).to_string()]})
        				}
        			}
        			ImportType::Exclude => Ok(Self::Exclude(ident, String::parse_sep(lexer, Tokens::ProductionToken(Productions::Additional))?))
        		}
        	}
        });
        check_semi(lexer)?;
        resp
    }
}

/*
Ident format:

{BASE}-{alt}-{chunk}             - Default
                    -p           - Helper for '+'
                    [^{chunk}]+  - Deeper layers of recursive EBNF
                               - - Default again but at this layer

*/
impl Rewrite<Vec<Rule>> for Rule {
    fn rewrite(&mut self, depth: usize) -> Vec<Rule> {
        match self {
            Rule::Analytic(s, rhs) => {
                let mut rules = Vec::new();
                for (i, alt) in rhs.iter_mut().enumerate() {
                    for (j, chunk) in alt.chunks.iter_mut().enumerate() {
                        let ident = format!("{}-{}-{}", s, i, j);
                        rules.extend(chunk.rewrite(ident, Rule::Analytic, depth));
                    }
                }
                rules
            },
            Rule::Generate(s, rhs) => {
                let mut rules = Vec::new();
                for (i, alt) in rhs.iter_mut().enumerate() {
                    for (j, chunk) in alt.chunks.iter_mut().enumerate() {
                        let ident = format!("{}-{}-{}", s, i, j);
                        rules.extend(chunk.rewrite(ident, Rule::Generate, depth));
                    }
                }
                rules
            },
            Rule::Import(_, _, _) => todo!(),
            Rule::Exclude(_, _) => todo!(),
        }
    }
}

impl ToAst for Rule {
    fn to_ast(self, ast: &mut super::ast::WagTree) -> super::ast::WagIx {
        match self {
            Self::Analytic(s, v) => {
                let node = super::ast::WagNode::Rule(s, Arrow::Analytic);
                Self::add_vec_children(node, v, ast)
            },
            Self::Generate(s, v) => {
                let node = super::ast::WagNode::Rule(s, Arrow::Generate);
                Self::add_vec_children(node, v, ast)
            },
            Self::Import(s, i, s2) => ast.add_node(super::ast::WagNode::Rule(s, Arrow::Import(i, s2))),
            Self::Exclude(s, v) => {
                let node = super::ast::WagNode::Rule(s, Arrow::Exclude);
                Self::add_vec_children(node, v, ast)
            }
        }
    }
}