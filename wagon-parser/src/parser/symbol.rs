
use crate::firstpass::GetReqAttributes;
use crate::firstpass::ReqAttributes;
use crate::parser::{Span, Spannable};
use std::fmt::Display;
use std::write;

use super::helpers::between_sep;
use super::CallingArgs;
use super::{LexerBridge, Parse, ParseResult, Peek, ResultPeek, SpannableNode, Tokens};
use wagon_lexer::{math::Math, productions::Productions};

use super::terminal::Terminal;
use super::assignment::Assignment;
use super::Ident;

use wagon_macros::new_unspanned;

#[derive(PartialEq, Debug, Eq, Hash, Clone)]
#[new_unspanned]
/// A symbol in a [`Chunk`][super::chunk::Chunk].
///
/// A symbol is any individual element of a `Chunk`.
///
/// # Grammar
/// ```text
/// Symbol -> [Ident] NTArgs?
///        |  [Terminal]
///        |  ("{" [Assignment]* "}")?
///        |  
///        ;
/// ```
pub enum Symbol {
    /// A non-terminal with optional parameters.
	NonTerminal(SpannableNode<Ident>, Vec<SpannableNode<Ident>>),
    /// A list of [`Assignment`] enclosed by `{}`.
	Assignment(Vec<SpannableNode<Assignment>>),
    /// A Terminal.
	Terminal(SpannableNode<Terminal>),
    /// Nothing.
    Epsilon
}

impl Parse for Symbol {
    fn parse(lexer: &mut LexerBridge) -> ParseResult<Self> where Self: Sized {
        match lexer.peek_result()? {
        	Tokens::ProductionToken(Productions::Identifier(_)) => {
                let ident = SpannableNode::parse(lexer)?;
                let args = if lexer.peek() == Some(&Ok(Tokens::ProductionToken(Productions::Lt))) {
                    between_sep(lexer, &Tokens::ProductionToken(Productions::Lt), &Tokens::ProductionToken(Productions::Gt), Tokens::ProductionToken(Productions::Comma))?
                } else {
                    Vec::new()
                };
    			Ok(Self::NonTerminal(ident, args))
        	},
        	Tokens::ProductionToken(Productions::LCur) => {
                lexer.next();
                Ok(Self::Assignment(SpannableNode::parse_sep_end(lexer, Tokens::MathToken(Math::Semi), Tokens::MathToken(Math::RCur))?))
        	},
        	_ => Ok(Self::Terminal(SpannableNode::parse(lexer)?))
        }
    }
}

impl Default for Symbol {
    fn default() -> Self {
        Self::Epsilon
    }
}

impl Symbol {

    /// Check if this symbol is not a non-terminal.
    #[must_use] 
    pub const fn is_terminal(&self) -> bool {
        matches!(self, Self::Terminal(..) | Self::Assignment(..) | Self::Epsilon)
    }

    /// Check if this symbol is an [`Assignment`].
    #[must_use] 
    pub const fn is_assignment(&self) -> bool {
        matches!(self, Self::Assignment(..))
    }

    /// Create a symbol which is just a [`Terminal::LitString`] representing the input parameter.
    #[must_use] 
    pub fn simple_terminal(ident: &str) -> Self {
        Self::Terminal(SpannableNode::new(Terminal::LitString(ident.to_string()), 0..ident.len()))
    }

    /// Create a symbol which is just a non-terminal [`Ident::Unknown`] with no arguments, representing the input parameter.
    #[must_use] 
    pub fn simple_ident(ident: &str) -> Self {
        Self::NonTerminal(SpannableNode::new(Ident::Unknown(ident.to_string()), 0..ident.len()), Vec::new())
    }

    /// Create a symbol which is just a spanned non-terminal [`Ident::Unknown`].
    // pub(crate) fn simple_ident_spanned(ident: &str, span: Span) -> SpannableNode<Self> {
    //     Self::simple_ident_spanned_with_args(ident, span, Vec::new())
    // }

    pub(crate) fn simple_ident_spanned_with_args(ident: &str, span: Span, args: Vec<SpannableNode<Ident>>) -> SpannableNode<Self> {
        SpannableNode::new(Self::NonTerminal(SpannableNode::new(Ident::Unknown(ident.to_string()), span.clone()), args), span)
    }

    pub(crate) fn rewrite(&mut self) -> ReqAttributes {
        match self {
            Self::NonTerminal(_, v) => {
                let mut req = ReqAttributes::with_capacity(v.len());
                for i in v.iter_mut() {
                    match &i.node {
                        Ident::Inherit(s) | Ident::Local(s) | Ident::Unknown(s) => { // This happens in EBNF rewrites. Every change in EBNF created rules must be passed up.
                            i.node = Ident::Synth(s.clone());
                        },
                        Ident::Synth(_) => {}
                    }
                    if !req.contains(i) {
                        req.insert(i.clone());
                    }
                }
                req
            },
            Self::Assignment(v) => {
                let mut req = ReqAttributes::new();
                for a in v {
                    for i in a.get_req_attributes() {
                        let string = i.node.extract_string();
                        let to_insert = SpannableNode::new(Ident::Synth(string.to_owned()), i.span());
                        req.insert(to_insert);
                    }
                }
                req
            }
            _ => ReqAttributes::new()
        }
    }

    pub(crate) fn calling_args(&self) -> CallingArgs {
        match self {
            Self::NonTerminal(_, v) => v.clone(),
            Self::Assignment(v) => {
                let mut req = ReqAttributes::new();
                for a in v {
                    req.extend(a.get_req_attributes());
                }
                req.into_iter().collect()
            }
            _ => CallingArgs::new()
        }
    }
}

impl GetReqAttributes for Symbol {
    fn get_req_attributes(&self) -> ReqAttributes {
        self.calling_args().into_iter().collect()
    }
}

use itertools::Itertools;
impl Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NonTerminal(i, args) => {
                if args.is_empty() {
                    write!(f, "{i}")
                } else {
                    write!(f, "{i}<{}>", args.iter().join(", "))
                }
            }
            Self::Assignment(i) => write!(f, "{{{}}}", i.iter().join("; ")),
            Self::Terminal(i) => write!(f, "{i}"),
            Self::Epsilon => write!(f, "ε"),
        }
    }
}