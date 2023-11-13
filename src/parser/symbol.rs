use std::fmt::Display;
use std::write;

use super::helpers::between_sep;
use super::ast::{ToAst, WagNode};
use super::{Parse, PeekLexer, ParseResult, Tokens, WagParseError};
use crate::lexer::{math::Math, productions::Productions, UnsafeNext, UnsafePeek, Spannable};

use super::terminal::Terminal;
use super::assignment::Assignment;
use super::Ident;

#[derive(PartialEq, Debug, Eq, Hash, Clone)]
pub(crate) enum Symbol {
	NonTerminal(Ident, Vec<Ident>),
	Assignment(Vec<Assignment>),
	Terminal(Terminal),
    Epsilon
}

impl Parse for Symbol {
    fn parse(lexer: &mut PeekLexer) -> ParseResult<Self> where Self: Sized {
        match lexer.peek_unwrap() {
        	Tokens::ProductionToken(Productions::Identifier(_)) => {
        		if let Tokens::ProductionToken(Productions::Identifier(x)) = lexer.next_unwrap() {
                    let args = if let Some(Ok(Tokens::ProductionToken(Productions::LPar))) = lexer.peek() {
                        between_sep(lexer, Tokens::ProductionToken(Productions::LPar), Tokens::ProductionToken(Productions::RPar), Tokens::ProductionToken(Productions::Comma))?
                    } else {
                        Vec::new()
                    };
        			Ok(Self::NonTerminal(x, args))
        		} else { 
        			Err(WagParseError::Fatal((lexer.span(), "Something went terribly wrong. Unwrapped non-identifier when should have unwrapped identifier".to_string()))) 
        		}
        	},
        	Tokens::ProductionToken(Productions::LCur) => {
                lexer.next();
                Ok(Self::Assignment(Assignment::parse_sep_end(lexer, Tokens::MathToken(Math::Semi), Tokens::MathToken(Math::RCur))?))
        	},
        	_ => Ok(Self::Terminal(Terminal::parse(lexer)?))
        }
    }
}

impl Default for Symbol {
    fn default() -> Self {
        Self::Epsilon
    }
}

impl Symbol {

    pub(crate) fn is_terminal(&self) -> bool {
        matches!(self, Self::Terminal(_) | Self::Assignment(_) | Self::Epsilon)
    }

    pub(crate) fn is_assignment(&self) -> bool {
        matches!(self, Self::Assignment(_))
    }

    pub(crate) fn simple_terminal(ident: &str) -> Self {
        Self::Terminal(Terminal::LitString(ident.to_string()))
    }

    pub (crate) fn simple_ident(ident: &str) -> Self {
        Self::NonTerminal(Ident::Unknown(ident.to_string()), Vec::new())
    }
}

impl ToAst for Symbol {
    fn to_ast(self, ast: &mut super::ast::WagTree) -> super::ast::WagIx {
        match self {
            Symbol::NonTerminal(i, _) => ast.add_node(WagNode::Ident(i)),
            Symbol::Terminal(t) => ast.add_node(WagNode::Terminal(t)),
            Symbol::Assignment(v) => {let node = WagNode::Assignments; Self::add_vec_children(node, v, ast)},
            Symbol::Epsilon => ast.add_node(WagNode::Empty)
        }
    }
}

impl Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Symbol::NonTerminal(i, _) => write!(f, "{}", i),
            Symbol::Assignment(i) => write!(f, "{}", i.iter().map(|x| x.to_string()).collect::<Vec<_>>().join("; ")),
            Symbol::Terminal(i) => write!(f, "{}", i),
            Symbol::Epsilon => write!(f, "ε"),
        }
    }
}