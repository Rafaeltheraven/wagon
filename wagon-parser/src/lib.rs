#![warn(missing_docs)]
//! WAGon Parser
//!
//! A crate containing the [`Parser`] for the WAGon DSL as well as a checker and associated functions. 
//! As long as you do not need any extensions to the WAGon DSL itself, this will likely be your main interface to the ecosystem. 
//! After you have a parsed a full [`Wag`] tree, you can do with it whatever you require.

/// The parser
pub mod parser;
/// The checker
pub mod firstpass;

use std::collections::BTreeMap;
use std::fmt::Display;

use firstpass::GetReqAttributes;
use wagon_lexer::LexerBridge;
use wagon_utils::{Span, Spannable};
use crate::parser::{Parser, Parse, ParseResult, wag::Wag};
use crate::firstpass::{FirstPassState, Rewrite};

/// Parse an input string and check if the resulting WAG is valid.
///
/// Given the input string, will either return a full, rewritten, WAG or an error.
///
/// # Errors 
/// Returns a [`WagParseError`](`crate::parser::WagParseError`) if any error occurs during the parsing or checking stage.
pub fn parse_and_check(date: &str) -> ParseResult<Wag> {
    let mut parser = Parser::new(date);
    let mut wag = parser.parse()?;
    let mut state = FirstPassState::default();
    wag.rewrite(0, &mut state)?;
    Ok(wag)
}

/// A node is anything that implements [`Parse`]. `SpannableNode` then, is a wrapper around this node that holds span information about it.
/// It is intended to be a mostly see-through wrapper around whatever the inner node is. [`Parse`] is implemented on it in a way that
/// automatically calculates the span information.
#[derive(Debug, Clone)]
pub struct SpannableNode<T: Parse> {
	node: T,
	span: Span
}

impl<T: Parse + PartialEq> PartialEq for SpannableNode<T> {
    fn eq(&self, other: &Self) -> bool {
        self.node.eq(&other.node)
    }
}

impl<T: Parse + Eq> Eq for SpannableNode<T> {}

impl<T: Parse + std::hash::Hash> std::hash::Hash for SpannableNode<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.node.hash(state);
    }
}

impl<T: Parse + Display> Display for SpannableNode<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.node.fmt(f)
    }
}

impl<T: Parse> From<T> for SpannableNode<T> {
    fn from(value: T) -> Self {
        Self::new(value, Span::default())
    }
}

impl<T: Parse> SpannableNode<T> {
    /// Get the inner node, consuming the wrapper.
	pub fn into_inner(self) -> T {
		self.node
	}

    /// Get a reference to the inner node.
	pub const fn to_inner(&self) -> &T {
		&self.node
	}

    /// Get a mutable reference to the inner node.
    pub fn to_inner_mut(&mut self) -> &mut T {
        &mut self.node
    }

	const fn new(node: T, span: Span) -> Self {
		Self {node, span}
	}

    /// Get mutable references to both the inner node and the span.
	pub fn deconstruct(&mut self) -> (&mut T, &mut Span) {
		(&mut self.node, &mut self.span)
	}
}

/// A trait for internal use to automatically convert between nodes and [`SpannableNode`].
pub trait WrapSpannable<T: Parse, U> {
    /// Wrap dummy span information around the node.
	fn wrap_spannable(self) -> U;

    /// Convert the node into a [`SpannableNode`] with the specified [`Span`].
	fn into_spanned(self, _span: Span) -> U where Self: Sized {
		unimplemented!()
	}
}

impl<T: Parse, U: IntoIterator<Item = SpannableNode<T>> + FromIterator<SpannableNode<T>>, Y: IntoIterator<Item = T>> WrapSpannable<T, U> for Y {
	fn wrap_spannable(self) -> U {
		self.into_iter().map(WrapSpannable::wrap_spannable).collect()
	}
} 

impl<T: Parse> WrapSpannable<T, Option<SpannableNode<T>>> for Option<T> {
    fn wrap_spannable(self) -> Option<SpannableNode<T>> {
		self.map(WrapSpannable::wrap_spannable)
	}
}

impl<T: Parse> WrapSpannable<T, Option<Box<SpannableNode<T>>>> for Option<Box<T>> {
    fn wrap_spannable(self) -> Option<Box<SpannableNode<T>>> {
        self.map(WrapSpannable::wrap_spannable)
    }
}

impl<T: Parse> WrapSpannable<T, Box<SpannableNode<T>>> for Box<T> {
    fn wrap_spannable(self) -> Box<SpannableNode<T>> {
        Box::new(SpannableNode::from(*self))
    }
}

impl<T: Parse> WrapSpannable<T, SpannableNode<T>> for T {
    fn wrap_spannable(self) -> SpannableNode<T> {
        SpannableNode::from(self)
    }

    fn into_spanned(self, span: Span) -> SpannableNode<T> {
        SpannableNode::new(self, span)
    }
}

impl<T: Parse, U: std::cmp::Ord> WrapSpannable<T, BTreeMap<U, SpannableNode<T>>> for BTreeMap<U, T> {
    fn wrap_spannable(self) -> BTreeMap<U, SpannableNode<T>> {
        self.into_iter().map(|(x, y)| (x, y.wrap_spannable())).collect()
    }
} 

// impl<T: Parse> Deref for SpannableNode<T> {
//     type Target = T;

//     fn deref(&self) -> &Self::Target {
//         &self.node
//     }
// }

impl<T: Parse> Spannable for SpannableNode<T> {
    fn span(&self) -> Span {
        self.span.clone()
    }

    fn set_span(&mut self, span: Span) {
    	self.span = span;
    }
}

impl<T: Parse + std::fmt::Debug> Parse for SpannableNode<T> {
    fn parse(lexer: &mut LexerBridge) -> ParseResult<Self> where Self: Sized {
    	let start = lexer.span().start;
    	let node = T::parse(lexer)?;
    	let span = start..lexer.span().end;
        Ok(Self::new(node, span))
    }
}

impl<U, T: Parse + Rewrite<U>> Rewrite<U> for SpannableNode<T> {
    fn rewrite(&mut self, depth: usize, state: &mut crate::firstpass::FirstPassState) -> crate::firstpass::FirstPassResult<U> {
        self.node.rewrite(depth, state)
    }
}

impl<T: Parse + quote::ToTokens> quote::ToTokens for SpannableNode<T> {
    fn to_tokens(&self, tokens: &mut quote::__private::TokenStream) {
        self.node.to_tokens(tokens);
    }
}

impl<T: Parse + GetReqAttributes> GetReqAttributes for SpannableNode<T> {
    fn get_req_attributes(&self) -> firstpass::ReqAttributes {
        self.to_inner().get_req_attributes()
    }
}

#[test]
fn test_generative_and_analytic() {
    let input = r#"type: analytical;
=========================================

S -> "hello" | A;
S => "hi";
A -> "a";"#;
    if let Ok(output) = parse_and_check(input){
        println!("{}", output);
    };

}
