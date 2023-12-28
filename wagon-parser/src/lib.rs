pub mod parser;
pub mod firstpass;
mod ast;

use std::fmt::Display;

use wagon_lexer::{Span, Spannable, PeekLexer};
use crate::parser::{Parser, Parse, ParseResult, wag::Wag};
use crate::ast::ToAst;
use crate::firstpass::{FirstPassState, Rewrite};

pub fn parse_and_check(date: &str) -> ParseResult<Wag> {
    let mut parser = Parser::new(date);
    let mut wag = parser.parse()?;
    let mut state = FirstPassState::default();
    wag.rewrite(0, &mut state)?;
    Ok(wag)
}

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
        self.node.hash(state)
    }
}

impl<T: Parse + Display> Display for SpannableNode<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.node.fmt(f)
    }
}

impl<T: Parse> From<T> for SpannableNode<T> {
    fn from(value: T) -> Self {
        Self::new(value, Default::default())
    }
}

impl<T: Parse> SpannableNode<T> {
	pub fn into_inner(self) -> T {
		self.node
	}

	pub fn to_inner(&self) -> &T {
		&self.node
	}

	fn new(node: T, span: Span) -> Self {
		Self {node, span}
	}

	pub fn deconstruct(&mut self) -> (&mut T, &mut Span) {
		(&mut self.node, &mut self.span)
	}
}

pub(crate) trait WrapSpannable<T: Parse, U> {
	fn wrap_spannable(self) -> U;

	fn into_spanned(self, _span: Span) -> U where Self: Sized {
		unimplemented!()
	}
}

impl<T: Parse, U: IntoIterator<Item = SpannableNode<T>> + FromIterator<SpannableNode<T>>, Y: IntoIterator<Item = T>> WrapSpannable<T, U> for Y {
	fn wrap_spannable(self) -> U {
		self.into_iter().map(|x| x.wrap_spannable()).collect()
	}
} 

impl<T: Parse> WrapSpannable<T, Option<SpannableNode<T>>> for Option<T> {
    fn wrap_spannable(self) -> Option<SpannableNode<T>> {
		self.map(|x| x.wrap_spannable())
	}
}

impl<T: Parse> WrapSpannable<T, Option<Box<SpannableNode<T>>>> for Option<Box<T>> {
    fn wrap_spannable(self) -> Option<Box<SpannableNode<T>>> {
        self.map(|x| x.wrap_spannable())
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

// impl<T: Parse> Deref for SpannableNode<T> {
//     type Target = T;

//     fn deref(&self) -> &Self::Target {
//         &self.node
//     }
// }

impl<T: Parse> Spannable for SpannableNode<T> {
    fn span(&mut self) -> Span {
        self.span.to_owned()
    }

    fn set_span(&mut self, span: Span) {
    	self.span = span;
    }
}

impl<T: Parse> Parse for SpannableNode<T> {
    fn parse(lexer: &mut PeekLexer) -> ParseResult<Self> where Self: Sized {
    	let start = lexer.span().start;
    	let node = T::parse(lexer)?;
    	let span = start..lexer.span().end;
        Ok(Self::new(node, span))
    }
}

impl<T: Parse + ToAst> ToAst for SpannableNode<T> {
    fn to_ast(self, ast: &mut crate::ast::WagTree) -> crate::ast::WagIx {
        self.node.to_ast(ast)
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