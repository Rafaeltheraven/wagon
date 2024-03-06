#![warn(missing_docs)]
//! Code to create a GLL parser from a WAG
//!
//! This (and the associated [`wagon-gll`](../wagon_gll/index.html) crate) can be seen as examples of
//! what the WAGon ecosystem can do. 
//!
//! This crate holds code specifically for creating a GLL parser out of a [`WAG`](wagon_parser::parser::wag::Wag),
//! as opposed to the [`wagon_codegen`] crate which holds generic code to help with any codegen, and is intended for internal use.

/// Various implementations of codegen on the necessary [`wagon_parser::parser`] nodes.
mod nodes;
/// Module for [`CodeGenState`], a struct which keeps state during codegen.
mod state;

use crate::state::CodeGenState;
use std::{rc::Rc, collections::HashSet};
use proc_macro2::{Ident, Literal, TokenStream};
use quote::quote;
use indexmap::IndexSet;

use wagon_parser::parser::WagParseError;
use wagon_parser::parser::{wag::Wag, atom::Atom};
use wagon_parser::{Span, ErrorReport};
use wagon_codegen::{FileStructure, SpannableIdent};
use wagon_value::{RecursiveValue, ValueError};
use wagon_utils::ConversionError;

#[derive(Debug)]
pub(crate) enum CharBytes {
	Epsilon,
	Bytes(Literal)
}

type FirstSet = (Vec<TokenStream>, Option<CharBytes>);

type AttrSet = HashSet<SpannableIdent>;
type ReqCodeAttrs = AttrSet;
type ReqWeightAttrs = AttrSet;
type ReqFirstAttrs = AttrSet;

type Alt = usize;
type Block = usize;
type Symbol = usize;
type BlockSize = usize;

type PrevArgs = Vec<SpannableIdent>;
type FullArgs = IndexSet<SpannableIdent>;

#[derive(Default)]
#[allow(clippy::struct_excessive_bools)]
/// Configuration options for the final GLL parser.
pub(crate) struct WeightConfig {
	/// Ignore the first/follow-set when choosing possible alternatives to parse.
	no_first: bool,
	/// Don't remove alternatives based on weight.
	///
	/// Note that setting this to true essentially makes the final parser a normal, if less efficient, GLL parser.
	/// Alternatives are selected solely on the first/follow set (unless that is disabled as well).
	/// The weights are calculated and attached to the final parse nodes though.
	no_prune: bool,
	/// Choose alternatives with the lowest weight as opposed to the highest.
	min_weight: bool,
	/// Allow 0-ed weights
	allow_zero: bool
}

#[derive(Default)]
/// Arguments to pass along to codegen functions.
///
/// Because Rust does not allow for variable arguments, but we still need a trait to implement on the nodes, we pass
/// along this struct for any arguments we need from higher up in the chain.
pub(crate) struct CodeGenArgs {
	/// The current state of the codegen.
	pub(crate) state: CodeGenState,
	/// Is this the first symbol of a rule?
	pub(crate) fst: Option<bool>,
	/// The identifier we are currently working with.
	pub(crate) ident: Option<Rc<Ident>>,
	/// The numeric identifier of the alternative we are working on.
	pub(crate) alt: Option<Alt>,
	/// The numeric identifier of the GLLBlock inside the alternative we are working on.
	pub(crate) block: Option<Block>,
	/// The numberic identifier of the symbol inside the block we are working on.
	pub(crate) symbol: Option<Symbol>,
	/// The name of the label.
	pub(crate) label: Option<Rc<Ident>>,
	/// How big the current block is.
	pub(crate) block_size: Option<BlockSize>,
	/// Whether we can stop building a firstset for this label.
	pub(crate) found_first: Option<bool>,
	/// All the arguments needed for this rule.
	pub(crate) full_args: Option<FullArgs>,
	/// The arguments we encountered previously.
	pub(crate) prev_args: Option<PrevArgs>,
	/// Various config options.
	pub(crate) weight_config: WeightConfig
}

#[derive(Debug)]
/// Enum for all errors that can occur during codegen.
pub(crate) enum CodeGenErrorKind {
	/// Tried converting an [`Atom`] into a [`RecursiveValue`]
	///
	/// (this happens in [`nodes::metadata::Metadata::gen`]).
	AtomConversionError(ConversionError<Atom, RecursiveValue>),
	/// Encountered some kind of [`ValueError`] when dealing with [`RecursiveValue`]s.
	ValueError(ValueError<RecursiveValue>),
	/// Some fatal error occurred for which we have no specific variant.
	Fatal(String),
	/// An arg was expected from [`CodeGenArgs`] but it was unset.
	MissingArg(String),
	/// Expected to have a first set for some ident in [`CodeGenState`] but we do not.
	MissingFirst(Rc<Ident>),
	/// An error occurred in [`wagon_parser`].
	ParseError(WagParseError),
	/// An error occurred while prettifying [`TokenStream`].
	SynParseError(syn::parse::Error)
}

#[derive(Debug)]
/// Struct for errors that occur during codegen, as well as their span.
pub struct CodeGenError {
	/// The type of error that occurred (see [`CodeGenErrorKind`]).
	kind: CodeGenErrorKind,
	/// Span for where the error occurred.
	span: Span
}

impl CodeGenError {
	fn new(kind: CodeGenErrorKind) -> Self {
		Self {kind, span: Span::default()}
	}

	const fn new_spanned(kind: CodeGenErrorKind, span: Span) -> Self {
		Self {kind, span}
	}
}

impl ErrorReport for CodeGenError {
    fn span(self) -> Span {
        match self.kind {
        	CodeGenErrorKind::ParseError(e) => e.span(),
        	_ => self.span
        }
    }

    fn msg(&self) -> (String, String) {
        match &self.kind {
        	CodeGenErrorKind::AtomConversionError(e) => ("Conversion Error".to_string(), e.to_string()),
            CodeGenErrorKind::ValueError(e) => ("Value Error".to_string(), e.to_string()),
            CodeGenErrorKind::Fatal(s) => ("Fatal Error".to_string(), s.to_owned()),
            CodeGenErrorKind::MissingArg(s) => ("Missing Argument".to_string(), format!("Expected to see {s} but it was None")),
            CodeGenErrorKind::MissingFirst(i) => ("Missing First Set".to_string(), format!("Expected to have one for {i} but it was None")),
            CodeGenErrorKind::ParseError(e) => e.msg(),
            CodeGenErrorKind::SynParseError(e) => ("Error parsing TokenStream".to_string(), e.to_string()),
        }
    }

    fn source(&self) -> Option<String> {
        match &self.kind {
            CodeGenErrorKind::SynParseError(e) => e.span().source_text(), // This doesn't work as expected
            _ => None
        }
    }
}

impl std::fmt::Display for CodeGenError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    	let (head, text) = self.msg();
        write!(f, "{head}: {text}")
    }
}

impl std::error::Error for CodeGenError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match &self.kind {
            CodeGenErrorKind::AtomConversionError(e) => Some(e),
            CodeGenErrorKind::ValueError(e) => Some(e),
            CodeGenErrorKind::SynParseError(e) => Some(e),
            _ => None
        }
    }
}

impl From<ConversionError<Atom, RecursiveValue>> for CodeGenErrorKind {
    fn from(value: ConversionError<Atom, RecursiveValue>) -> Self {
        Self::AtomConversionError(value)
    }
}

impl From<ValueError<RecursiveValue>> for CodeGenErrorKind {
    fn from(value: ValueError<RecursiveValue>) -> Self {
        Self::ValueError(value)
    }
}

impl From<WagParseError> for CodeGenError {
    fn from(value: WagParseError) -> Self {
        Self::new(CodeGenErrorKind::ParseError(value))
    }
}

impl From<syn::parse::Error> for CodeGenError {
    fn from(value: syn::parse::Error) -> Self {
        Self::new(CodeGenErrorKind::SynParseError(value))
    }
}

type CodeGenResult<T> = Result<T, CodeGenError>;

trait CodeGen {
	fn gen(self, gen_args: &mut CodeGenArgs) -> CodeGenResult<()>;
}

/// Given a [`Wag`], create a GLL parser and return a [`FileStructure`] representing this parser.
///
/// # Errors
/// Will return a [`CodeGenError`] if anything goes wrong during codegen (usually unwrapping things).
pub fn gen_parser(wag: Wag) -> CodeGenResult<FileStructure> {
	let mut args = CodeGenArgs::default();
	wag.gen(&mut args)?;
	let mut state = args.state;
	let (mut fs, start) = state.gen_struct_stream()?;
	state.gen_nt_stream(&mut fs)?;
	let main = state.gen_state_stream(&mut fs)?;
	let fin = quote!(
		mod nonterminals;
		#start
		#main
	);
	fs.insert_tokenstream("main.rs", fin, true)?;
	Ok(fs)
}