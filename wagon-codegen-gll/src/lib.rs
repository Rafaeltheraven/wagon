#![warn(missing_docs)]
//! Code to create a GLL parser from a WAG
//!
//! This (and the associated [`wagon-gll`](../wagon_gll/index.html) crate) can be seen as examples of
//! what the WAGon ecosystem can do. 
//!
//! This crate holds code specifically for creating a GLL parser out of a [`WAG`](wagon_parser::parser::wag::Wag).
//! As opposed to the [`wagon_codegen`] crate, which holds generic code to help with any codegen and is intended for internal use.

/// Various implementations of codegen on the necessary [`wagon_parser::parser`] nodes.
mod nodes;
/// Module for [`CodeGenState`], a struct which keeps state during codegen.
mod state;

use crate::state::CodeGenState;
use std::{rc::Rc, collections::HashSet};
use proc_macro2::{Ident, Literal};
use quote::quote;
use indexmap::IndexSet;

use wagon_parser::parser::{wag::Wag, atom::Atom};
use wagon_codegen::{SpannableIdent, CodeMap};
use wagon_value::{RecursiveValue, ValueError};
use wagon_utils::ConversionError;

#[derive(Debug)]
pub(crate) enum CharBytes {
	Epsilon,
	Bytes(Literal)
}

type FirstSet = (Vec<SpannableIdent>, Option<CharBytes>);

type AttrSet = HashSet<SpannableIdent>;
type ReqCodeAttrs = AttrSet;
type ReqWeightAttrs = AttrSet;
type ReqFirstAttrs = AttrSet;

#[derive(Default)]
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
	min_weight: bool
}

#[derive(Default)]
/// Arguments to pass along to codegen functions.
///
/// Because Rust does not allow for variable arguments, but we still need a trait to implement on the nodes, we pass
/// along this struct for any arguments we need from higher up in the chain.
pub(crate) struct CodeGenArgs {
	pub(crate) state: CodeGenState,
	pub(crate) fst: Option<bool>,
	pub(crate) ident: Option<Rc<Ident>>,
	pub(crate) alt: Option<usize>,
	pub(crate) block: Option<usize>,
	pub(crate) symbol: Option<usize>,
	pub(crate) label: Option<Rc<Ident>>,
	pub(crate) block_size: Option<usize>,
	pub(crate) found_first: Option<bool>,
	pub(crate) full_args: Option<IndexSet<SpannableIdent>>,
	pub(crate) prev_args: Option<Vec<SpannableIdent>>,
	pub(crate) weight_config: WeightConfig
}

#[derive(Debug)]
/// Enum for all errors that can occur during codegen.
pub enum CodeGenError {
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
}

impl std::fmt::Display for CodeGenError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CodeGenError::AtomConversionError(e) => e.fmt(f),
            CodeGenError::ValueError(e) => e.fmt(f),
            CodeGenError::Fatal(s) => write!(f, "Fatal Error! {}", s),
            CodeGenError::MissingArg(s) => write!(f, "Missing Argument! Expected to see {} but it was None", s),
            CodeGenError::MissingFirst(i) => write!(f, "Missing First Set! Expected to have one for {} but it was None", i),
        }
    }
}

impl std::error::Error for CodeGenError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            CodeGenError::AtomConversionError(e) => Some(e),
            CodeGenError::ValueError(e) => Some(e),
            _ => None
        }
    }
}

impl From<ConversionError<Atom, RecursiveValue>> for CodeGenError {
    fn from(value: ConversionError<Atom, RecursiveValue>) -> Self {
        Self::AtomConversionError(value)
    }
}

impl From<ValueError<RecursiveValue>> for CodeGenError {
    fn from(value: ValueError<RecursiveValue>) -> Self {
        Self::ValueError(value)
    }
}

type CodeGenResult<T> = Result<T, CodeGenError>;

trait CodeGen {
	fn gen(self, gen_args: &mut CodeGenArgs) -> CodeGenResult<()>;
}

/// Given a [`Wag`], create a GLL parser and return a [`CodeMap`] representing this parser.
pub fn gen_parser(wag: Wag) -> CodeGenResult<CodeMap> {
	let mut args = CodeGenArgs::default();
	wag.gen(&mut args)?;
	let state = args.state;
	let (structs, start) = state.gen_struct_stream()?;
	let main = state.gen_state_stream()?;
	let fin = quote!(
		mod terminals;
		#start
		#main
	);
	Ok((structs, fin))
}