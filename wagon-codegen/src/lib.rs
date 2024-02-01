#![warn(missing_docs)]
//! Generally useful methods to convert a WAG to Rust code.
//!
//! A WAG may be converted into code in any number of ways for any number of reasons. This crate intends to hold
//! code that is generally useful, regardless of the purpose of the final generated code.

/// Codegen for parts of the [`wagon-parser`] AST that are generally useful.
pub mod nodes;
/// A datastructure that represents a structure and data to write to disk.
mod filestructure;

use std::rc::Rc;
use proc_macro2::{TokenStream, Ident};
use wagon_parser::{parser::Parse, SpannableNode};
pub use filestructure::{FileStructure, FileType};

/// A quick type alias to represent a [`wagon_ident::Ident`] with span information.
pub type SpannableIdent = SpannableNode<wagon_ident::Ident>;

/// A trait for anything that does codegen while having to keep track of some state object `U`.
pub trait ToTokensState<U> {
    /// A method to do codegen based on state and possibly modify that state.
    ///
    /// This method takes a given mutable state `U`, as well as a reference to some [`proc_macro2::Ident`] and a function to modify the state.
    /// It then potentially calls this function to modify the state and returns a `TokenStream` for the generated code.
    ///
    /// # Example
    /// ```
    /// # use wagon_parser::SpannableNode;
    /// # use proc_macro2::TokenStream;
    /// # use wagon_codegen::{SpannableIdent, ToTokensState};
    /// # use std::rc::Rc;
    /// struct A;
    /// struct State { toggle: bool }
    /// impl State {
    ///     fn new() -> Self {
    ///         Self { toggle: false }  
    ///     }
    ///     fn toggle(&mut self, _: Rc<proc_macro2::Ident>, _: SpannableIdent) {
    ///         self.toggle = true;
    ///     }   
    /// }
    /// impl ToTokensState<State> for A {
    ///     fn to_tokens(&self, state: &mut State, label: Rc<proc_macro2::Ident>, attr_fun: fn(&mut State, Rc<proc_macro2::Ident>, SpannableIdent)) -> TokenStream {
    ///         attr_fun(state, label, SpannableNode::from(wagon_ident::Ident::default()));
    ///         TokenStream::new()
    ///     }
    /// }
    /// let a = A;
    /// let mut state = State { toggle: false };
    /// let label = Rc::new(proc_macro2::Ident::new("label", proc_macro2::Span::call_site()));
    /// a.to_tokens(&mut state, label, State::toggle);
    /// assert!(state.toggle) // this is now true
    /// ```
    ///
    /// For practical examples. Look at the implementations in this crate as well as how they are called in the [`wagon-codegen-gll`] crate.
	fn to_tokens(&self, state: &mut U, label: Rc<Ident>, attr_fun: fn(&mut U, Rc<Ident>, SpannableIdent)) -> TokenStream;
}

impl<U, T: Parse + ToTokensState<U>> ToTokensState<U> for SpannableNode<T> {
    fn to_tokens(&self, state: &mut U, label: Rc<Ident>, attr_fun: fn(&mut U, Rc<Ident>, SpannableIdent)) -> TokenStream {
        self.to_inner().to_tokens(state, label, attr_fun)
    }
}