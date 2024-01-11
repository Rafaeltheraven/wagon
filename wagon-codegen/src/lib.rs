#![warn(missing_docs)]
//! Generally useful methods to convert a WAG to Rust code.
//!
//! A WAG may be converted into code in any number of ways for any number of reasons. This crate intends to hold
//! code that is generally useful, regardless of the purpose of the final generated code.

/// Codegen for parts of the [`wagon-parser`] AST that are generally useful.
pub mod nodes;
/// Code to deal with typing, addition and general operations on attributes.
pub mod value;

use std::{rc::Rc, collections::HashMap};
use proc_macro2::{TokenStream, Ident};
use wagon_parser::{parser::Parse, SpannableNode};

/// A [`HashMap`] that represents what [`TokenStream`] to write where.  
///
/// The `CodeMap` is a tuple with 2 elements:
/// 1. A `HashMap` from [`String`] to `Vec<(String, TokenStream)`. In this map, the key is the name of the module/folder to write the data to.
///    The tuple then, is a "mapping" for a specific submodule of this module. The `String` represents the name of the file to write to, while the `TokenStream` is the code to write.
/// 2. The `TokenStream` for the `main.rs` file.
///
/// # Example
/// Say we have the following `CodeMap`: `(HashMap::from(("A", vec![("A_0_0", quote!(Foo)), ("A_0_1", quote!(Bar))]), quote!(Baz));`
///
/// This map represents a structure such that we have a `main.rs` file with the code `Baz`. And then a module `A.rs` 
/// which has 2 submodules `A_0_0.rs` and `A_0_1.rs` with the code `Foo` and `Bar` respectively.
///
/// Of course, the actual meaning of this map is up to the implementer. But this should help keep the structure clear for most usecases.
pub type CodeMap = (HashMap<String, Vec<(String, TokenStream)>>, TokenStream);
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
    /// struct A;
    /// struct State { toggle: bool }
    /// impl State {
    ///     fn new() -> Self {
    ///         Self { toggle: false }  
    ///     }
    ///     fn toggle(&mut self, Rc<proc_macro2::Ident>, SpannableIdent) {
    ///         self.toggle = true;
    ///     }   
    /// }
    /// impl ToTokensState<State> for A {
    ///     fn to_tokens(&self, state: &mut State, label: Rc<proc_macro2::Ident>, attr_fun: fn(&mut State, Rc<proc_macro2::Ident>, SpannableIdent)) -> TokenStream {
    ///         attr_fun(state, label, SpannableNode::from(wagon_ident::Ident::default()));
    ///     }
    /// }
    /// let a = A;
    /// let state = State { toggle: false };
    /// let label = proc_macro2::Ident::from("label");
    /// a.to_tokens(state, label, State::toggle);
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