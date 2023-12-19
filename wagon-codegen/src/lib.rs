pub mod nodes;
pub mod value;

use std::{rc::Rc, collections::HashMap};
use proc_macro2::{TokenStream, Ident};
use wagon_parser::{parser::Parse, SpannableNode};

pub type CodeMap = (HashMap<String, Vec<(String, TokenStream)>>, TokenStream);
pub type SpannableIdent = SpannableNode<wagon_ident::Ident>;

pub trait ToTokensState<U> {
	fn to_tokens(&self, state: &mut U, label: Rc<Ident>, attr_fun: fn(&mut U, Rc<Ident>, SpannableIdent)) -> TokenStream;
}

impl<U, T: Parse + ToTokensState<U>> ToTokensState<U> for SpannableNode<T> {
    fn to_tokens(&self, state: &mut U, label: Rc<Ident>, attr_fun: fn(&mut U, Rc<Ident>, SpannableIdent)) -> TokenStream {
        self.to_inner().to_tokens(state, label, attr_fun)
    }
}