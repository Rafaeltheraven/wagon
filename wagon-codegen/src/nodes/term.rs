use crate::SpannableIdent;

use super::{Rc, ToTokensState};
use proc_macro2::{TokenStream, Ident};
use quote::quote;

use wagon_parser::parser::term::{Term, TermP};

fn to_tokens_termp<U>(left: &TokenStream, termp: &TermP, state: &mut U, label: Rc<Ident>, attr_fun: fn(&mut U, Rc<Ident>, SpannableIdent)) -> TokenStream {
    let op = &termp.op;
    let right = termp.right.to_tokens(state, label.clone(), attr_fun);
    termp.cont.as_ref().map_or_else(|| quote!(#op (#left, #right)?), |cont| {
        let cont_stream = to_tokens_termp(&right, cont, state, label, attr_fun);
        quote!(#op (#left, #cont_stream)?)
    })
}

impl<U> ToTokensState<U> for Term {
    fn to_tokens(&self, state: &mut U, label: Rc<Ident>, attr_fun: fn(&mut U, Rc<Ident>, SpannableIdent)) -> TokenStream {
        let left = self.left.to_tokens(state, label.clone(), attr_fun);
        if let Some(termp) = &self.cont {
            to_tokens_termp(&left, termp, state, label, attr_fun)
        } else {
            left
        }
    }
}
