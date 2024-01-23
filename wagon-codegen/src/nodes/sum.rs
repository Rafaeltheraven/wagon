use crate::SpannableIdent;

use super::{Rc, ToTokensState};
use proc_macro2::{TokenStream, Ident};
use quote::quote;

use wagon_parser::parser::sum::{Sum, SumP};

fn to_tokens_sump<U>(left: &TokenStream, sump: &SumP, state: &mut U, label: Rc<Ident>, attr_fun: fn(&mut U, Rc<Ident>, SpannableIdent)) -> TokenStream {
    let op = &sump.op;
    let right = sump.right.to_tokens(state, label.clone(), attr_fun);
    sump.cont.as_ref().map_or_else(|| quote!(#op (#left, #right)?), |cont| {
        let cont_stream = to_tokens_sump(&right, cont, state, label, attr_fun);
        quote!(#op (#left, #cont_stream)?)
    })
}

impl<U> ToTokensState<U> for Sum {
    fn to_tokens(&self, state: &mut U, label: Rc<Ident>, attr_fun: fn(&mut U, Rc<Ident>, SpannableIdent)) -> TokenStream {
        let left = self.left.to_tokens(state, label.clone(), attr_fun);
        if let Some(sump) = &self.cont {
            to_tokens_sump(&left, sump, state, label, attr_fun)
        } else {
            left
        }
    }
}
