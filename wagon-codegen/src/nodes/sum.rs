use crate::SpannableIdent;

use super::{Rc, ToTokensState};
use proc_macro2::{TokenStream, Ident};
use quote::quote;

use wagon_parser::parser::sum::{Sum, SumP};

impl<U> ToTokensState<U> for Sum {
    fn to_tokens(&self, state: &mut U, label: Rc<Ident>, attr_fun: fn(&mut U, Rc<Ident>, SpannableIdent)) -> TokenStream {
        let left = self.left.to_tokens(state, label.clone(), attr_fun);
        if let Some(sump) = &self.cont {
            let cont = sump.to_tokens(state, label, attr_fun);
            quote!(#left #cont)
        } else {
            left
        }
    }
}

impl<U> ToTokensState<U> for SumP {
    fn to_tokens(&self, state: &mut U, label: Rc<Ident>, attr_fun: fn(&mut U, Rc<Ident>, SpannableIdent)) -> TokenStream {
        let op = &self.op;
        let right = self.right.to_tokens(state, label.clone(), attr_fun);
        if let Some(cont) = &self.cont {
            let cont_stream = cont.right.to_tokens(state, label, attr_fun);
            quote!(#op #right #cont_stream)
        } else {
            quote!(#op #right)
        }
    }
}
