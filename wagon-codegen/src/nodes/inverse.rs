use quote::quote;
use crate::SpannableIdent;

use super::{Rc, ToTokensState};
use proc_macro2::{TokenStream, Ident};
use wagon_parser::parser::inverse::Inverse;

impl<U> ToTokensState<U> for Inverse {
    fn to_tokens(&self, state: &mut U, label: Rc<Ident>, attr_fun: fn(&mut U, Rc<Ident>, SpannableIdent)) -> TokenStream {
        match self {
            Self::Not(i) => {
                let i_stream = i.to_tokens(state, label, attr_fun);
                quote!(
                    !#i_stream
                )
            },
            Self::Comparison(c) => {
                c.to_tokens(state, label, attr_fun)
            },
        }
    }
}