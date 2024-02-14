use crate::SpannableIdent;

use super::{Rc, ToTokensState};
use proc_macro2::{TokenStream, Ident};
use quote::quote;

use wagon_parser::parser::factor::Factor;

impl<U> ToTokensState<U> for Factor {
    fn to_tokens(&self, state: &mut U, label: Rc<Ident>, attr_fun: fn(&mut U, Rc<Ident>, SpannableIdent)) -> TokenStream {
        match self {
            Self::Primary(p) => p.to_tokens(state, label, attr_fun),
            Self::Power { left, right } => {
                let left_stream = left.to_tokens(state, label.clone(), attr_fun);
                let right_stream = right.to_tokens(state, label, attr_fun);
                quote!(wagon_value::Pow::pow(#left_stream, #right_stream)?)
            },
        }
    }
}