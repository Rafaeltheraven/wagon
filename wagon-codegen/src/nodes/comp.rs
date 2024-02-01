use quote::quote;

use crate::SpannableIdent;

use super::{Rc, ToTokensState};
use proc_macro2::{TokenStream, Ident};

use wagon_parser::parser::comp::{Comparison, CompOp};

impl<U> ToTokensState<U> for Comparison {
    fn to_tokens(&self, state: &mut U, label: Rc<Ident>, attr_fun: fn(&mut U, Rc<Ident>, SpannableIdent)) -> TokenStream {
        if let Some(comp_op) = &self.comp {
            let left = self.sum.to_tokens(state, label.clone(), attr_fun);
            let op  = &comp_op.op;
            let right = comp_op.right.to_tokens(state, label, attr_fun);
            if matches!(op, CompOp::In) {
                quote!(
                    (#left.contains(#right)).into()
                )
            } else {
                quote!(
                    (#left #op #right).into()
                )
            }
        } else {
            self.sum.to_tokens(state, label, attr_fun)
        }
    }
}