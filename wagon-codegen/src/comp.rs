use quote::quote;


use super::ToTokensState;
use super::{CodeGenState, Rc};
use proc_macro2::{TokenStream, Ident};

use wagon_parser::parser::comp::{Comparison, CompOp};

impl ToTokensState for Comparison {
    fn to_tokens(&self, state: &mut CodeGenState, label: Rc<Ident>, is_weight_expr: bool) -> TokenStream {
        if let Some(comp_op) = &self.comp {
            let left = self.sum.to_tokens(state, label.clone(), is_weight_expr);
            let op  = &comp_op.op;
            let right = comp_op.right.to_tokens(state, label.clone(), is_weight_expr);
            if let CompOp::In = op {
                quote!(
                    #left.contains(#right)
                )
            } else {
                quote!(
                    #left #op #right
                )
            }
        } else {
            self.sum.to_tokens(state, label, is_weight_expr)
        }
    }
}