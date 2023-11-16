use quote::ToTokens;
use quote::quote;
use std::panic;

use super::{CodeGenState, Rc};
use proc_macro2::{TokenStream, Ident};

use crate::parser::comp::{Comparison, CompOp};

impl ToTokens for CompOp {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            CompOp::Eq => tokens.extend(quote!(==)),
            CompOp::Neq => tokens.extend(quote!(!=)),
            CompOp::Lte => tokens.extend(quote!(<=)),
            CompOp::Lt => tokens.extend(quote!(<)),
            CompOp::Gte => tokens.extend(quote!(>=)),
            CompOp::Gt => tokens.extend(quote!(>)),
            CompOp::In => panic!("Should be a special case!"),
        };
    }
}

impl Comparison {
    pub(crate) fn to_tokens(&self, state: &mut CodeGenState, label: Rc<Ident>, is_weight_expr: bool) -> TokenStream {
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