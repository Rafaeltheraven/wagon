use super::{CodeGenState, Rc, ToTokensState};
use proc_macro2::{TokenStream, Ident};
use quote::quote;

use wagon_parser::parser::sum::{Sum, SumP};

impl ToTokensState for Sum {
    fn to_tokens(&self, state: &mut CodeGenState, label: Rc<Ident>, is_weight_expr: bool) -> TokenStream {
        let left = self.left.to_tokens(state, label.clone(), is_weight_expr);
        if let Some(sump) = &self.cont {
            let cont = sump.to_tokens(state, label, is_weight_expr);
            quote!(#left #cont)
        } else {
            left
        }
    }
}

impl ToTokensState for SumP {
    fn to_tokens(&self, state: &mut CodeGenState, label: Rc<Ident>, is_weight_expr: bool) -> TokenStream {
        let op = &self.op;
        let right = self.right.to_tokens(state, label.clone(), is_weight_expr);
        if let Some(cont) = &self.cont {
            let cont_stream = cont.to_tokens(state, label, is_weight_expr);
            quote!(#op #right #cont_stream)
        } else {
            quote!(#op #right)
        }
    }
}
