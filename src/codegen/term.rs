use super::{CodeGenState, Rc};
use proc_macro2::{TokenStream, Ident};
use quote::{ToTokens, quote};

use crate::parser::term::{Term, TermP, Op2};

impl ToTokens for Op2 {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            Op2::Mul => tokens.extend(quote!(*)),
            Op2::Div => tokens.extend(quote!(/)),
            Op2::Floor => unimplemented!("Not sure how to do this yet"),
            Op2::Mod => tokens.extend(quote!(%)),
        }
    }
}

impl Term {
    pub(crate) fn to_tokens(&self, state: &mut CodeGenState, label: Rc<Ident>, is_weight_expr: bool) -> TokenStream {
        let left = self.left.to_tokens(state, label.clone(), is_weight_expr);
        if let Some(sump) = &self.cont {
            let cont = sump.to_tokens(state, label, is_weight_expr);
            quote!(#left #cont)
        } else {
            left
        }
    }
}

impl TermP {
    pub(crate) fn to_tokens(&self, state: &mut CodeGenState, label: Rc<Ident>, is_weight_expr: bool) -> TokenStream {
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
