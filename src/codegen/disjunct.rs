use crate::parser::disjunct::Disjunct;
use super::{CodeGenState, Rc, ToTokensState};
use proc_macro2::{TokenStream, Ident};
use quote::quote;

impl ToTokensState for Disjunct {
    fn to_tokens(&self, state: &mut CodeGenState, label: Rc<Ident>, is_weight_expr: bool) -> TokenStream {
        let c = &self.0;
        if c.len() > 1 {
            let stream: Vec<TokenStream> = c.iter().map(|x| x.to_tokens(state, label.clone(), is_weight_expr)).collect();
            quote!(
                #(#stream &&)*
            )
        } else {
            c[0].to_tokens(state, label, is_weight_expr)
        }
    }
}