use crate::parser::conjunct::Conjunct;
use super::{CodeGenState, Rc, ToTokensState};
use proc_macro2::{TokenStream, Ident};
use quote::quote;

impl ToTokensState for Conjunct {
    fn to_tokens(&self, state: &mut CodeGenState, label: Rc<Ident>, is_weight_expr: bool) -> TokenStream {
        let i = &self.0;
        if i.len() > 1 {
            let stream: Vec<TokenStream> = i.iter().map(|x| x.to_tokens(state, label.clone(), is_weight_expr)).collect();
            quote!(
                #(#stream ||)*
            )
        } else {
            i[0].to_tokens(state, label, is_weight_expr)
        }
    }
}