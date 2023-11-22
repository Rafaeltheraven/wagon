use quote::quote;
use super::{CodeGenState, Rc, ToTokensState};
use proc_macro2::{TokenStream, Ident};
use wagon_parser::parser::inverse::Inverse;

impl ToTokensState for Inverse {
    fn to_tokens(&self, state: &mut CodeGenState, label: Rc<Ident>, is_weight_expr: bool) -> TokenStream {
        match self {
            Inverse::Not(i) => {
                let i_stream = i.to_tokens(state, label, is_weight_expr);
                quote!(
                    !#i_stream
                )
            },
            Inverse::Comparison(c) => {
                c.to_tokens(state, label, is_weight_expr)
            },
        }
    }
}