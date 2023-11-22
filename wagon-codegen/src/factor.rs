use super::{CodeGenState, Rc, ToTokensState};
use proc_macro2::{TokenStream, Ident};
use quote::quote;

use wagon_parser::parser::factor::Factor;

impl ToTokensState for Factor {
    fn to_tokens(&self, state: &mut CodeGenState, label: Rc<Ident>, is_weight_expr: bool) -> TokenStream {
        match self {
            Factor::Primary(p) => p.to_tokens(state, label, is_weight_expr),
            Factor::Power { left, right } => {
                let left_stream = left.to_tokens(state, label.clone(), is_weight_expr);
                let right_stream = right.to_tokens(state, label, is_weight_expr);
                quote!(#left_stream.pow(#right_stream))
            },
        }
    }
}