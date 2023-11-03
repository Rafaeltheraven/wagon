use quote::{ToTokens, quote};

use crate::parser::factor::Factor;


impl ToTokens for Factor {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            Factor::Primary(p) => p.to_tokens(tokens),
            Factor::Power { left, right } => tokens.extend(quote!(#left.pow(#right))),
        }
    }
}