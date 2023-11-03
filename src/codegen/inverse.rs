use quote::{ToTokens, quote};

use crate::parser::inverse::Inverse;


impl ToTokens for Inverse {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            Inverse::Not(i) => {
            	tokens.extend(quote!(
            		!#i
            	));
            },
            Inverse::Comparison(c) => {
            	c.to_tokens(tokens);
            },
        }
    }
}