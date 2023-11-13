use crate::parser::disjunct::Disjunct;
use proc_macro2::TokenStream;
use quote::{ToTokens, quote};

impl ToTokens for Disjunct {
    fn to_tokens(&self, tokens: &mut TokenStream) {
    	let c = &self.0;
        if c.len() > 1 {
            tokens.extend(quote!(
            	#(#c &&)*
            ))
        } else {
            c[0].to_tokens(tokens)
        };
    }
}