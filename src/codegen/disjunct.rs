use crate::parser::disjunct::Disjunct;
use proc_macro2::TokenStream;
use quote::{ToTokens, quote};

impl ToTokens for Disjunct {
    fn to_tokens(&self, tokens: &mut TokenStream) {
    	let c = &self.0;
        tokens.extend(quote!(
        	#(#c &&)*
        ))
    }
}