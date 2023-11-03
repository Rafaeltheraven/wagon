use crate::parser::conjunct::Conjunct;
use proc_macro2::TokenStream;
use quote::{ToTokens, quote};

impl ToTokens for Conjunct {
    fn to_tokens(&self, tokens: &mut TokenStream) {
    	let i = &self.0;
        tokens.extend(quote!(
        	#(#i ||)*
        ))
    }
}