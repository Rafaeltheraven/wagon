use quote::{ToTokens, quote};

use crate::parser::term::{Term, TermP, Op2};


impl ToTokens for Term {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
    	self.left.to_tokens(tokens);
        if let Some(sump) = &self.cont {
        	sump.to_tokens(tokens);
        }
    }
}

impl ToTokens for TermP {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        self.op.to_tokens(tokens);
        self.right.to_tokens(tokens);
        if let Some(cont) = &self.cont {
        	cont.to_tokens(tokens);
        }
    }
}

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