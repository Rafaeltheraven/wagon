use quote::{ToTokens, quote};

use crate::parser::sum::{Sum, SumP, Op1};


impl ToTokens for Sum {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
    	self.left.to_tokens(tokens);
        if let Some(sump) = &self.cont {
        	sump.to_tokens(tokens);
        }
    }
}

impl ToTokens for SumP {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        self.op.to_tokens(tokens);
        self.right.to_tokens(tokens);
        if let Some(cont) = &self.cont {
        	cont.to_tokens(tokens);
        }
    }
}

impl ToTokens for Op1 {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            Op1::Add => tokens.extend(quote!(+)),
            Op1::Sub => tokens.extend(quote!(-)),
        }
    }
}