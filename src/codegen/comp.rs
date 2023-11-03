use std::panic;

use quote::{ToTokens, quote};

use crate::parser::comp::{Comparison, CompOp};


impl ToTokens for Comparison {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        if let Some(comp_op) = &self.comp {
        	let left = &self.sum;
        	let op  = &comp_op.op;
        	let right = &comp_op.right;
        	if let CompOp::In = op {
        		tokens.extend(quote!(
        			#left.contains(#right)
        		));
        	} else {
        		tokens.extend(quote!(
	        		#left #op #right
	        	));
        	}
        } else {
        	self.sum.to_tokens(tokens);
        }
    }
}

impl ToTokens for CompOp {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            CompOp::Eq => tokens.extend(quote!(==)),
            CompOp::Neq => tokens.extend(quote!(!=)),
            CompOp::Lte => tokens.extend(quote!(<=)),
            CompOp::Lt => tokens.extend(quote!(<)),
            CompOp::Gte => tokens.extend(quote!(>=)),
            CompOp::Gt => tokens.extend(quote!(>)),
            CompOp::In => panic!("Should be a special case!"),
        };
    }
}