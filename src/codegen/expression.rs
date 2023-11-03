use crate::parser::expression::Expression;
use proc_macro2::TokenStream;
use quote::{ToTokens, quote};

impl ToTokens for Expression {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Expression::Subproc(s) => {
            	tokens.extend(quote!(
            		{
            			let out = subprocess::Exec::shell(#s).stdout(subprocess::Redirection::Pipe).capture().expect("Was unable to capture shell output").stdout_str();
            			let value: Value = serde_json::from_str(&out).expect("Was unable to parse json");
                        value
            		}
            	));
            },
            Expression::If { this, then, r#else } => {
            	tokens.extend(quote!(
            		if #this {
            			#then
            		}
            	));
            	if let Some(e) = r#else {
            		tokens.extend(quote!(
            			else {
            				#e
            			}
            		));
            	}
            },
            Expression::Disjunct(d) => {
            	d.to_tokens(tokens);
            },
        }
    }
}
