use wagon_parser::parser::expression::Expression;
use proc_macro2::{TokenStream, Ident};
use quote::quote;
use crate::SpannableIdent;

use super::{Rc, ToTokensState};

impl<U> ToTokensState<U> for Expression {
    fn to_tokens(&self, state: &mut U, label: Rc<Ident>, attr_fun: fn(&mut U, Rc<Ident>, SpannableIdent)) -> TokenStream {
        match self {
            Expression::Subproc(s) => {
                quote!(
                    {
                        let out = subprocess::Exec::shell(#s).stdout(subprocess::Redirection::Pipe).capture().expect("Was unable to capture shell output").stdout_str();
                        let value: Value = serde_json::from_str(&out).expect("Was unable to parse json");
                        value
                    }
                )
            },
            Expression::If { this, then, r#else } => {
                let this_stream = this.to_tokens(state, label.clone(), attr_fun);
                let then_stream = then.to_tokens(state, label.clone(), attr_fun);
                let if_stream = quote!(
                    if #this_stream {
                        #then_stream
                    }
                );
                if let Some(e) = r#else {
                    let e_stream = e.to_tokens(state, label, attr_fun);
                    quote!(
                        #if_stream else {
                            #e_stream
                        }
                    )
                } else {
                    if_stream
                }
            },
            Expression::Disjunct(d) => {
                d.to_tokens(state, label, attr_fun)
            },
        }
    }
}