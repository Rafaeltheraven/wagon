use wagon_parser::parser::disjunct::Disjunct;
use crate::SpannableIdent;

use super::{Rc, ToTokensState};
use proc_macro2::{TokenStream, Ident};
use quote::quote;

impl<U> ToTokensState<U> for Disjunct {
    fn to_tokens(&self, state: &mut U, label: Rc<Ident>, attr_fun: fn(&mut U, Rc<Ident>, SpannableIdent)) -> TokenStream {
        let c = &self.0;
        if c.len() > 1 {
            let stream: Vec<TokenStream> = c.iter().map(|x| x.to_tokens(state, label.clone(), attr_fun)).collect();
            quote!(
                #(wagon_value::Valueable::is_truthy(&#stream)?) &&*
            )
        } else {
            c[0].to_tokens(state, label, attr_fun)
        }
    }
}