use wagon_parser::parser::conjunct::Conjunct;
use crate::SpannableIdent;

use super::{Rc, ToTokensState};
use proc_macro2::{TokenStream, Ident};
use quote::quote;

impl<U> ToTokensState<U> for Conjunct {
    fn to_tokens(&self, state: &mut U, label: Rc<Ident>, attr_fun: fn(&mut U, Rc<Ident>, SpannableIdent)) -> TokenStream {
        let i = &self.0;
        if i.len() > 1 {
            let stream: Vec<TokenStream> = i.iter().map(|x| x.to_tokens(state, label.clone(), attr_fun)).collect();
            quote!(
                #(#stream ||)*
            )
        } else {
            i[0].to_tokens(state, label, attr_fun)
        }
    }
}