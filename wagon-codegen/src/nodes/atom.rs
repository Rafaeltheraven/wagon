use quote::quote;

use wagon_parser::parser::atom::Atom;
use crate::SpannableIdent;

use super::{Rc, ToTokensState};
use proc_macro2::{TokenStream, Ident};

impl<U> ToTokensState<U> for Atom {
    fn to_tokens(&self, state: &mut U, label: Rc<Ident>, attr_fun: fn(&mut U, Rc<Ident>, SpannableIdent)) -> TokenStream {
        match self {
            Self::Ident(i) => {
                let proc_ident = i.to_ident();
                attr_fun(state, label, i.clone().into());
                quote!(#proc_ident.clone())
            },
            Self::LitBool(b) => quote!(wagon_value::Value::from(#b)),
            Self::LitNum(n) => quote!(wagon_value::Value::from(#n)),
            Self::LitFloat(f) => {
                let real_float: f32 = **f;
                quote!(wagon_value::Value::try_from(#real_float?))
            },
            Self::LitString(s) => quote!(wagon_value::Value::from(#s)),
            Self::Group(g) => {
                let g_stream = g.to_tokens(state, label, attr_fun);
                quote!((#g_stream))
            },
            Self::Dict(d) => {
                let (i, e) = d.deconstruct();
                let e_stream = e.to_tokens(state, label.clone(), attr_fun);
                let ident = i.to_ident();
                attr_fun(state, label, i.clone().into());
                quote!(#ident[#e_stream])
            },
        }
    }
}