use quote::quote;

use wagon_parser::parser::atom::Atom;
use crate::SpannableIdent;

use super::{Rc, ToTokensState};
use proc_macro2::{TokenStream, Ident};

impl<U> ToTokensState<U> for Atom {
    fn to_tokens(&self, state: &mut U, label: Rc<Ident>, attr_fun: fn(&mut U, Rc<Ident>, SpannableIdent)) -> TokenStream {
        match self {
            Atom::Ident(i) => {
                let proc_ident = i.to_ident();
                attr_fun(state, label, i.clone().into());
                quote!(#proc_ident)
            },
            Atom::LitBool(b) => quote!(#b.into()),
            Atom::LitNum(n) => quote!(#n.into()),
            Atom::LitFloat(f) => {
                let real_float: f32 = **f;
                quote!(#real_float.into())
            },
            Atom::LitString(s) => quote!(#s.into()),
            Atom::Group(g) => {
                let g_stream = g.to_tokens(state, label, attr_fun);
                quote!((#g_stream))
            },
            Atom::Dict(d) => {
                let (i, e) = d.deconstruct();
                let e_stream = e.to_tokens(state, label.clone(), attr_fun);
                let ident = i.to_ident();
                attr_fun(state, label, i.clone().into());
                quote!(#ident[#e_stream])
            },
        }
    }
}