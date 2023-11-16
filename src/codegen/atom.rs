use quote::quote;

use crate::parser::atom::Atom;
use super::{CodeGenState, Rc};
use proc_macro2::{TokenStream, Ident};

impl Atom {
    pub(crate) fn to_tokens(&self, state: &mut CodeGenState, label: Rc<Ident>, is_weight_expr: bool) -> TokenStream {
        match self {
            Atom::Ident(i) => {
                let proc_ident = i.to_ident();
                if is_weight_expr {
                    state.add_req_weight_attr(label, i.clone());
                } else {
                    state.add_req_code_attr(label, i.clone());
                };
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
                let g_stream = g.to_tokens(state, label, is_weight_expr);
                quote!((#g_stream))
            },
            Atom::Dict(d) => {
                let (i, e) = d.deconstruct();
                let e_stream = e.to_tokens(state, label.clone(), is_weight_expr);
                let ident = i.to_ident();
                if is_weight_expr {
                    state.add_req_weight_attr(label, i.clone());
                } else {
                    state.add_req_code_attr(label, i.clone());
                };
                quote!(#ident[#e_stream])
            },
        }
    }
}