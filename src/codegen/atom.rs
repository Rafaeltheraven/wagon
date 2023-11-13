use quote::{ToTokens, quote};

use crate::parser::atom::Atom;


impl ToTokens for Atom {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            Atom::Ident(i) => {
                let proc_ident = i.to_ident();
                proc_ident.to_tokens(tokens);
            },
            Atom::LitBool(b) => tokens.extend(quote!(#b.into())),
            Atom::LitNum(n) => tokens.extend(quote!(#n.into())),
            Atom::LitFloat(f) => {
                let real_float: f32 = **f;
                tokens.extend(quote!(ordered_float::NotNan::new(#real_float).into()))
            },
            Atom::LitString(s) => tokens.extend(quote!(#s.into())),
            Atom::Group(g) => tokens.extend(quote!((#g))),
            Atom::Dict(d) => {
            	let (i, e) = d.deconstruct();
            	tokens.extend(quote!(state.get_label(#i)[#e]))
            },
        }
    }
}