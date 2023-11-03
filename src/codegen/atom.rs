use quote::{ToTokens, quote};

use crate::parser::atom::Atom;


impl ToTokens for Atom {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            Atom::Ident(i) => tokens.extend(quote!(state.get_label(#i))),
            Atom::LitBool(b) => b.to_tokens(tokens),
            Atom::LitNum(n) => n.to_tokens(tokens),
            Atom::LitFloat(f) => f.to_tokens(tokens),
            Atom::LitString(s) => s.to_tokens(tokens),
            Atom::Group(g) => tokens.extend(quote!((#g))),
            Atom::Dict(d) => {
            	let (i, e) = d.deconstruct();
            	tokens.extend(quote!(state.get_label(#i)[#e]))
            },
        }
    }
}