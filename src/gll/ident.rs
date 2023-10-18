use quote::{quote, ToTokens};

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Ident {
    Inherit(String),
    Synth(String),
    Unknown(String)
}

impl ToTokens for Ident {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let new_tokens = match self {
            Ident::Inherit(s) => quote!(wagon::lexer::ident::Ident::Inherit(#s)),
            Ident::Synth(s) => quote!(wagon::lexer::ident::Ident::Synth(#s)),
            Ident::Unknown(s) => quote!(wagon::lexer::ident::Ident::Unknown(#s)),
        };
        tokens.extend(new_tokens);
    }
}

impl Default for Ident {
    fn default() -> Self {
        Self::Unknown(String::new())
    }
}

impl Ident {
    pub(crate) fn extract_string(&self) -> &str {
        match self {
            Ident::Inherit(s) | Ident::Synth(s) | Ident::Unknown(s) => s,
        }
    }

    pub(crate) fn to_ident(self) -> proc_macro2::Ident {
        let s = self.extract_string();
        proc_macro2::Ident::new(&s, proc_macro2::Span::call_site())
    }
}