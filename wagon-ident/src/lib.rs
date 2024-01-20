#![warn(missing_docs)]
//! Crate for Identifiers used in the WAGon ecosystem.
//!
//! Identifiers are used throughout various parts of WAGon for various reasons. This crate acts as a central location to retrieve them from.
use std::{fmt::Display, write};

use quote::{quote, ToTokens};

#[derive(Debug, PartialEq, Eq, Clone, Hash, PartialOrd, Ord)]
/// A valid WAGon identifier.
pub enum Ident {
    /// An inherited attribute
    Inherit(String),
    /// A synthesized attribute
    Synth(String),
    /// A local attribute
    Local(String),
    /// Either an attribute that we do not yet know the scope of, or an identifier used for another purpose.
    Unknown(String)
}

impl ToTokens for Ident {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let new_tokens = match self {
            Self::Inherit(s) => quote!(wagon_ident::Ident::Inherit(#s.to_string())),
            Self::Synth(s) => quote!(wagon_ident::Ident::Synth(#s.to_string())),
            Self::Local(s) => quote!(wagon_ident::Ident::Local(#s.to_string())),
            Self::Unknown(s) => quote!(wagon_ident::Ident::Unknown(#s.to_string())),
        };
        tokens.extend(new_tokens);
    }
}

impl Default for Ident {
    fn default() -> Self {
        Self::Unknown(String::new())
    }
}

impl Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Inherit(i) => write!(f, "*{i}"),
            Self::Synth(i) => write!(f, "&{i}"),
            Self::Local(i) => write!(f, "${i}"),
            Self::Unknown(i) => write!(f, "{i}"),
        }
    }
}

impl Ident {
    /// Get the original string from the identifier.
    #[must_use]
    pub fn extract_string(&self) -> &str {
        match self {
            Self::Inherit(s) | Self::Synth(s) | Self::Local(s) | Self::Unknown(s) => s,
        }
    }

    /// Convert into a standardized [`proc_macro2::Ident`] for code generation purposes.
    ///
    /// # Conversions
    /// | `Ident` | `proc_macro2::Ident` |
    /// |:-------:|:--------------------:|
    /// | `*foo`  | `i_foo`              |
    /// | `&foo`  | `s_foo`              |
    /// | `$foo`  | `l_foo`              |
    /// | `foo`   | `u_foo`              |
    #[must_use]
    pub fn to_ident(&self) -> proc_macro2::Ident {
        let text = match self {
            Self::Inherit(s) => format!("i_{s}"),
            Self::Synth(s) => format!("s_{s}"),
            Self::Local(s) => format!("l_{s}"),
            Self::Unknown(s) => format!("u_{s}"),
        };
        proc_macro2::Ident::new(&text, proc_macro2::Span::call_site())
    }
}