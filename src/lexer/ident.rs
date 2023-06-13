pub use crate::helpers::rem_first_char;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Ident {
    Inherit(String),
    Synth(String),
    Unknown(String)
}

pub fn detect_ident_type(ident: &str) -> Ident {
    let mut chars = ident.chars();
    let fst = chars.next().unwrap();
    match fst {
        '$' => Ident::Synth(chars.as_str().to_string()),
        '!' => Ident::Inherit(chars.as_str().to_string()),
        _ => Ident::Unknown(ident.to_string()),
    }
}