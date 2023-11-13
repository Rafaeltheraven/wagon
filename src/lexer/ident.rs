
use super::{TypeDetect, Ident};

impl TypeDetect for Ident {
    fn detect(inp: &str) -> Self {
        let mut chars = inp.chars();
        let fst = chars.next().unwrap();
        match fst {
            '&' => Ident::Synth(chars.as_str().to_string()),
            '*' => Ident::Inherit(chars.as_str().to_string()),
            '$' => Ident::Local(chars.as_str().to_string()),
            _ => Ident::Unknown(inp.to_string()),
        }
    }
}
