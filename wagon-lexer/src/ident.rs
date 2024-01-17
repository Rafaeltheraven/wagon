
use crate::LexingError;

use super::{TypeDetect, Ident};

impl TypeDetect for Ident {
    fn detect(inp: &str, span: logos::Span) -> Result<Self, LexingError> {
        let mut chars = inp.chars();
        match chars.next() {
            Some('&') => Ok(Ident::Synth(chars.as_str().to_string())),
            Some('*') => Ok(Ident::Inherit(chars.as_str().to_string())),
            Some('$') => Ok(Ident::Local(chars.as_str().to_string())),
            Some(_)   => Ok(Ident::Unknown(inp.to_string())),
            None      => Err(LexingError::UnexpectedEOF(span))
        }
    }
}
