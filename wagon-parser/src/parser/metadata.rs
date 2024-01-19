use crate::parser::helpers::check_semi;
use wagon_macros::match_error;
use std::collections::BTreeMap;

use super::{Parse, LexerBridge, ParseResult, Tokens, WagParseError, atom::Atom, Peek, Spannable, ResultNext, SpannableNode};

#[cfg(test)]
use wagon_macros::new_unspanned;

#[derive(PartialEq, Debug, Eq, Hash)]
#[cfg_attr(test, new_unspanned)]
/// The metadata of the WAG.
pub struct Metadata {
    /// All imports for this grammar.
	pub includes: Vec<String>,
    /// Any extra key-value mappings.
	pub mappings: BTreeMap<String, SpannableNode<Atom>>
}

impl Parse for Metadata {
    fn parse(lexer: &mut LexerBridge) -> ParseResult<Self> {
        let mut includes = Vec::new();
        let mut mappings = BTreeMap::new();
        while let Some(Ok(Tokens::MetadataToken(_))) = lexer.peek() {
                match_error!(match lexer.next_result()? {
                    Tokens::MetadataToken(wagon_lexer::metadata::Metadata::Key(s)) => {
                        let atom = SpannableNode::parse(lexer)?;
                        check_semi(lexer)?;
                        mappings.insert(s, atom);
                        Ok(())
                    },
                    Tokens::MetadataToken(wagon_lexer::metadata::Metadata::Delim) => break,
                    Tokens::MetadataToken(wagon_lexer::metadata::Metadata::Include) => {
                        match_error!(match lexer.next_result()? {
                            Tokens::MetadataToken(wagon_lexer::metadata::Metadata::Path(p)) => {
                                includes.push(p); 
                                check_semi(lexer)?;
                                Ok(())
                            }
                        })
                    }
                })?
        }
        Ok(Metadata { includes, mappings })
    }
}