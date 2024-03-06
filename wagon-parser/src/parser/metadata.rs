use crate::parser::helpers::{check_semi, check_colon};
use wagon_ident::Ident;
use wagon_macros::match_error;
use std::{collections::BTreeMap, fmt::Display};

use super::{Parse, LexerBridge, ParseResult, Tokens, WagParseError, atom::Atom, Peek, Spannable, ResultNext, SpannableNode};

use wagon_macros::new_unspanned;

#[derive(PartialEq, Debug, Eq, Hash)]
#[new_unspanned]
/// The metadata of the WAG.
///
/// # Grammar
/// ```ignore
/// Metadata  -> Meta* MetaDelim;
/// MetaDelim -> "==" "="+;
/// Meta      -> Include | Config;
/// Include   -> "include" Path;
/// Config    -> Identifier ":" [Expression] ";";
/// ```
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
                    Tokens::MetadataToken(wagon_lexer::metadata::Metadata::Identifier(Ident::Unknown(s))) => {
                        check_colon(lexer)?;
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
                })?;
        }
        Ok(Self { includes, mappings })
    }
}

impl Display for Metadata {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for include in &self.includes {
            writeln!(f, "include {include};")?;
        }
        for (key, value) in &self.mappings {
            writeln!(f, "{key}: {value};")?;
        }
        writeln!(f, "================")
    }
}