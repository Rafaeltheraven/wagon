use proc_macro::TokenStream;
use extendable_enums::extendable_enum;
use quote::quote;

#[extendable_enum(inherit_from_base)]
#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"[ \t\n\f]+")]
enum Base {
    #[regex("([a-zA-Z][a-zA-Z0-9]*)", |lex| lex.slice().to_owned())]
    Ident(String),

    #[regex("\"([^\"\\\\]|\\\\.)*\"", |lex| rem_first_and_last_char(lex.slice()))]
    #[regex("\'([^\'\\\\]|\\\\.)*\'", |lex| rem_first_and_last_char(lex.slice()))]
    LitString(String),
}
