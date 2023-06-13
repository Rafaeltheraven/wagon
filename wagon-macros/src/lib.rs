use extendable_enums::extendable_enum;

#[extendable_enum(inherit_from_base)]
#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"[ \t\n\f]+")]
enum Base {
    #[regex("(\\$|!)?([a-zA-Z][a-zA-Z0-9]*)", |lex| detect_ident_type(lex.slice()))]
    Identifier(Ident),

    #[regex("\"([^\"\\\\]|\\\\.)*\"", |lex| rem_first_and_last_char(lex.slice()))]
    #[regex("\'([^\'\\\\]|\\\\.)*\'", |lex| rem_first_and_last_char(lex.slice()))]
    LitString(String),

    #[token("[")]
    LBr,

    #[token("]")]
    RBr,
}
