
use std::str::Chars;
use logos::Logos;

use std::fmt;


pub fn rem_first_and_last_char(value: &str) -> String {
    _rem_last_char(_rem_first_char(value))
}

pub fn rem_first_char(value: &str) -> String {
    _rem_first_char(value).as_str().to_string()
}

pub fn rem_last_char(value: &str) -> String {
    _rem_last_char(value.chars())
}

fn _rem_first_char(value: &str) -> Chars {
    let mut chars = value.chars();
    chars.next();
    chars
}

fn _rem_last_char(mut chars: Chars) -> String {
    chars.next_back();
    return chars.as_str().to_string();
}

pub fn assert_lex<'a, Token>(
    source: &'a Token::Source,
    tokens: &[Result<Token, Token::Error>],
) where
    Token: Logos<'a> + fmt::Debug + PartialEq,
    Token::Extras: Default,
{
    let mut lex = Token::lexer(source);

    for token in tokens {
        assert_eq!(
            &lex.next().expect("Unexpected end"),
            token
        );
    }

    assert_eq!(lex.next(), None);
}
