pub mod peekable;
pub mod binary_children;

use std::str::Chars;

pub fn rem_first_and_last_char(value: &str) -> String {
    _rem_last_char(_rem_first_char(value))
}

pub fn rem_first_char(value: &str) -> String {
    _rem_first_char(value).as_str().to_string()
}

pub fn rem_last_char(value: &str) -> String {
    _rem_last_char(value.chars())
}

pub fn rem_first_char_n(value: &str, n: u32) -> String {
    _rem_first_char_n(value, n).as_str().to_string()
}

pub fn remove_whitespace(mut s: String) -> String {
    s.retain(|c| !c.is_whitespace());
    s
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

fn _rem_first_char_n(value: &str, n: u32) -> Chars {
    let mut chars = value.chars();
    let mut i = 0;
    while i <= n {
        i += 1;
        chars.next();
    }
    chars
}

pub fn comma_separated_with_or(strings: &Vec<String>) -> String {
    match strings.len() {
        0 => String::new(),
        1 => strings[0].clone(),
        len => {
            let last = strings.last().unwrap();
            let joined = strings.iter().take(len - 1).map(String::as_str).collect::<Vec<&str>>().join(", ");
            format!("{} or {}", joined, last)
        }
    }
}

#[macro_export]
macro_rules! string_vec {
    ( $( $x:expr ),* ) => {
        vec![$($x.to_string(),)*]
    };
}