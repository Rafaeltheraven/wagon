#![warn(missing_docs)]
//! Utility methods for the WAGon suite of libraries.
//!
//! Provides a number of simple functions, as well as a modified version of [`std::iter::Peekable`].

/// A modified version of [`std::iter::Peekable`] that allows access to the inner iterator.
pub mod peekable;

use std::str::Chars;

/// Removes the first and last character of a string.
/// # Example
/// ```
/// let s = "123";
/// assert_eq!("2", rem_first_and_last_char(s));
/// ```
pub fn rem_first_and_last_char(value: &str) -> String {
    _rem_last_char(_rem_first_char(value))
}

/// Removes the first character of a string.
/// # Example
/// ```
/// let s = "123";
/// assert_eq!("23", rem_first_char(s));
/// ```
pub fn rem_first_char(value: &str) -> String {
    _rem_first_char(value).as_str().to_string()
}

/// Removes last character of a string.
/// # Example
/// ```
/// let s = "123";
/// assert_eq!("12", rem_last_char(s));
/// ```
pub fn rem_last_char(value: &str) -> String {
    _rem_last_char(value.chars())
}

/// Removes the first n characters of a string.
/// # Example
/// ```
/// let s = "123";
/// assert_eq!("3", rem_first_char_n(s, 2));
/// ```
pub fn rem_first_char_n(value: &str, n: usize) -> String {
    _rem_first_char_n(value, n).as_str().to_string()
}

/// Removes all whitespace from a string.
/// # Example
/// ```
/// let s = "   1  2 3      ";
/// assert_eq!("123", remove_whitespace(s));
/// ```
pub fn remove_whitespace(mut s: String) -> String {
    s.retain(|c| !c.is_whitespace());
    s
}

/// Given string and a character. Attempt to split the string at that character into 2 distinct parts.
/// # Example
/// ```
/// let s = "1:2";
/// assert_eq!(Ok(("1", "2")), split_to_twople(s, ':')).
pub fn split_to_twople(s: &str, split: char) -> Result<(String, String), SplitError> {
    let mut splitted = s.split(split);
    if let Some(left) = splitted.next() {
        if let Some(right) = splitted.next() {
            return Ok((left.trim().to_string(), right.trim().to_string()))
        }
    }
    Err(SplitError(s.to_owned(), split))
}

/// An struct for when [split_to_twople] fails.
pub struct SplitError(String, char);

impl SplitError {
    /// Get the input string that caused the error.
    pub fn get_input(self) -> String {
        self.0
    }
    /// Get the character that we weren't able to split on.
    pub fn get_split(self) -> char {
        self.1
    }
    /// Get both values in a tuple.
    pub fn decompose(self) -> (String, char) {
        (self.0, self.1)
    }
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

fn _rem_first_char_n(value: &str, n: usize) -> Chars {
    let mut chars = value.chars();
    let mut i = 0;
    while i <= n {
        i += 1;
        chars.next();
    }
    chars
}

/// Given a vector of strings, return a string that has all the values joined by a `,`. Except for the last which is joined by ` or `.
/// # Example
/// ```
/// let v = vec!["1", "2", "3"];
/// assert_eq!("1, 2 or 3", comma_separated_with_or(v));
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

/// Given a list of values, attempt to normalize them based on their sum.
/// # Example
/// ```
/// let v = vec![1, 1, 2];
/// assert_eq([0.25, 0.25, 0.5], normalize_to_probabilities(v))
pub fn normalize_to_probabilities<T: std::ops::Div<Output = T> + for<'a> std::iter::Sum<&'a T>>(input: &Vec<T>) -> Vec<T> where for<'a> &'a T: std::ops::Div<Output = T> {
    let mut output = Vec::with_capacity(input.len());
    let sum: T = input.iter().sum();

    for val in input {
        output.push(val / &sum);
    }
    output
}

/// Same as [`vec!`] but calls `to_string()` on all the elements.
#[macro_export]
macro_rules! string_vec {
    ( $( $x:expr ),* ) => {
        vec![$($x.to_string(),)*]
    };
}