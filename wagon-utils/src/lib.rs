#![warn(missing_docs)]
//! Utility methods for the WAGon suite of libraries.
//!
//! Provides a number of simple functions, as well as a trait version of [`std::iter::Peekable`].

/// A trait version of [`std::iter::Peekable`].
mod peek;

use std::{str::Chars, marker::PhantomData, fmt::Display, error::Error};

pub use peek::Peek;

/// Removes the first and last character of a string.
/// # Example
/// ```
/// use wagon_utils::rem_first_and_last_char;
///
/// let s = "123";
/// assert_eq!("2", rem_first_and_last_char(s));
/// ```
pub fn rem_first_and_last_char(value: &str) -> String {
    _rem_last_char(_rem_first_char(value))
}

/// Removes the first character of a string.
/// # Example
/// ```
/// use wagon_utils::rem_first_char;
///
/// let s = "123";
/// assert_eq!("23", rem_first_char(s));
/// ```
pub fn rem_first_char(value: &str) -> String {
    _rem_first_char(value).as_str().to_string()
}

/// Removes last character of a string.
/// # Example
/// ```
/// use wagon_utils::rem_last_char;
///
/// let s = "123";
/// assert_eq!("12", rem_last_char(s));
/// ```
pub fn rem_last_char(value: &str) -> String {
    _rem_last_char(value.chars())
}

/// Removes the first n characters of a string.
/// # Example
/// ```
/// use wagon_utils::rem_first_char_n;
///
/// let s = "123";
/// assert_eq!("3", rem_first_char_n(s, 2));
/// ```
pub fn rem_first_char_n(value: &str, n: usize) -> String {
    _rem_first_char_n(value, n).as_str().to_string()
}

/// Removes all whitespace from a string.
/// # Example
/// ```
/// use wagon_utils::remove_whitespace;
///
/// let s = "   1  2 3      ".to_string();
/// assert_eq!("123", remove_whitespace(s));
/// ```
pub fn remove_whitespace(mut s: String) -> String {
    s.retain(|c| !c.is_whitespace());
    s
}

/// Given string and a character. Attempt to split the string at that character into 2 distinct parts.
/// # Example
/// ```
/// use wagon_utils::split_to_twople;
///
/// let s = "1:2";
/// assert_eq!(Ok(("1".to_string(), "2".to_string())), split_to_twople(s, ':'));
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
#[derive(Eq, PartialEq, Debug)]
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
    while i < n {
        i += 1;
        chars.next();
    }
    chars
}

/// Given a vector of strings, return a string that has all the values joined by a `,`. Except for the last which is joined by ` or `.
/// # Example
/// ```
/// use wagon_utils::comma_separated_with_or;
///
/// let v = vec!["1".to_string(), "2".to_string(), "3".to_string()];
/// assert_eq!("1, 2 or 3".to_string(), comma_separated_with_or(&v));
/// let v = vec!["1".to_string()];
/// assert_eq!("1".to_string(), comma_separated_with_or(&v));
/// let v = vec![];
/// assert_eq!("".to_string(), comma_separated_with_or(&v));
pub fn comma_separated_with_or(strings: &Vec<String>) -> String {
    match strings.last() {
        Some(last) => {
            let len = strings.len();
            if len == 1 {
                last.clone()
            } else {
                let joined = strings.iter().take(len - 1).map(String::as_str).collect::<Vec<&str>>().join(", ");
                format!("{} or {}", joined, last)
            }
        },
        None => String::new(),
    }
}

/// Given a list of values, attempt to normalize them based on their sum.
///
/// This method works as long as the type inside the vec supports the [`std::ops::Div`] and [`std::iter::Sum`] traits. 
///
/// # Example
/// ```
/// use wagon_utils::normalize_to_probabilities;
///
/// let v = vec![1.0, 1.0, 2.0];
/// assert_eq!(vec![0.25, 0.25, 0.5], normalize_to_probabilities(&v))
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

/// Quickly get a result from an iterator of [`Result`]s.
///
/// This trait is automatically implemented for any iterator of `Result`s that has an error type
/// that implements [`From<UnexpectedEnd>`].
pub trait ResultNext<T, E>: Iterator<Item = Result<T,E>> {
    /// If you have an iterator that holds `Result` items, you start having to deal with nested `Some(Ok(...))` patterns,
    /// which gets annoying quickly. This trait is intended so that the iterator always returns some sort of `Result`, which can then be unwrapped as needed (probably using `?`).
    ///
    /// # Example
    /// ```
    /// # use wagon_utils::ResultNext;
    /// # use wagon_utils::UnexpectedEnd;
    /// struct IterVec<T, E>(Vec<Result<T, E>>);
    /// #[derive(Debug, Eq, PartialEq)]
    /// enum IterErr {
    ///      SomeErr,
    ///      EndErr
    /// }
    /// impl From<UnexpectedEnd> for IterErr {
    /// #    fn from(value: UnexpectedEnd) -> Self {
    /// #        Self::EndErr
    /// #    }
    /// }
    /// impl<T, E> Iterator for IterVec<T, E> {
    ///     # type Item = Result<T, E>;
    ///     # fn next(&mut self) -> Option<Self::Item> {
    ///     #     self.0.pop()
    ///     # }
    /// }
    /// 
    /// let mut iter: IterVec<i32, IterErr> = IterVec(vec![Ok(1)]);
    /// assert_eq!(Ok(1), iter.next_result());
    /// assert_eq!(Err(IterErr::EndErr), iter.next_result());
    /// ```
    fn next_result(&mut self) -> Result<T, E>;
}

/// Same as [`ResultNext`] but for things that implement [`Peek`].
///
/// This trait is automatically implemented for any iterator of `Result`s that has an error type `E`
/// such that `&E: From<UnexpectedEnd>`.
pub trait ResultPeek<T, E>: Peek + Iterator<Item = Result<T, E>> {
    /// See [`next_result`](`ResultNext::next_result`).
    fn peek_result(&mut self) -> Result<&T, E>;
}

/// Error struct to represent we've reached the end of an iterator. Used for [`ResultNext`].
pub struct UnexpectedEnd;

impl<T, E: From<UnexpectedEnd>, U: Iterator<Item = Result<T, E>>> ResultNext<T, E> for U {
    fn next_result(&mut self) -> Result<T, E> {
        match self.next() {
            Some(Ok(x)) => Ok(x),
            Some(Err(e)) => Err(e),
            None => Err(UnexpectedEnd.into())
        }
    }
}

impl<T, E, U: Peek + Iterator<Item = Result<T, E>>> ResultPeek<T, E> for U 
    where for<'a> E: From<UnexpectedEnd> + Clone + 'a
    {
        fn peek_result(&mut self) -> Result<&T, E> {
            match self.peek() {
                None => Err(UnexpectedEnd.into()),
                Some(Ok(ref x)) => Ok(x),
                Some(Err(e)) => Err(e.clone())
            }
        }
    }

/// Forcibly extract an item out of an iterator of [`Result`]s.
///
/// If you have an iterator that holds `Result` items, it quickly becomes annoying to constantly unwrap.
/// This trait provides the method `next_unwrap` to quickly extract the inner item.
pub trait UnsafeNext<T, E: std::fmt::Debug>: Iterator<Item = Result<T, E>> {
    /// # Example
    /// ```should_panic
    /// # use wagon_utils::UnsafeNext;
    /// struct IterVec<T, E>(Vec<Result<T, E>>);
    /// impl<T, E: std::fmt::Debug> Iterator for IterVec<T, E> {
    ///     # type Item = Result<T, E>;
    ///     # fn next(&mut self) -> Option<Self::Item> {
    ///     #     self.0.pop()
    ///     # }
    /// }
    /// impl<T, E: std::fmt::Debug> UnsafeNext<T, E> for IterVec<T, E> {}
    /// 
    /// let mut iter: IterVec<i32, ()> = IterVec(vec![Ok(1)]);
    /// assert_eq!(1, iter.next_unwrap());
    /// iter.next_unwrap(); // panic!
    /// ```
    ///
    /// # Panics
    /// Panics if the next element is either `None` or an `Err`.
    fn next_unwrap(&mut self) -> T {
        match self.next() {
            Some(Ok(x)) => x,
            Some(Err(e)) => panic!("Got error: {:?}", e),
            None => panic!("Expected a value, but failed")
        }
    }
}

/// Same as [`UnsafeNext`] but intended for iterators that allow peeking (such as [`Peekable`](`std::iter::Peekable`)).
pub trait UnsafePeek<'a, T, E: std::fmt::Debug + 'a>: Peek + Iterator<Item = Result<T, E>> {
    /// See [`next_unwrap`](`UnsafeNext::next_unwrap`).
    fn peek_unwrap(&'a mut self) -> &T {
        match self.peek() {
            Some(Ok(ref x)) => x,
            Some(Err(ref e)) => panic!("Got error: {:?}", e),
            None => panic!("Expected a value, but failed")
        }
    }
}

#[derive(Debug)]
/// We failed to convert from some thing to another thing.
///
/// A generic error for when doing any [`TryFrom`] type implementations.
///
/// # Example
/// ```
/// use wagon_utils::ConversionError;
///
/// struct A;
/// impl TryFrom<i32> for A {
///     type Error = ConversionError<i32, Self>;
///
///     fn try_from(value: i32) -> Result<Self, Self::Error> {
///         Err(ConversionError::new(value))
///     }
/// }
/// ```
/// 
pub struct ConversionError<T, U> {
    /// The thing we want to convert.
    subject: T,
    /// Used to hold the type info for the type of [`Valueable`] we are trying to convert to.
    to: PhantomData<U>
}

impl<T: Display, U> Display for ConversionError<T, U> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Failed to convert {} from type {} to value variant {}", self.subject, std::any::type_name::<T>(), std::any::type_name::<U>())
    }
}

impl<T: Display + std::fmt::Debug, U: std::fmt::Debug> Error for ConversionError<T, U> {}

impl<T, U> ConversionError<T, U> {
    /// Create a new `ConversionError`.
    pub fn new(subject: T) -> Self {
        Self {subject, to: PhantomData}
    }
}