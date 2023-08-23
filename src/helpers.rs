
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

fn _rem_first_char(value: &str) -> Chars {
    let mut chars = value.chars();
    chars.next();
    chars
}

fn _rem_last_char(mut chars: Chars) -> String {
    chars.next_back();
    return chars.as_str().to_string();
}

struct IterVec<T> {
    data: Vec<T>,
    index: usize
}

impl<T> IterVec<T> {
    fn next(&mut self) -> Option<&T> {
        self.index += 1;
        if self.index <= self.data.len() {
            Some(&self.data[self.index])
        } else {
            None
        }
    }

    fn back(&mut self) {
        self.index -= 1;
    }

    fn set_index(&mut self, i: usize) {
        self.index = i;
    }

    fn peek(&mut self) -> &T {
        &self.data[self.index]
    }
}