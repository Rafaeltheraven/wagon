mod lexer;
mod parser;
mod codegen;

pub mod helpers;

use std::env;
use std::fs;

fn main() {
    let args: Vec<String> = env::args().collect();
    let input_file: &String = &args[1];
    let contents = fs::read_to_string(input_file).expect("Couldn't read file");
    codegen::gen_parser(&contents);
}
