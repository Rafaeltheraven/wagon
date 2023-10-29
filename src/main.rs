mod lexer;
mod parser;
mod codegen;

pub mod helpers;

use std::env;
use std::format;
use std::fs;
use std::fs::File;
use std::io::Write;
use std::process::Command;

use parser::WagParseError;

fn main() {
    let args: Vec<String> = env::args().collect();
    let input_file: &String = &args[1];
    let proj_name: &String = &args[2];
    let overwrite = if args.len() > 3 {
        args[3] == "--overwrite"
    } else {
        false
    };
    let contents = fs::read_to_string(input_file).expect("Couldn't read file");
    match codegen::gen_parser(&contents) {
        Ok(data) => write_parser(data, proj_name, overwrite),
        Err(e) => handle_error(e),
    }
}

fn handle_error(err: WagParseError) {
    eprintln!("{}", err);
}

fn write_parser(data: String, proj_name: &str, overwrite: bool) {
    let path = std::path::Path::new(proj_name);
    let mut exists = path.exists();
    if exists && overwrite {
        println!("{:?} already exists. Overwriting!", proj_name);
        std::fs::remove_dir_all(path).unwrap();
        exists = false;
    }
    if !exists {
        Command::new("cargo").args(["new", proj_name]).output().unwrap();
        Command::new("cargo") 
            .current_dir(proj_name)
            .args(["add", "wagon-gll", "--path", "../../wagon_gll"])
            .output()
            .unwrap();
        Command::new("cargo").current_dir(proj_name).args(["add", "petgraph"]).output().unwrap();
    }
    let mut file = File::create(format!("./{}/src/main.rs", proj_name)).unwrap();
    file.write_all(&data.into_bytes()).unwrap();
}
