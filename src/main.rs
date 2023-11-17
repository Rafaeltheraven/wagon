mod lexer;
mod parser;
mod codegen;
mod firstpass;

pub mod helpers;

use std::env;
use std::format;
use std::fs;
use std::fs::File;
use std::io::Write;
use std::process::Command;

use parser::WagParseError;
use proc_macro2::Ident;
use proc_macro2::Span;
use proc_macro2::TokenStream;
use quote::quote;

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
        Err(e) => handle_error(e, input_file, contents),
    }
}

fn handle_error(err: WagParseError, file_path: &str, file: String) {
    use ariadne::{ColorGenerator, Label, Report, ReportKind, Source};
    let mut colors = ColorGenerator::new();
    let a = colors.next();
    let ((head, msg), span) = err.msg_and_span();
    Report::build(ReportKind::Error, file_path, 12)
        .with_message(head)
        .with_label(
            Label::new((file_path, span))
                .with_message(msg)
                .with_color(a),
        )
        .finish()
        .eprint((file_path, Source::from(file)))
        .unwrap();
}

fn write_parser(data: codegen::CodeMap, proj_name: &str, overwrite: bool) {
    let (subcode, code) = data;
    let root_terms = subcode.keys().collect();
    create_structure(proj_name, &root_terms, overwrite);
    let path = std::path::Path::new(proj_name).join("src");
    let main_path = path.join("main.rs");
    let mut file = File::create(&main_path).unwrap();
    file.write_all(pretty_code(code, &main_path).as_bytes()).unwrap();
    let term_path = path.join("terminals");
    let term_file_path = path.join(format!("{}.rs", "terminals"));
    let mut term_file = File::create(&term_file_path).unwrap();
    let root_terms_idents = root_terms.iter().map(|x| Ident::new(x, Span::call_site()));
    term_file.write_all(pretty_code(quote!(
        #![allow(non_snake_case)]

        #(pub(crate) mod #root_terms_idents;)*
    ), &term_file_path).as_bytes()).unwrap();
    for (terminal, structs) in subcode {
        write_terminal(&term_path, terminal, structs);
    }
}

fn create_structure(proj_name: &str, terminals: &Vec<&String>, overwrite: bool) {
    let path = std::path::Path::new(proj_name);
    let mut exists = path.exists();
    if exists && overwrite {
        println!("{:?} already exists. Overwriting!", proj_name);
        std::fs::remove_dir_all(path).unwrap();
        exists = false;
    }
    let term_path = path.join("src").join("terminals");
    if !exists {
        let libs = ["subprocess", "serde_json", "rand_dist", "itertools"];
        Command::new("cargo").args(["new", proj_name]).output().unwrap();
        Command::new("cargo") 
            .current_dir(path)
            .args(["add", "wagon-gll", "--path", "../../wagon_gll"])
            .output()
            .unwrap();
        Command::new("cargo").current_dir(path).args(["add", "clap", "--features", "derive,cargo"]).output().unwrap();
        Command::new("cargo").current_dir(path).args(["add", "rand", "--features", "small_rng"]).output().unwrap();
        for lib in libs {
            Command::new("cargo").current_dir(path).args(["add", lib]).output().unwrap();
        }
        std::fs::create_dir(&term_path).unwrap();
    }
    for term in terminals {
        let curr_path = term_path.join(term);
        if !curr_path.exists() {
            std::fs::create_dir(curr_path).unwrap();
        }
    }
}

fn pretty_code(code: TokenStream, path: &std::path::Path) -> String {
    let code_string = code.to_string();
    match syn::parse_file(&code_string) {
        Ok(tree) => prettyplease::unparse(&tree),
        Err(e) => {
            eprintln!("Error parsing code for {}: {}", path.display(), e);
            code_string
        }
    }
}

fn write_terminal(root_path: &std::path::Path, terminal: String, structs: Vec<(String, TokenStream)>) {
    let term_path = root_path.join(&terminal);
    let mut root_term = TokenStream::new();
    let mut sub_structs = Vec::new();
    for (curr_struct, code) in structs {
        if curr_struct == terminal {
            root_term.extend([code]);
        } else {
            sub_structs.push(Ident::new(&curr_struct, Span::call_site()));
            let curr_path = term_path.join(format!("{}.rs", curr_struct));
            let mut file = File::create(&curr_path).unwrap();
            file.write_all(pretty_code(quote!(
                #![allow(non_snake_case)] 
                #code
            ), &curr_path).as_bytes()).unwrap();
        }
    }
    let file_path = root_path.join(format!("{}.rs", terminal));
    let mut file = File::create(&file_path).unwrap();
    file.write_all(pretty_code(quote!(
        #![allow(non_snake_case)]

        #(pub(crate) mod #sub_structs;)*
        #root_term
    ), &file_path).as_bytes()).unwrap();
}
