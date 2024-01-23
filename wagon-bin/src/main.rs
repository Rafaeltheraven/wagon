#![allow(clippy::expect_used)]
#![allow(clippy::panic)]
use std::env;
use std::format;
use std::fs;
use std::fs::File;
use std::io::Write;
use std::path::PathBuf;
use std::process::Command;

use clap::ArgMatches;
use wagon_parser::MsgAndSpan;
use wagon_parser::parse_and_check;
use wagon_codegen_gll::gen_parser;
use wagon_codegen::CodeMap;
use proc_macro2::Ident;
use proc_macro2::Span;
use proc_macro2::TokenStream;
use quote::quote;

fn main() {
    let args = parse_args();
    let input_file = args
        .get_one::<std::path::PathBuf>("filename")
        .expect("Input file required");
    let proj_name = args
        .get_one::<std::path::PathBuf>("project_name")
        .expect("Project name required");
    let overwrite = args.get_one::<bool>("overwrite").unwrap_or(&false);
    let contents = fs::read_to_string(input_file).expect("Couldn't read file");
    match parse_and_check(&contents) {
        Ok(wag) => {
            match gen_parser(wag) {
                Ok(code) => write_parser(code, proj_name, *overwrite),
                Err(e) => handle_error(e, input_file.to_str().expect("Input file path was empty"), contents),
            }
        },
        Err(e) => handle_error(e, input_file.to_str().expect("Input file path was empty"), contents),
    }
}

#[allow(clippy::cognitive_complexity)]
fn parse_args() -> ArgMatches {
    clap::command!()
        .arg(
            clap::arg!(< filename > "The input WAGon grammar file")
                .value_parser(clap::value_parser!(std::path::PathBuf)),
        )
        .arg(
            clap::arg!(< project_name > "The name of the project to output")
                .value_parser(clap::value_parser!(std::path::PathBuf)),
        )
        .arg(clap::arg!(- - "overwrite" "Delete any existing project with the same name").num_args(0))
        .get_matches()
}

fn handle_error<T: MsgAndSpan>(err: T, file_path: &str, file: String) {
    use ariadne::{ColorGenerator, Label, Report, ReportKind, Source};
    let mut colors = ColorGenerator::new();
    let a = colors.next();
    let ((head, msg), span) = err.msg_and_span();
    Report::build(ReportKind::Error, file_path, 0)
        .with_message(head)
        .with_label(
            Label::new((file_path, span))
                .with_message(msg)
                .with_color(a),
        )
        .finish()
        .eprint((file_path, Source::from(file)))
        .expect("Failed to construct error reporter");
}

fn write_parser(data: CodeMap, proj_name: &PathBuf, overwrite: bool) {
    let (subcode, code) = data;
    let root_terms: Vec<&String> = subcode.keys().collect();
    let do_cargo = create_structure(proj_name, &root_terms, overwrite);
    let path = std::path::Path::new(proj_name).join("src");
    let main_path = path.join("main.rs");
    let mut file = File::create(&main_path).expect("Failed to create main.rs");
    file.write_all(pretty_code(&code, &main_path).as_bytes()).expect("Failed to write to main.rs");
    let term_path = path.join("terminals");
    let term_file_path = path.join(format!("{}.rs", "terminals"));
    let mut term_file = File::create(&term_file_path).expect("Failed to create terminals.rs");
    let root_terms_idents = root_terms.iter().map(|x| Ident::new(x, Span::call_site()));
    term_file.write_all(pretty_code(&quote!(
        #![allow(non_snake_case)]

        #(pub(crate) mod #root_terms_idents;)*
    ), &term_file_path).as_bytes()).expect("Failed to write to terminals.rs");
    for (terminal, structs) in subcode {
        write_terminal(&term_path, &terminal, structs);
    }
    if do_cargo {
        create_cargo(proj_name);
    }
    cargo_fix(proj_name);
}

fn create_structure(proj_name: &PathBuf, terminals: &Vec<&String>, overwrite: bool) -> bool {
    let path = std::path::Path::new(proj_name);
    let mut exists = path.exists();
    if exists && overwrite {
        println!("{proj_name:?} already exists. Overwriting!");
        std::fs::remove_dir_all(path).expect("Failed to remove project dir");
        exists = false;
    }
    let src_path = path.join("src");
    let term_path = src_path.join("terminals");
    if !exists {
        std::fs::create_dir_all(src_path).expect("Failed to create project directory");
        std::fs::create_dir(&term_path).expect("Failed to create terminals directory");
    }
    for term in terminals {
        let curr_path = term_path.join(term);
        if !curr_path.exists() {
            std::fs::create_dir(curr_path).unwrap_or_else(|_| panic!("Failed to create {term} directory"));
        }
    }
    !exists
}

fn create_cargo(proj_name: &PathBuf) {
    let path = std::path::Path::new(proj_name);
    let libs = ["subprocess", "serde_json", "rand_dist", "itertools"];
    let mut toml = File::create(path.join("Cargo.toml")).expect("Failed to create Cargo.toml");
    toml.write_all(format!(
"[package]
name = \"{}\"
version = \"1.0.0\"
edition = \"2021\"

# See more keys and their definitions at https://doc.rust-lang.org/cargo/reference/manifest.html

[workspace]

[dependencies]", proj_name.display()).as_bytes()).expect("Failed to write Cargo.toml");
    Command::new("cargo") 
        .current_dir(path)
        .args(["add", "wagon-gll", "--path", "../wagon-gll"])
        .output()
        .expect("Failed to add wagon-gll library");
    Command::new("cargo") 
        .current_dir(path)
        .args(["add", "wagon-ident", "--path", "../wagon-ident"])
        .output()
        .expect("Failed to add wagon-ident library");
    Command::new("cargo").current_dir(path).args(["add", "clap", "--features", "derive,cargo"]).output().expect("Failed to add clap library");
    for lib in libs {
        Command::new("cargo").current_dir(path).args(["add", lib]).output().unwrap_or_else(|_| panic!("Failed to add {lib} library"));
    }
}

fn cargo_fix(proj_name: &PathBuf) {
    let path = std::path::Path::new(proj_name);
    Command::new("cargo") 
        .current_dir(path)
        .args(["fix", "--bin", proj_name.to_str().expect("Path was empty"), "--allow-no-vcs"])
        .output()
        .expect("Failed to cargo fix");
}

fn pretty_code(code: &TokenStream, path: &std::path::Path) -> String {
    let code_string = code.to_string();
    match syn::parse_file(&code_string) {
        Ok(tree) => prettyplease::unparse(&tree),
        Err(e) => {
            eprintln!("Error parsing code for {}: {}", path.display(), e);
            code_string
        }
    }
}

fn write_terminal(root_path: &std::path::Path, terminal: &str, structs: Vec<(String, TokenStream)>) {
    let term_path = root_path.join(terminal);
    let mut root_term = TokenStream::new();
    let mut sub_structs = Vec::new();
    for (curr_struct, code) in structs {
        if curr_struct == terminal {
            root_term.extend([code]);
        } else {
            sub_structs.push(Ident::new(&curr_struct, Span::call_site()));
            let curr_path = term_path.join(format!("{curr_struct}.rs"));
            let mut file = File::create(&curr_path).expect("Failed to create terminal file");
            file.write_all(pretty_code(&quote!(
                #![allow(non_snake_case)] 
                #code
            ), &curr_path).as_bytes()).expect("Failed to write terminal file");
        }
    }
    let file_path = root_path.join(format!("{terminal}.rs"));
    let mut file = File::create(&file_path).expect("Failed to create terminal.rs file");
    file.write_all(pretty_code(&quote!(
        #![allow(non_snake_case)]

        #(pub(crate) mod #sub_structs;)*
        #root_term
    ), &file_path).as_bytes()).expect("Failed to write terminal.rs file");
}
