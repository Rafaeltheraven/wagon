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
use wagon_utils::handle_error;
use wagon_parser::parse_and_check;
use wagon_codegen_gll::gen_parser;
use wagon_codegen::FileStructure;

fn main() {
    let args = Box::leak(Box::new(parse_args()));
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
                Err(e) => handle_error(vec![e], input_file.to_str().expect("Input file path was empty"), &contents, 0).expect("Failed to construct error reporter"),
            }
        },
        Err(e) => handle_error(vec![e], input_file.to_str().expect("Input file path was empty"), &contents, 0).expect("Failed to construct error reporter"),
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

fn write_parser(files: FileStructure, proj_name: &PathBuf, overwrite: bool) {
    let do_cargo = create_structure(proj_name, files, overwrite);
    if do_cargo {
        create_cargo(proj_name);
    }
    cargo_fix(proj_name);
}

fn create_structure(proj_name: &PathBuf, files: FileStructure, overwrite: bool) -> bool {
    let path = std::path::Path::new(proj_name);
    let mut exists = path.exists();
    if exists && overwrite {
        println!("{proj_name:?} already exists. Overwriting!");
        std::fs::remove_dir_all(path).expect("Failed to remove project dir");
        exists = false;
    }
    if !exists {
        std::fs::create_dir(path).expect("Failed to create project directory");
    }
    let mut tree = FileStructure::from_path("");
    let src = tree.insert_dir("src").expect("Failed to insert src into file tree");
    src.insert(files).expect("Failed to insert children to src");
    tree.write_to_disk(path).expect("Failed to write files to disk");
    !exists
}

fn create_cargo(proj_name: &PathBuf) {
    println!("Setting up Cargo");
    let path = std::path::Path::new(proj_name);
    let libs = ["subprocess", "serde_json", "rand_dist", "itertools", "regex_automata"];
    let local_libs = ["wagon-gll", "wagon-ident", "wagon-value"];
    let mut toml = File::create(path.join("Cargo.toml")).expect("Failed to create Cargo.toml");
    toml.write_all(format!(
"[package]
name = \"{}\"
version = \"1.0.0\"
edition = \"2021\"

# See more keys and their definitions at https://doc.rust-lang.org/cargo/reference/manifest.html

[workspace]

[dependencies]", proj_name.display()).as_bytes()).expect("Failed to write Cargo.toml");
    println!("Setting up dependencies");
    Command::new("cargo").current_dir(path).args(["add", "clap", "--features", "derive,cargo"]).output().expect("Failed to add clap library");
    Command::new("cargo").current_dir(path).args(["add", "wagon-utils", "--path", "../wagon-utils", "--features", "error_printing"]).output().expect("Failed to add wagon-utils library");
    for lib in local_libs {
        let lib_path = format!("../{lib}");
        Command::new("cargo").current_dir(path).args(["add", lib, "--path", &lib_path]).output().unwrap_or_else(|_| panic!("Failed to add {lib} library"));
    }
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
