mod metadata;
mod wag;
mod rule;
mod rhs;
mod symbol;

use std::rc::Rc;
use proc_macro2::TokenStream;
use std::{todo, collections::{HashMap, HashSet}};

use crate::{parser::{Parser, WagParseError, wag::Wag}, lexer::ident::Ident};

#[derive(Debug)]
pub(crate) struct CodeGenState {
	lexer: HashMap<Rc<str>, TokenStream>,
	follow: HashMap<Rc<str>, HashSet<Rc<str>>>,
	// A queue of NTs to follow to fill the first set, per alt. Optionally, if we exhaust the queue for an alt we add the final T to the first set
	first_queue: HashMap<Rc<str>, Vec<(Vec<Ident>, Option<Rc<str>>)>>,
	epsilon: Rc<str>,
}

impl CodeGenState {
    fn default() -> Self {
        Self {
        	epsilon: "_Epsilon".into(),
        	lexer: Default::default(),
        	follow: Default::default(),
        	first_queue: Default::default(),
        }
    }
}

pub fn gen_parser(input: &str) {
	let mut g_parser = Parser::new(input);
	match g_parser.parse() {
	    Ok(w) => _gen_parser(w),
	    Err(e) => parse_error(e),
	}
}

fn _gen_parser(wag: Wag) {
	let stream = wag.gen(&mut CodeGenState::default());

}

fn parse_error(err: WagParseError) {
	todo!()
}