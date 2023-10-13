mod metadata;
mod wag;
mod rule;
mod rhs;
mod symbol;

use std::rc::Rc;
use proc_macro2::{TokenStream, Ident};
use quote::quote;
use std::{todo, collections::{HashMap}};

use crate::{parser::{Parser, WagParseError, wag::Wag}};

#[derive(Debug, PartialEq, Eq, Hash)]
pub(crate) enum CharByte {
	Epsilon,
	Byte(u8)
}

#[derive(Debug, Default)]
pub(crate) struct CodeGenState {
	// A queue of NTs to follow to fill the first set, per alt. Optionally, if we exhaust the queue for an alt we add the final T to the first set
	first_queue: HashMap<Rc<Ident>, Vec<(Vec<crate::gll::ident::Ident>, Option<CharByte>)>>,
	code: HashMap<Rc<Ident>, Vec<TokenStream>>
}

impl CodeGenState {
    fn add_code(&mut self, label: Rc<Ident>, tokens: TokenStream) {
    	if let Some(stream) = self.code.get_mut(&label) {
    		stream.push(tokens);
    	} else {
    		self.code.insert(label, vec![tokens]);
    	}
    }

    fn gen_stream(self) -> TokenStream {
    	let mut stream = TokenStream::new();
    	for (id, firsts) in self.first_queue { // All are "root"
    		let mut has_eps = false;
    		for (alt, fin) in firsts {
    			let byte = match fin {
			        Some(CharByte::Byte(b)) => Some(b),
			        Some(CharByte::Epsilon) => {has_eps = true; Some(0)},
			        None => None,
			    };
			    let label_list = Vec::new();
			    for label in alt {

			    }
    		}
    		let str_repr = id.to_string();
    		let code = self.code.get(&id).unwrap();
    		stream.extend(quote!(
    			struct #id;

    			impl<'a> wagon::gll::Label<'a> for #id {
    				fn is_eps(&self) -> bool {
    					#has_eps
    				}
    				fn get_root(&self) -> &dyn wagon::gll::Label {
    					self
    				}
    				fn to_string(&self) -> &str {
    					#str_repr
    				}
    				fn code(&self, state: &mut GLLState<'a>) {
    					#(#code)*
    				}
    			}
    		));
    	}
    	stream
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
	let mut state = CodeGenState::default();
	wag.gen(&mut state);
	let stream = state.gen_stream();

}

fn parse_error(err: WagParseError) {
	todo!()
}