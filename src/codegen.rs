mod metadata;
mod wag;
mod rule;
mod rhs;
mod symbol;

use std::{rc::Rc, collections::HashSet, eprintln, fs::write, path::Path, print, println};
use proc_macro2::{TokenStream, Ident};
use quote::{quote, format_ident};
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
	code: HashMap<Rc<Ident>, Vec<TokenStream>>,
	roots: HashSet<Rc<Ident>>
}

impl CodeGenState {
    fn add_code(&mut self, label: Rc<Ident>, tokens: TokenStream) {
    	if let Some(stream) = self.code.get_mut(&label) {
    		stream.push(tokens);
    	} else {
    		self.code.insert(label, vec![tokens]);
    	}
    }

    fn gen_struct_stream(&self) -> TokenStream {
    	let mut stream = TokenStream::new();
    	for (i, (id, firsts)) in self.first_queue.iter().enumerate() {
    		let mut has_eps = false;
    		let mut first_stream = Vec::new();
    		for (alt, fin) in firsts {
    			let byte = match fin {
			        Some(CharByte::Byte(b)) => quote!(Some(#b)),
			        Some(CharByte::Epsilon) => {has_eps = true; quote!(Some(0))},
			        None => quote!(None),
			    };
			    first_stream.push(quote!(
			    	(vec![#(state.get_label(#alt),)*], #byte)
			    ));
    		}
    		let str_repr = id.to_string();
    		let code = self.code.get(id).unwrap();
    		stream.extend(quote!(
    			struct #id;

    			impl<'a> wagon::gll::Label<'a> for #id {
    				fn first_set(&'a self, state: &GLLState<'a>) -> Vec<(Vec<&'a dyn Label>, Option<TerminalBit>)> {
    					vec![#(#first_stream,)*]
    				}
    				fn is_eps(&self) -> bool {
    					#has_eps
    				}
    				fn to_string(&self) -> &str {
    					#str_repr
    				}
    				fn code(&self, state: &mut wagon::gll::state::GLLState<'a>) {
    					#(#code)*
    				}
    			}
    		));
    		if i == 0 {
    			stream.extend(quote!(
		    		struct _S;

		    		impl<'a> wagon::gll::Label<'a> for _S {
		    			fn first_set(&'a self, state: &GLLState<'a>) -> Vec<(Vec<&'a dyn Label>, Option<TerminalBit>)> {
							vec![(vec![state.get_label_by_uuid(#str_repr)], None)]
						}
						fn is_eps(&self) -> bool {
							false
						}
						fn to_string(&self) -> &str {
							"S'"
						}
						fn code(&self, state: &mut wagon::gll::state::GLLState<'a>) {
							unreachable!("This should never be called")
						}
		    		}
		    	));
    		}

    	}
    	stream
    }

    fn gen_state_stream(&self) -> TokenStream {
    	let mut stream = TokenStream::new();
    	for (i, (id, alts)) in self.first_queue.iter().enumerate() {
    		let str_repr = id.to_string();
    		let label_id = format_ident!("label_{}", i);
    		stream.extend(quote!(
    			let #label_id = Rc::new(#id{});
    			label_map.insert(#str_repr, #label_id);
    		));
    		if self.roots.contains(id) {
    			for (j, (alt, _)) in alts.iter().enumerate() {
    				let rule_id = format!("{}_{}", id, j);
    				let rule_var = format_ident!("alt_{}", rule_id);
    				stream.extend(quote!(
    					let #rule_var = vec![#(#alt,)*];
    					rule_map.insert(#rule_id, #rule_var);
    				));
    			}
    		}
    		if i == 0 {
    			stream.extend(quote!(
    				label_map.insert("S'", Rc::new(_S{}));
    				rule_map.insert("S'", vec![wagon::gll::ident::Ident::Unknown(#str_repr.to_string())]);
    			));
    		}
    	}
    	let label_len = self.first_queue.len();
    	let root_len = self.roots.len();
    	quote!(
    		fn main() {
    			let label_map = HashMap::with_capacity(#label_len);
    			let rule_map = HashMap::with_capacity(#root_len);
    			#stream
    			let args: Vec<String> = env::args().collect();
			    let input_file: &String = &args[1];
			    let contents = fs::read_to_string(input_file).expect("Couldn't read file");
    			let state = wagon::gll::state::GLLState::init(contents.into_bytes(), label_map, rule_map);
    			state.main();
    		}
    	)
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
	let structs = state.gen_struct_stream();
	let main = state.gen_state_stream();
	let fin = quote!(
		#structs
		#main
	);
	let tree = syn::parse_file(&fin.to_string()).unwrap();
	let formatted = prettyplease::unparse(&tree);
	print!("{}", formatted);

}

fn parse_error(err: WagParseError) {
	eprintln!("{}", err);
}