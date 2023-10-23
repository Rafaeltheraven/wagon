mod metadata;
mod wag;
mod rule;
mod rhs;
mod symbol;

use std::{rc::Rc, collections::HashSet};
use proc_macro2::{TokenStream, Ident};
use quote::{quote, format_ident};
use std::{collections::{HashMap}};

use crate::{parser::{Parser, WagParseError, wag::Wag}};

#[derive(Debug, PartialEq, Eq, Hash)]
pub(crate) enum CharByte {
	Epsilon,
	Byte(u8)
}

#[derive(Debug, Default)]
pub(crate) struct CodeGenState {
	// A queue of NTs to follow to fill the first set, per alt. Optionally, if we exhaust the queue for an alt we add the final T to the first set
	first_queue: HashMap<Rc<Ident>, Vec<(Vec<wagon_gll::ident::Ident>, Option<CharByte>)>>,
	code: HashMap<Rc<Ident>, Vec<TokenStream>>,
	roots: HashSet<Rc<Ident>>,
	top: Option<Rc<Ident>>,
	str_repr: HashMap<Rc<Ident>, String>
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
    	for (_i, (id, firsts)) in self.first_queue.iter().enumerate() {
    		let mut has_eps = false;
    		let mut first_stream = Vec::new();
    		for (alt, fin) in firsts {
    			let byte = match fin {
			        Some(CharByte::Byte(b)) => quote!(Some(&#b)),
			        Some(CharByte::Epsilon) => {has_eps = true; quote!(Some(&0))},
			        None => quote!(None),
			    };
			    first_stream.push(quote!(
			    	(vec![#(state.get_label(&#alt),)*], #byte)
			    ));
    		}
    		let uuid = id.to_string();
    		let str_repr = self.str_repr.get(id).unwrap();
    		let code = self.code.get(id).unwrap();
    		stream.extend(quote!(
    			#[derive(Debug)]
    			struct #id;

    			impl<'a> wagon_gll::Label<'a> for #id {
    				fn first_set(&self, state: &wagon_gll::state::GLLState<'a>) -> Vec<(Vec<std::rc::Rc<dyn wagon_gll::Label<'a>>>, Option<wagon_gll::TerminalBit>)> {
    					vec![#(#first_stream,)*]
    				}
    				fn is_eps(&self) -> bool {
    					#has_eps
    				}
    				fn uuid(&self) -> &str {
    					#uuid
    				}
    				fn code(&self, state: &mut wagon_gll::state::GLLState<'a>) {
    					#(#code)*
    				}
    				fn to_string(&self) -> &str {
    					#str_repr
    				}
    			}
    		));
    		if Some(id) == self.top.as_ref() {
    			stream.extend(quote!(
    				#[derive(Debug)]
		    		struct _S;

		    		impl<'a> wagon_gll::Label<'a> for _S {
		    			fn first_set(&self, state: &wagon_gll::state::GLLState<'a>) -> Vec<(Vec<std::rc::Rc<dyn wagon_gll::Label<'a>>>, Option<wagon_gll::TerminalBit>)> {
							vec![(vec![state.get_label_by_uuid(#uuid)], None)]
						}
						fn is_eps(&self) -> bool {
							false
						}
						fn uuid(&self) -> &str {
							wagon_gll::ROOT_UUID
						}
						fn to_string(&self) -> &str {
							wagon_gll::ROOT_UUID
						}
						fn code(&self, _: &mut wagon_gll::state::GLLState<'a>) {
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
    			let #label_id = std::rc::Rc::new(#id{});
    			label_map.insert(#str_repr, #label_id);
    		));
    		if self.roots.contains(id) {
    			for (j, (alt, _)) in alts.iter().enumerate() {
    				let rule_id = format!("{}_{}", id, j);
    				let rule_var = format_ident!("alt_{}", rule_id);
    				stream.extend(quote!(
    					let #rule_var = std::rc::Rc::new(vec![#(#alt,)*]);
    					rule_map.insert(#rule_id, #rule_var);
    				));
    			}
    		}
    		if i == 0 {
    			stream.extend(quote!(
    				label_map.insert(wagon_gll::ROOT_UUID, std::rc::Rc::new(_S{}));
    				rule_map.insert(wagon_gll::ROOT_UUID, std::rc::Rc::new(vec![wagon_gll::ident::Ident::Unknown(#str_repr.to_string())]));
    			));
    		}
    	}
    	let label_len = self.first_queue.len();
    	let root_len = self.roots.len();
    	quote!(
    		fn main() {
    			let args: Vec<String> = std::env::args().collect();
			    let input_file: &String = &args[1];
			    let contents = std::fs::read_to_string(input_file).expect("Couldn't read file").into_bytes();
    			let mut label_map: std::collections::HashMap<&str, std::rc::Rc<dyn wagon_gll::Label>> = std::collections::HashMap::with_capacity(#label_len);
    			let mut rule_map: std::collections::HashMap<&str, std::rc::Rc<Vec<wagon_gll::ident::Ident>>> = std::collections::HashMap::with_capacity(#root_len);
    			#stream
    			let mut state = wagon_gll::state::GLLState::init(&contents, label_map, rule_map);
    			state.main();
    		}
    	)
    }
}

pub(crate) fn gen_parser(input: &str) -> Result<String, WagParseError> {
	let mut g_parser = Parser::new(input);
	let wag = g_parser.parse()?;
	Ok(_gen_parser(wag))
}

fn _gen_parser(wag: Wag) -> String {
	let mut state = CodeGenState::default();
	wag.gen(&mut state);
	let structs = state.gen_struct_stream();
	let main = state.gen_state_stream();
	let fin = quote!(
		#structs
		#main
	);
	let tree = syn::parse_file(&fin.to_string()).unwrap();
	prettyplease::unparse(&tree)
}
