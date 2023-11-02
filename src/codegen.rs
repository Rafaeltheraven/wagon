mod metadata;
mod wag;
mod rule;
mod rhs;
mod symbol;

use std::{rc::Rc, collections::HashSet};
use proc_macro2::{TokenStream, Ident, Literal};
use quote::{quote, format_ident};
use std::{collections::{HashMap}};

use crate::{parser::{Parser, WagParseError, wag::Wag}};

#[derive(Debug)]
pub(crate) enum CharBytes {
	Epsilon,
	Bytes(Literal)
}

#[derive(Debug, Default)]
pub(crate) struct CodeGenState {
	// A queue of NTs to follow to fill the first set, per alt. Optionally, if we exhaust the queue for an alt we add the final T to the first set
	first_queue: HashMap<Rc<Ident>, Vec<(Vec<wagon_gll::ident::Ident>, Option<CharBytes>)>>,
	code: HashMap<Rc<Ident>, Vec<TokenStream>>,
	roots: HashSet<Rc<Ident>>,
	top: Option<Rc<Ident>>,
	str_repr: HashMap<Rc<Ident>, Vec<String>>
}

impl CodeGenState {
    fn add_code(&mut self, label: Rc<Ident>, tokens: TokenStream) {
    	if let Some(streams) = self.code.get_mut(&label) {
    		streams.push(tokens);
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
			        Some(CharBytes::Bytes(b)) => quote!(Some(#b)),
			        Some(CharBytes::Epsilon) => {has_eps = true; quote!(Some(&[]))},
			        None => quote!(None),
			    };
			    first_stream.push(quote!(
			    	(vec![#(state.get_label(&#alt),)*], #byte)
			    ));
    		}
    		let uuid = id.to_string();
    		let str_list = self.str_repr.get(id).unwrap();
    		let str_repr = str_list.join(" ");
    		let code = self.code.get(id).unwrap();
    		stream.extend(quote!(
    			#[derive(Debug)]
    			#[allow(non_camel_case_types)]
    			struct #id;

    			impl<'a> wagon_gll::Label<'a> for #id {
    				#[allow(unused_variables)]
    				fn first_set(&self, state: &wagon_gll::state::GLLState<'a>) -> Vec<(Vec<std::rc::Rc<dyn wagon_gll::Label<'a>>>, Option<wagon_gll::Terminal<'a>>)> {
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
    				fn str_parts(&self) -> Vec<&str> {
    					vec![#(#str_list,)*]
    				}

    			}
    		));
    		if Some(id) == self.top.as_ref() {
    			stream.extend(quote!(
    				#[derive(Debug)]
		    		struct _S;

		    		impl<'a> wagon_gll::Label<'a> for _S {
		    			fn first_set(&self, state: &wagon_gll::state::GLLState<'a>) -> Vec<(Vec<std::rc::Rc<dyn wagon_gll::Label<'a>>>, Option<wagon_gll::Terminal<'a>>)> {
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
						fn str_parts(&self) -> Vec<&str> {
							vec![wagon_gll::ROOT_UUID]
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
    		if Some(id) == self.top.as_ref() {
    			stream.extend(quote!(
    				label_map.insert(wagon_gll::ROOT_UUID, std::rc::Rc::new(_S{}));
    				rule_map.insert(wagon_gll::ROOT_UUID, std::rc::Rc::new(vec![wagon_gll::ident::Ident::Unknown(#str_repr.to_string())]));
    			));
    		}
    	}
    	let label_len = self.first_queue.len();
    	let root_len = self.roots.len();
    	quote!(
    		#[allow(non_snake_case)]
    		fn main() {
    			let args = clap::command!()
			        .arg(
			            clap::arg!([filename] "Input file to parse")
			                .required(true)
			                .index(1)
			                .value_parser(clap::value_parser!(std::path::PathBuf))
			        )
			        .arg(
			            clap::Arg::new("no-crop")
			            .help("Don't crop resulting sppf")
			            .long("no-crop")
			            .required(false)
			            .num_args(0)
			        )
			        .get_matches();
			    let input_file = args.get_one::<std::path::PathBuf>("filename").expect("Input file required");
			    let crop = args.get_one::<bool>("no-crop").unwrap() == &false;
			    let content_string = std::fs::read_to_string(input_file).expect("Couldn't read file");
			    let contents = content_string.trim_end().as_bytes();
    			let mut label_map: std::collections::HashMap<&str, std::rc::Rc<dyn wagon_gll::Label>> = std::collections::HashMap::with_capacity(#label_len);
    			let mut rule_map: std::collections::HashMap<&str, std::rc::Rc<Vec<wagon_gll::ident::Ident>>> = std::collections::HashMap::with_capacity(#root_len);
    			#stream
    			let mut state = wagon_gll::state::GLLState::init(&contents, label_map, rule_map);
    			state.main();
    			println!("{}", state.print_sppf_dot(crop));
    			assert!(state.accepts());
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
