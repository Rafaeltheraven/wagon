mod metadata;
mod wag;
mod rule;
mod rhs;
mod symbol;
mod assignment;
mod expression;
mod disjunct;
mod conjunct;
mod inverse;
mod comp;
mod sum;
mod term;
mod factor;
mod atom;

use std::{rc::Rc, collections::HashSet};
use proc_macro2::{TokenStream, Ident, Literal};
use quote::{quote, format_ident};
use std::{collections::{HashMap}};

use crate::{parser::{Parser, WagParseError, wag::Wag}, firstpass::{FirstPassState, Rewrite}};

#[derive(Debug)]
pub(crate) enum CharBytes {
	Epsilon,
	Bytes(Literal)
}

type FirstSet = (Vec<wagon_gll::ident::Ident>, Option<CharBytes>);

type AttrSet = HashSet<wagon_gll::ident::Ident>;
type ReqCodeAttrs = AttrSet;
type ReqWeightAttrs = AttrSet;
type ReqFirstAttrs = AttrSet;

pub(crate) type CodeMap = (HashMap<String, Vec<(String, TokenStream)>>, TokenStream);

#[derive(Debug, Default)]
pub(crate) struct CodeGenState {
	// A queue of NTs to follow to fill the first set, per alt. Optionally, if we exhaust the queue for an alt we add the final T to the first set
	first_queue: HashMap<Rc<Ident>, Vec<FirstSet>>,
	code: HashMap<Rc<Ident>, Vec<TokenStream>>,
	weight_code: HashMap<Rc<Ident>, Vec<TokenStream>>,
	roots: HashSet<Rc<Ident>>,
	top: Option<Rc<Ident>>,
	str_repr: HashMap<Rc<Ident>, Vec<String>>,
	attr_repr: HashMap<Rc<Ident>, (Vec<String>, Vec<String>)>,
	call_info: FirstPassState,
	attribute_map: HashMap<Rc<Ident>, HashMap<wagon_gll::ident::Ident, TokenStream>>,
	req_attribute_map: HashMap<Rc<Ident>, (ReqCodeAttrs, ReqWeightAttrs, ReqFirstAttrs)>
}

impl CodeGenState {
	fn set_call_info(&mut self, state: FirstPassState) {
		self.call_info = state;
	}

    fn add_code(&mut self, label: Rc<Ident>, tokens: TokenStream) {
    	if let Some(streams) = self.code.get_mut(&label) {
    		streams.push(tokens);
    	} else {
    		self.code.insert(label, vec![tokens]);
    	}
    }

    fn add_weight_code(&mut self, label: Rc<Ident>, tokens: TokenStream) {
    	if let Some(streams) = self.weight_code.get_mut(&label) {
    		streams.push(tokens);
    	} else {
    		self.weight_code.insert(label, vec![tokens]);
    	}
    }

    fn add_req_code_attr(&mut self, label: Rc<Ident>, ident: wagon_gll::ident::Ident) {
    	if let Some((attrs, _, _)) = self.req_attribute_map.get_mut(&label) {
    		attrs.insert(ident);
    	} else {
    		let mut new_set = HashSet::new();
    		new_set.insert(ident);
    		self.req_attribute_map.insert(label, (new_set, HashSet::new(), HashSet::new()));
    	}
    }

    fn add_req_weight_attr(&mut self, label: Rc<Ident>, ident: wagon_gll::ident::Ident) {
    	if let Some((_, attrs, _)) = self.req_attribute_map.get_mut(&label) {
    		attrs.insert(ident);
    	} else {
    		let mut new_set = HashSet::new();
    		new_set.insert(ident);
    		self.req_attribute_map.insert(label, (HashSet::new(), new_set, HashSet::new()));
    	}
    }

    fn add_req_first_attr(&mut self, label: Rc<Ident>, ident: wagon_gll::ident::Ident) {
    	if let Some((_, _, attrs)) = self.req_attribute_map.get_mut(&label) {
    		attrs.insert(ident);
    	} else {
    		let mut new_set = HashSet::new();
    		new_set.insert(ident);
    		self.req_attribute_map.insert(label, (HashSet::new(), HashSet::new(), new_set));
    	}
    }

    fn get_req_code_attrs(&self, label: &Ident) -> Option<&ReqCodeAttrs> {
    	if let Some((attrs, _, _)) = self.req_attribute_map.get(label) {
    		Some(attrs)
    	} else {
    		None
    	}
    }

    fn get_req_weight_attrs(&self, label: &Ident) -> Option<&ReqWeightAttrs> {
    	if let Some((_, attrs, _)) = self.req_attribute_map.get(label) {
    		Some(attrs)
    	} else {
    		None
    	}
    }

    fn get_req_first_attrs(&self, label: &Ident) -> Option<&ReqFirstAttrs> {
    	if let Some((_, _, attrs)) = self.req_attribute_map.get(label) {
    		Some(attrs)
    	} else {
    		None
    	}
    }

    fn collect_attrs(&self, label: &Ident, a: Option<&AttrSet>) -> Vec<&TokenStream> {
    	let mut stream = Vec::new();
    	if let Some(attrs) = a { // If we have any associated attrs
    		if let Some(map) = self.attribute_map.get(label) { // And this block uses them
    			for attr in attrs {
	    			if let Some(code) = map.get(attr) { // And they are not defined inside the block itself
	    				stream.push(code);
	    			}
	    		}
    		}
    	}
    	stream
    }

    fn add_attribute_mapping(&mut self, label: Rc<Ident>, ident: &wagon_gll::ident::Ident, code: TokenStream) {
    	if let Some(inner_map) = self.attribute_map.get_mut(&label) {
    		if let Some(stream) = inner_map.get_mut(ident) {
    			stream.extend(code);
    		} else {
    			inner_map.insert(ident.to_owned(), code);
    		}
    	} else {
    		let mut new_map = HashMap::new();
    		new_map.insert(ident.to_owned(), code);
    		self.attribute_map.insert(label, new_map);
    	}
    }

    fn add_ret_attr(&mut self, label: Rc<Ident>, attr: String) {
    	if let Some((ret, _)) = self.attr_repr.get_mut(&label) {
    		ret.push(attr);
    	} else {
    		self.attr_repr.insert(label, (vec![attr], Vec::new()));
    	}
    }

    fn add_ctx_attr(&mut self, label: Rc<Ident>, attr: String) {
    	if let Some((_, ctx)) = self.attr_repr.get_mut(&label) {
    		ctx.push(attr);
    	} else {
    		self.attr_repr.insert(label, (Vec::new(), vec![attr]));
    	}
    }

    fn gen_struct_stream(&self) -> CodeMap {
    	let mut code_map = HashMap::from_iter(self.roots.iter().map(|x| (x.to_string(), Vec::new())));
    	let empty_repr = (Vec::new(), Vec::new());
    	let mut main_stream = TokenStream::new();
    	for (_i, (id, firsts)) in self.first_queue.iter().enumerate() {
    		let mut stream = TokenStream::new();
    		let mut has_eps = false;
    		let mut first_stream = Vec::new();
    		for (alt, fin) in firsts {
    			let byte = match fin {
			        Some(CharBytes::Bytes(b)) => quote!(Some(#b)),
			        Some(CharBytes::Epsilon) => {has_eps = true; quote!(Some(&[]))},
			        None => quote!(None),
			    };
			    let mut vec_stream = Vec::new();
			    for ident in alt {
			    	let stream = match ident {
			            wagon_gll::ident::Ident::Unknown(s) => quote!(state.get_label_by_uuid(#s)),
			            other => {
			            	let i = other.to_ident();
			            	quote!(#i.into())
			            }
			        };
			        vec_stream.push(stream);
			    }
			    first_stream.push(quote!(
			    	(vec![#(#vec_stream),*], #byte)
			    ));
    		}
    		let first_attr_stream = self.collect_attrs(id, self.get_req_first_attrs(id));
    		let code_attr_stream = self.collect_attrs(id, self.get_req_code_attrs(id));
    		let uuid = id.to_string();
    		let root_uuid = uuid.chars().next().unwrap().to_string();
    		let str_list = self.str_repr.get(id).unwrap();
    		let str_repr = str_list.join(" ");
    		let (pop_repr, ctx_repr) = self.attr_repr.get(id).unwrap_or(&empty_repr);
    		let code = self.code.get(id).unwrap();
    		let weight_stream = if let Some(weight) = self.weight_code.get(id) {
    			let weight_attrs = self.collect_attrs(id, self.get_req_weight_attrs(id));
    			quote!(
    				#(#weight_attrs)*
    				#(#weight)*
    			)
    		} else {
    			quote!(unreachable!("Weight should never be evaluated for non-zero GLL blocks"))
    		};
    		stream.extend(quote!(
    			#[derive(Debug)]
    			#[allow(non_camel_case_types)]
    			pub(crate) struct #id;

    			impl<'a> wagon_gll::Label<'a> for #id {
    				#[allow(unused_variables)]
    				fn first_set(&self, state: &wagon_gll::state::GLLState<'a>) -> Vec<(Vec<wagon_gll::GLLBlockLabel<'a>>, Option<wagon_gll::Terminal<'a>>)> {
    					#(#first_attr_stream)*
    					vec![#(#first_stream,)*]
    				}
    				fn is_eps(&self) -> bool {
    					#has_eps
    				}
    				fn uuid(&self) -> &str {
    					#uuid
    				}
    				#[allow(unused_variables)]
    				fn code(&self, state: &mut wagon_gll::state::GLLState<'a>) {
    					#(#code_attr_stream)*
    					#(#code)*
    				}
    				fn to_string(&self) -> &str {
    					#str_repr
    				}
    				fn str_parts(&self) -> Vec<&str> {
    					vec![#(#str_list,)*]
    				}
    				fn attr_rep_map(&self) -> (Vec<&str>, Vec<&str>) {
    					(vec![#(#pop_repr,)*], vec![#(#ctx_repr,)*])
    				}
    				#[allow(unused_variables)]
    				fn weight(&self, state: &wagon_gll::state::GLLState<'a>) -> wagon_gll::value::Value<'a> {
    					#weight_stream
    				}

    			}
    		));
    		if Some(id) == self.top.as_ref() {
    			main_stream.extend(quote!(
    				#[derive(Debug)]
		    		struct _S;

		    		impl<'a> wagon_gll::Label<'a> for _S {
		    			fn first_set(&self, state: &wagon_gll::state::GLLState<'a>) -> Vec<(Vec<wagon_gll::GLLBlockLabel<'a>>, Option<wagon_gll::Terminal<'a>>)> {
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
							unreachable!("This should never be called");
						}
						fn attr_rep_map(&self) -> (Vec<&str>, Vec<&str>) { 
							(Vec::new(), Vec::new())
						}
						fn weight(&self, _state: &wagon_gll::state::GLLState<'a>) -> wagon_gll::value::Value<'a> {
							unreachable!("This should never be called");
						}
		    		}
		    	));
    		}
    		if let Some(v) = code_map.get_mut(&root_uuid) {
    			v.push((uuid, stream));
    		} else { // just to be sure
    			code_map.insert(root_uuid, vec![(uuid, stream)]);
    		}
    	}
    	(code_map, main_stream)
    }

    fn gen_state_stream(&self) -> TokenStream {
    	let mut stream = TokenStream::new();
    	for (i, (id, alts)) in self.first_queue.iter().enumerate() {
    		let str_repr = id.to_string();
    		let root_path = Ident::new(&str_repr.chars().next().unwrap().to_string(), proc_macro2::Span::call_site());
    		let label_id = format_ident!("label_{}", i);
    		if self.roots.contains(id) {
    			stream.extend(quote!(
	    			let #label_id = std::rc::Rc::new(terminals::#root_path::#id{});
	    			label_map.insert(#str_repr, #label_id);
	    		));
    			for (j, (alt, _)) in alts.iter().enumerate() {
    				let rule_id = format!("{}_{}", id, j);
    				let rule_var = format_ident!("alt_{}", rule_id);
    				stream.extend(quote!(
    					let #rule_var = std::rc::Rc::new(vec![#(#alt,)*]);
    					rule_map.insert(#rule_id, #rule_var);
    				));
    			}
    		} else {
    			stream.extend(quote!(
	    			let #label_id = std::rc::Rc::new(terminals::#root_path::#id::#id{});
	    			label_map.insert(#str_repr, #label_id);
	    		));
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
			            clap::arg!(<filename> "Input file to parse")
			                .value_parser(clap::value_parser!(std::path::PathBuf)),
			        )
			        .arg(
			            clap::arg!(--"no-crop" "Don't crop resulting sppf")
			                .num_args(0),
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

pub(crate) fn gen_parser(input: &str) -> Result<CodeMap, WagParseError> {
	let mut g_parser = Parser::new(input);
	let mut wag = g_parser.parse()?;
	let mut check_state = FirstPassState::default();
	wag.rewrite(0, &mut check_state)?;
	Ok(_gen_parser(wag, check_state))
}

fn _gen_parser(wag: Wag, check_state: FirstPassState) -> CodeMap {
	let mut state = CodeGenState::default();
	state.set_call_info(check_state);
	wag.gen(&mut state);
	let (structs, start) = state.gen_struct_stream();
	let main = state.gen_state_stream();
	let fin = quote!(
		mod terminals;
		#start
		#main
	);
	(structs, fin)
}
