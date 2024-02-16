
use regex::Regex;
use regex_automata::dfa::dense::DFA;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::rc::Rc;
use std::collections::{HashSet, HashMap};

use proc_macro2::{TokenStream, Ident};
use quote::{quote, format_ident};

use wagon_codegen::{FileStructure, SpannableIdent};

use crate::{FirstSet, CharBytes, AttrSet, ReqCodeAttrs, ReqWeightAttrs, ReqFirstAttrs, CodeGenResult, CodeGenError, CodeGenErrorKind};

const EMPTY_REPR: (Vec<String>, Vec<String>) = (Vec::new(), Vec::new());

const DELIM_REGEX: &str = r"_[0-9]";

#[derive(Debug, Default)]
/// The state object that is passed along to [`ToTokensState::to_tokens`](`wagon_codegen::ToTokensState::to_tokens`)
pub struct CodeGenState {
	/// A queue of NTs to follow to fill the first set, per alt. Optionally, if we exhaust the queue for an alt we add the final T to the first set.
	pub(crate) first_queue: HashMap<Rc<Ident>, Vec<FirstSet>>,
	/// A representation of the first_queue but with Idents instead of TokenStreams (yes this is horrible).
	pub(crate) first_idents: HashMap<Rc<Ident>, Vec<Vec<SpannableIdent>>>,
	/// The body of the `code` method for some non-terminal.
	pub(crate) code: HashMap<Rc<Ident>, Vec<TokenStream>>,
	/// The body of the `weight` method for some non-terminal.
	pub(crate) weight_code: HashMap<Rc<Ident>, Vec<TokenStream>>,
	pub(crate) roots: HashSet<Rc<Ident>>,
	pub(crate) top: Option<Rc<Ident>>,
	pub(crate) str_repr: HashMap<Rc<Ident>, Vec<String>>,
	pub(crate) attr_repr: HashMap<Rc<Ident>, (Vec<String>, Vec<String>)>,
	pub(crate) attribute_map: HashMap<Rc<Ident>, HashMap<SpannableIdent, TokenStream>>,
	pub(crate) req_attribute_map: HashMap<Rc<Ident>, (ReqCodeAttrs, ReqWeightAttrs, ReqFirstAttrs)>,
	pub(crate) alt_map: HashMap<Rc<Ident>, Vec<Rc<Ident>>>,
	pub(crate) regexes: Vec<(String, Box<DFA<Vec<u32>>>)>,
	pub(crate) files: FileStructure
}

impl CodeGenState {

    pub(crate) fn add_code(&mut self, label: Rc<Ident>, tokens: TokenStream) {
    	if let Some(streams) = self.code.get_mut(&label) {
    		streams.push(tokens);
    	} else {
    		self.code.insert(label, vec![tokens]);
    	}
    }

    pub(crate) fn add_weight_code(&mut self, label: Rc<Ident>, tokens: TokenStream) {
    	if let Some(streams) = self.weight_code.get_mut(&label) {
    		streams.push(tokens);
    	} else {
    		self.weight_code.insert(label, vec![tokens]);
    	}
    }

    pub(crate) fn add_req_code_attr(&mut self, label: Rc<Ident>, ident: SpannableIdent) {
    	if let Some((attrs, _, _)) = self.req_attribute_map.get_mut(&label) {
    		attrs.insert(ident);
    	} else {
    		let mut new_set = HashSet::new();
    		new_set.insert(ident);
    		self.req_attribute_map.insert(label, (new_set, HashSet::new(), HashSet::new()));
    	}
    }

    pub(crate) fn add_req_weight_attr(&mut self, label: Rc<Ident>, ident: SpannableIdent) {
    	if let Some((_, attrs, _)) = self.req_attribute_map.get_mut(&label) {
    		attrs.insert(ident);
    	} else {
    		let mut new_set = HashSet::new();
    		new_set.insert(ident);
    		self.req_attribute_map.insert(label, (HashSet::new(), new_set, HashSet::new()));
    	}
    }

    pub(crate) fn add_req_first_attr(&mut self, label: Rc<Ident>, ident: SpannableIdent) {
    	if let Some((_, _, attrs)) = self.req_attribute_map.get_mut(&label) {
    		attrs.insert(ident);
    	} else {
    		let mut new_set = HashSet::new();
    		new_set.insert(ident);
    		self.req_attribute_map.insert(label, (HashSet::new(), HashSet::new(), new_set));
    	}
    }

    pub(crate) fn get_first(&mut self, label: &Rc<Ident>) -> CodeGenResult<&mut Vec<FirstSet>> {
    	self.first_queue.get_mut(label).ok_or_else(|| CodeGenError::new(CodeGenErrorKind::MissingFirst(label.clone())))
    }

    pub(crate) fn get_first_ident(&mut self, label: &Rc<Ident>) -> CodeGenResult<&mut Vec<Vec<SpannableIdent>>> {
    	self.first_idents.get_mut(label).ok_or_else(|| CodeGenError::new(CodeGenErrorKind::MissingFirst(label.clone())))
    }

    fn get_req_code_attrs(&self, label: &Ident) -> Option<&ReqCodeAttrs> {
    	if let Some((attrs, _, _)) = self.req_attribute_map.get(label) {
    		Some(attrs)
    	} else {
    		None
    	}
    }

    pub(crate) fn get_req_weight_attrs(&self, label: &Ident) -> Option<&ReqWeightAttrs> {
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

    pub(crate) fn collect_attrs(&self, label: &Ident, a: Option<&AttrSet>) -> Vec<&TokenStream> {
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

    pub(crate) fn add_attribute_mapping(&mut self, label: Rc<Ident>, ident: &SpannableIdent, code: TokenStream) {
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

    pub(crate) fn add_ret_attr(&mut self, label: Rc<Ident>, attr: String) {
    	if let Some((ret, _)) = self.attr_repr.get_mut(&label) {
    		ret.push(attr);
    	} else {
    		self.attr_repr.insert(label, (vec![attr], Vec::new()));
    	}
    }

    pub(crate) fn add_ctx_attr(&mut self, label: Rc<Ident>, attr: String) {
    	if let Some((_, ctx)) = self.attr_repr.get_mut(&label) {
    		ctx.push(attr);
    	} else {
    		self.attr_repr.insert(label, (Vec::new(), vec![attr]));
    	}
    }

    /// At the end of this method, `self.files` is empty again and the resulting [`FileStructure`] should be used instead.
    pub(crate) fn gen_struct_stream(&mut self) -> CodeGenResult<(FileStructure, TokenStream)> {
    	let mut main_stream = TokenStream::new();
    	let mut fs = std::mem::take(&mut self.files);
    	for (id, firsts) in &self.first_queue {
    		self.handle_ident(&mut fs, &mut main_stream, id, firsts)?;
    	}
    	Ok((fs, main_stream))
    }

    pub(crate) fn gen_nt_stream(&self, fs: &mut FileStructure) -> CodeGenResult<()> {
    	let nonterminals = self.alt_map.keys();
    	let mut stream = quote!(
    		#![allow(non_snake_case)]
			#![allow(unused_parens)]
    	);
    	for nt in nonterminals {
    		let path = format!("nonterminals/{nt}.rs");
    		stream.extend(quote!(
    			#[path = #path]
    			pub(crate) mod #nt;
    		));
    	}
    	fs.insert_tokenstream("nonterminals.rs", stream, true)?;
    	Ok(())
    }

    fn handle_ident(&self, files: &mut FileStructure, main_stream: &mut TokenStream, id: &Rc<Ident>, firsts: &Vec<FirstSet>) -> CodeGenResult<()> {
    	let mut stream = TokenStream::new();
		let mut has_eps = false;
		let mut first_stream = Vec::new();
		let empty_repr = &EMPTY_REPR;
		for (alt, fin) in firsts {
			let (eps, stream) = Self::handle_first(alt, fin);
			has_eps = eps;
			first_stream.push(stream);
		}
		let re = Regex::new(DELIM_REGEX).map_err(|_| CodeGenError::new(CodeGenErrorKind::Fatal("Unable to construct identifier regex".to_string())))?;
		let first_attr_stream = self.collect_attrs(id, self.get_req_first_attrs(id));
		let code_attr_stream = self.collect_attrs(id, self.get_req_code_attrs(id));
		let uuid = id.to_string();
		let delim = re.find(&uuid).map_or(uuid.len(), |x| x.start());
		let root_uuid = &uuid[0..delim];
		let str_list = self.str_repr.get(id).ok_or_else(|| CodeGenError::new(CodeGenErrorKind::Fatal(format!("Missing str_repr for {id}"))))?;
		let str_repr = str_list.join(" ");
		let (pop_repr, ctx_repr) = self.attr_repr.get(id).unwrap_or(empty_repr);
		let code = self.code.get(id).ok_or_else(|| CodeGenError::new(CodeGenErrorKind::Fatal(format!("Missing code for {id}"))))?;
		#[allow(clippy::option_if_let_else)]
		let weight_stream = if let Some(weight) = self.weight_code.get(id) {
			quote!(
				#(#weight)*
			)
		} else {
			quote!(unreachable!("Weight should never be evaluated for non-zero GLL blocks"))
		};
		let filename = if root_uuid == uuid {
			if let Some(alts) = self.alt_map.get(id) {
				for alt in alts {
					let path = format!("{root_uuid}/{alt}.rs");
					stream.extend(quote!(
						#[path = #path]
						pub(crate) mod #alt;
					));
				}
			}
			format!("nonterminals/{root_uuid}.rs")
		} else {
			format!("nonterminals/{root_uuid}/{uuid}.rs")
		};
		stream.extend(quote!(
			#[allow(non_camel_case_types)]
			#[derive(Debug)]
			pub(crate) struct #id;

			impl<'a> wagon_gll::Label<'a> for #id {
				#[allow(unused_variables)]
				fn first_set(&self, state: &wagon_gll::GLLState<'a>) -> wagon_gll::ParseResult<'a, Vec<(Vec<wagon_gll::GLLBlockLabel<'a>>, Option<wagon_gll::Terminal<'a>>)>> {
					#(#first_attr_stream)*
					Ok(vec![#(#first_stream,)*])
				}
				fn is_eps(&self) -> bool {
					#has_eps
				}
				fn uuid(&self) -> &str {
					#uuid
				}
				#[allow(unused_variables)]
				fn code(&self, state: &mut wagon_gll::GLLState<'a>) -> wagon_gll::ParseResult<'a, ()> {
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
				fn _weight(&self, state: &wagon_gll::GLLState<'a>) -> Option<wagon_gll::ParseResult<'a, wagon_gll::value::Value<'a>>> {
					#weight_stream
				}
			}
		));
		if Some(id) == self.top.as_ref() {
			main_stream.extend(Self::construct_root_stream(&uuid));
		}
		files.insert_tokenstream(&filename, stream, true)?;
		Ok(())
    }

    fn handle_first(alt: &Vec<TokenStream>, fin: &Option<CharBytes>) -> (bool, TokenStream) {
    	let mut ret = false;
    	let byte = match fin {
	        Some(CharBytes::Bytes(b)) => quote!(Some(#b)),
	        Some(CharBytes::Epsilon) => {ret = true; quote!(Some(&[]))},
	        None => quote!(None),
	    };
	    let stream = quote!(
	    	(vec![#(#alt),*], #byte)
	    );
	    (ret, stream)
    }

    fn construct_root_stream(uuid: &str) -> TokenStream {
    	quote!(
			#[derive(Debug)]
    		struct _S;

    		impl<'a> wagon_gll::Label<'a> for _S {
    			fn first_set(&self, state: &wagon_gll::GLLState<'a>) -> wagon_gll::ParseResult<'a, Vec<(Vec<wagon_gll::GLLBlockLabel<'a>>, Option<wagon_gll::Terminal<'a>>)>> {
					Ok(vec![(vec![state.get_label_by_uuid(#uuid)?], None)])
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
				fn code(&self, _: &mut wagon_gll::GLLState<'a>) -> wagon_gll::ParseResult<'a, ()> {
					unreachable!("This should never be called");
				}
				fn attr_rep_map(&self) -> (Vec<&str>, Vec<&str>) { 
					(Vec::new(), Vec::new())
				}
				fn _weight(&self, _state: &wagon_gll::GLLState<'a>) -> Option<wagon_gll::ParseResult<'a, wagon_gll::value::Value<'a>>> {
					unreachable!("This should never be called");
				}
    		}
    	)
    }

    pub(crate) fn gen_state_stream(&self, files: &mut FileStructure) -> CodeGenResult<TokenStream> {
    	let mut stream = TokenStream::new();
    	let re = Regex::new(DELIM_REGEX).map_err(|_| CodeGenError::new(CodeGenErrorKind::Fatal("Unable to construct identifier regex".to_string())))?;
    	for (i, (id, alts)) in self.first_idents.iter().enumerate() {
    		let str_repr = id.to_string();
    		let delim = re.find(&str_repr).map_or(str_repr.len(), |x| x.start());
			let root_uuid = &str_repr[0..delim];
    		let root_path = Ident::new(root_uuid, proc_macro2::Span::call_site());
    		let label_id = format_ident!("label_{}", i);
    		if self.roots.contains(id) {
    			stream.extend(quote!(
	    			let #label_id = std::rc::Rc::new(nonterminals::#root_path::#id{});
	    			label_map.insert(#str_repr, #label_id);
	    		));
    			for (j, alt) in alts.iter().enumerate() {
    				let rule_id = format!("{id}_{j}");
    				let rule_var = format_ident!("alt_{}", rule_id);
    				stream.extend(quote!(
    					let #rule_var = std::rc::Rc::new(vec![#(#alt,)*]);
    					rule_map.insert(#rule_id, #rule_var);
    				));
    			}
    		} else {
    			stream.extend(quote!(
	    			let #label_id = std::rc::Rc::new(nonterminals::#root_path::#id::#id{});
	    			label_map.insert(#str_repr, #label_id);
	    		));
    		}
    		if Some(id) == self.top.as_ref() {
    			stream.extend(quote!(
    				label_map.insert(wagon_gll::ROOT_UUID, std::rc::Rc::new(_S{}));
    				rule_map.insert(wagon_gll::ROOT_UUID, std::rc::Rc::new(vec![wagon_ident::Ident::Unknown(#str_repr.to_string())]));
    			));
    		}
    	}
    	#[allow(clippy::expect_used)]
    	let regex_dir = files.insert_dir("regexes").expect("Unable to add regexes dir, should be impossible");
    	for (i, (r, dfa)) in self.regexes.iter().enumerate() {
    		let mut hasher = DefaultHasher::new();
			r.hash(&mut hasher);
			let basename = hasher.finish();
			let little_name = format!("{basename:x}_little.dfa");
			let little_path = std::path::Path::new("regexes").join(&little_name);
			let little_string = little_path.to_str().ok_or_else(|| CodeGenError::new(CodeGenErrorKind::Fatal("Non utf-8 path".to_string())))?;
			let (bytes, pad) = dfa.to_bytes_little_endian();
			regex_dir.insert_blob(&little_name, bytes[pad..].into());
			let big_name = format!("{basename:x}_big.dfa");
			let big_path = std::path::Path::new("regexes").join(&big_name);
			let big_string = big_path.to_str().ok_or_else(|| CodeGenError::new(CodeGenErrorKind::Fatal("Non utf-8 path".to_string())))?;
			let (bytes, pad) = dfa.to_bytes_big_endian();
			regex_dir.insert_blob(&big_name, bytes[pad..].into());
			let aligned_ident = format_ident!("ALIGNED_{}", i);
    		stream.extend(quote!(
    			static #aligned_ident: &regex_automata::util::wire::AlignAs<[u8], u32> = &regex_automata::util::wire::AlignAs {
    				_align: [],
    				#[cfg(target_endian = "big")]
			        bytes: *include_bytes!(#big_string),
			        #[cfg(target_endian = "little")]
			        bytes: *include_bytes!(#little_string),
    			};
    			let (dfa, _) = regex_automata::dfa::dense::DFA::from_bytes(&#aligned_ident.bytes).expect("Unable to serialize regex DFA");
    			let automata = wagon_gll::RegexTerminal::new(#r, dfa);
    			let pointer = std::rc::Rc::new(automata);
    			label_map.insert(#r, pointer.clone());
    			regex_map.insert(#r, pointer);
    		));
    	}
    	let regex_len = self.regexes.len();
    	let label_len = self.first_queue.len();
    	let root_len = self.roots.len();
    	Ok(quote!(
    		#[allow(non_snake_case)]
    		fn main() {
    			let args = clap::command!()
			        .arg(
			            clap::arg!(<filename> "Input file to parse")
			                .value_parser(clap::value_parser!(std::path::PathBuf))
			        )
			        .arg(
			            clap::arg!(--"no-crop" "Don't crop resulting sppf")
			                .num_args(0)
			        )
			        .get_matches();
			    let input_file = args.get_one::<std::path::PathBuf>("filename").expect("Input file required");
			    let crop = args.get_one::<bool>("no-crop").unwrap_or(&false) == &false;
			    let content_string = std::fs::read_to_string(input_file).expect("Couldn't read file");
			    let contents: &'static [u8] = Box::leak(content_string.trim().as_bytes().into()); // This is required to tell Rust the input data lasts forever.
    			let mut label_map: wagon_gll::LabelMap = std::collections::HashMap::with_capacity(#label_len);
    			let mut rule_map: wagon_gll::RuleMap = std::collections::HashMap::with_capacity(#root_len);
    			let mut regex_map: wagon_gll::RegexMap = std::collections::HashMap::with_capacity(#regex_len);
    			#stream
    			let mut state = wagon_gll::GLLState::init(contents, label_map, rule_map, regex_map).unwrap();
    			state.main();
    			if state.errors.len() > 0 {
			        for error in state.errors {
			            println!("Error: {error}");
			        }
			        std::process::exit(1);
			    }
    			match state.print_sppf_dot(crop) {
			        Ok(t) => println!("{t}"),
			        Err(e) => println!("Error: {e}")
			    }
    			assert!(state.accepts());
    		}
    	))
    }
}