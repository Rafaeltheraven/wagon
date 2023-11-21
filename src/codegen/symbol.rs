
use std::matches;

use indexmap::IndexSet;
use quote::quote;

use crate::parser::{symbol::Symbol, terminal::Terminal};
use proc_macro2::{Ident, Literal, TokenStream};
use super::{CodeGenState, Rc, CharBytes, SpannableIdent};

impl Symbol {
	#[allow(clippy::too_many_arguments)]
	pub(crate) fn gen(self, state: &mut CodeGenState, ident: Rc<Ident>, alt: usize, block: usize, symbol: usize, label: Rc<Ident>, block_size: usize, found_first: bool, full_args: &mut IndexSet<SpannableIdent>) -> (bool, Vec<SpannableIdent>) {
		let first_symbol = block == 0 && symbol == 0;
		let uuid: String = ident.to_string();
		let rule_uuid = format!("{}_{}", uuid, alt);
		match self {
			Symbol::NonTerminal(i, args) => {
				let next_block = block + 1;
				let args_idents = args.iter().map(|x| x.to_inner().to_ident());
				let mut full_args_idents = Vec::with_capacity(full_args.len());
				for arg in full_args.iter() {
					state.add_req_code_attr(label.clone(), arg.clone());
					full_args_idents.push(arg.to_inner().to_ident());
				}
				let base = quote!(
					state.gss_pointer = state.create(
						std::rc::Rc::new(wagon_gll::GrammarSlot::new(
							state.get_label_by_uuid(#uuid), 
							state.get_rule(#rule_uuid),
							#next_block,
							0, 
							#rule_uuid
						)),
						vec![#(#args_idents.clone(),)*#(#full_args_idents,)*]
					);
					label.code(state);
				);
				if !first_symbol {
					state.add_code(label.clone(), quote!(
						let label = state.get_label(&#i);
						if state.test_next(label.clone()) {
							#base
						} else {
							return;
						}
					));
				} else {
					state.add_code(label.clone(), quote!(
						let label = state.get_label(&#i);
						#base
					));
				}
				if !found_first {
					state.add_req_first_attr(label.clone(), i.clone());
					state.first_queue.get_mut(&label).unwrap()[0].0.push(i.clone());
				}
				if !matches!(i.to_inner(), wagon_gll::ident::Ident::Unknown(_)) {
					state.add_req_code_attr(label.clone(), i);
				}
				(found_first, args)
			},
			Symbol::Assignment(v) => {
				for ass in v {
					ass.into_inner().gen(state, label.clone(), full_args);
				}
				(found_first, Vec::new())
			},
			Symbol::Terminal(t) => {
				match t.into_inner() {
					Terminal::Regex(_r) => {
						unimplemented!("Still determining what to do with regexes");
					},
					Terminal::LitString(s) => {
						let bytes = Literal::byte_string(s.as_bytes());
						let mut stream = TokenStream::new();
						stream.extend(quote!(
							let bytes = #bytes;
						));
						if !found_first {
							state.first_queue.get_mut(&label).unwrap()[0].1 = Some(CharBytes::Bytes(bytes))
						}
						if first_symbol && block_size != 1 {
							stream.extend(quote!(
								let new_node = state.get_node_t(bytes);
								state.sppf_pointer = new_node;
								state.next(bytes).unwrap();
							));
							state.add_code(label.clone(), stream);
							return (true, Vec::new());
						}
						let (dot, pos) = if symbol == block_size-1 {
							(block+1, 0)
						} else {
							(block, symbol+1)
						};
						let base = quote!(
							let node = state.get_node_t(bytes);
							state.next(bytes).unwrap();
							let slot = wagon_gll::GrammarSlot::new(
								state.get_label_by_uuid(#uuid), 
								state.get_rule(#rule_uuid),
								#dot, 
								#pos,
								#rule_uuid
							);
							state.sppf_pointer = state.get_node_p(std::rc::Rc::new(slot), state.sppf_pointer, node, state.gss_pointer);
						);
						if !first_symbol {
							stream.extend(quote!(
								if state.has_next(bytes) {
									#base
								} else {
									return;
								}
							));
							state.add_code(label, stream);
						} else if block_size == 1 {
							stream.extend(base);
							state.add_code(label, stream);
						}
					},
				};
				(true, Vec::new())
			},
			Symbol::Epsilon => {
				state.add_code(label.clone(), quote!(
					let cr = state.get_node_t(&[]);
					let slot = wagon_gll::GrammarSlot::new(
						state.get_label_by_uuid(#uuid),
						state.get_rule(#rule_uuid),
						1,
						0,
						#rule_uuid
					);
					state.sppf_pointer = state.get_node_p(std::rc::Rc::new(slot), state.sppf_pointer, cr, state.gss_pointer);
				));
				state.first_queue.get_mut(&label).unwrap()[0].1 = Some(CharBytes::Epsilon);
				(true, Vec::new())
			},
		}
	}
}