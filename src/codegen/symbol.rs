
use quote::quote;

use crate::{parser::symbol::Symbol};
use crate::parser::terminal::Terminal;
use proc_macro2::{Ident};
use super::{CodeGenState, Rc, CharByte};

impl Symbol {
	pub(crate) fn gen(self, state: &mut CodeGenState, ident: Rc<Ident>, alt: usize, block: usize, symbol: usize, found_first: bool, label: Rc<Ident>, block_size: usize) -> bool {
		let first_symbol = symbol == 0;
		let uuid: String = ident.to_string();
		let rule_uuid = format!("{}_{}", uuid, alt);
		match self {
			Symbol::NonTerminal(i) => {
				let next_block = block + 1;
				let base = quote!(
					state.gss_pointer = state.create(
						std::rc::Rc::new(wagon_gll::GrammarSlot::new(
							state.get_label_by_uuid(#uuid), 
							state.get_rule(#rule_uuid),
							#next_block, 
							#rule_uuid
						))
					);
					label.code(state);
				);
				state.add_code(label.clone(), quote!(let label = state.get_label(&#i);));
				if !first_symbol {
					state.add_code(label.clone(), quote!(
						if state.test_next(label.clone()) {
							#base
						}
					));
				} else {
					state.add_code(label.clone(), base);
				}
				if !found_first {
					state.first_queue.get_mut(&label).unwrap()[0].0.push(i);
				}
				found_first
			},
			Symbol::Assignment(_) => todo!(),
			Symbol::Terminal(t) => {
				match t {
					Terminal::Regex(r) => {
						unimplemented!("Still determining what to do with regexes");
					},
					Terminal::LitString(s) => {
						if !found_first {
							if let Some(byte) = s.bytes().next() {
								state.first_queue.get_mut(&label).unwrap()[0].1 = Some(CharByte::Byte(byte));
							} else {
								state.first_queue.get_mut(&label).unwrap()[0].1 = Some(CharByte::Epsilon);
							}
						}
						state.add_code(label.clone(), quote!(
							let bytes = #s.as_bytes();
						));
						if first_symbol && block_size != 1 {
							state.add_code(label.clone(), quote!(
								let new_node = state.get_node_t(bytes);
								state.sppf_pointer = new_node;
								state.next(bytes).unwrap();
							));
						}
						let base = quote!(
							let node = state.get_node_t(bytes);
							state.next(&bytes);
							let slot = wagon_gll::GrammarSlot::new(
								state.get_label_by_uuid(#uuid), 
								state.get_rule(#rule_uuid),
								#block, 
								#rule_uuid
							);
							state.sppf_pointer = state.get_node_p(std::rc::Rc::new(slot), state.sppf_pointer, node);
						);
						if !first_symbol {
							state.add_code(label, quote!(
								if state.has_next(bytes) {
									#base
								}
							));
						} else if block_size == 1 {
							state.add_code(label, base);
						}
					},
				};
				true
			},
			Symbol::Epsilon => {
				state.first_queue.get_mut(&label).unwrap()[0].1 = Some(CharByte::Epsilon);
				true
			},
		}
	}
}