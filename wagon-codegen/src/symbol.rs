
use std::matches;

use quote::quote;

use wagon_parser::parser::{symbol::Symbol, terminal::Terminal};
use proc_macro2::{Literal, TokenStream};
use super::{CodeGenArgs, CodeGen, CharBytes};

impl CodeGen for Symbol {
	fn gen(self, gen_args: &mut CodeGenArgs) {
		let ident = gen_args.ident.as_ref().unwrap();
		let alt = gen_args.alt.as_ref().unwrap();
		let block = gen_args.block.unwrap();
		let symbol = gen_args.symbol.unwrap();
		let label = gen_args.label.as_ref().unwrap();
		let block_size = gen_args.block_size.unwrap();
		let found_first = gen_args.found_first.unwrap();
		let full_args = gen_args.full_args.as_ref().unwrap();
		let state = &mut gen_args.state;

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
					state.first_queue.get_mut(label).unwrap()[0].0.push(i.clone());
				}
				if !matches!(i.to_inner(), wagon_ident::Ident::Unknown(_)) {
					state.add_req_code_attr(label.clone(), i);
				}
				gen_args.prev_args = Some(args);
			},
			Symbol::Assignment(v) => {
				for ass in v {
					ass.into_inner().gen(gen_args);
				}
				gen_args.prev_args = Some(Vec::new());
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
							state.first_queue.get_mut(label).unwrap()[0].1 = Some(CharBytes::Bytes(bytes))
						}
						if first_symbol && block_size != 1 {
							stream.extend(quote!(
								let new_node = state.get_node_t(bytes);
								state.sppf_pointer = new_node;
								state.next(bytes).unwrap();
							));
							state.add_code(label.clone(), stream);
							return;
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
							state.add_code(label.clone(), stream);
						} else if block_size == 1 {
							stream.extend(base);
							state.add_code(label.clone(), stream);
						}
					},
				};
				gen_args.found_first = Some(true);
				gen_args.prev_args = Some(Vec::new());
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
				state.first_queue.get_mut(label).unwrap()[0].1 = Some(CharBytes::Epsilon);
				gen_args.found_first = Some(true);
				gen_args.prev_args = Some(Vec::new());
			},
		}
	}
}