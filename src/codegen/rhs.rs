
use indexmap::IndexSet;
use proc_macro2::{Ident, Span};
use quote::quote;

use crate::parser::rhs::Rhs;

use super::{CodeGenState, Rc, SpannableIdent, ToTokensState};


impl Rhs {
    pub(crate) fn gen(mut self, state: &mut CodeGenState, ident: Rc<Ident>, alt: usize, args: &mut IndexSet<SpannableIdent>) {
        let mut firsts = Vec::with_capacity(self.chunks.len());
        let weight = std::mem::take(&mut self.weight);
        let blocks = self.blocks();
        let blocks_count = blocks.len();
        let mut found_first = false;
        let mut prev_args = Vec::new();
        for (j, block) in blocks.into_iter().enumerate() {
            let label_str = format!("{}_{}_{}", &ident, alt, j);
            let label = Rc::new(Ident::new(&label_str, Span::call_site()));
            state.first_queue.insert(label.clone(), vec![(Vec::with_capacity(block.len()), None)]);
            let block_size = block.len();
            let mut str_repr = Vec::with_capacity(block_size);
            if block_size == 0 {
                state.first_queue.get_mut(&label).unwrap()[0].1 = Some(super::CharBytes::Epsilon);
            } 
            let mut counter: usize = 0;
            for (k, arg) in args.iter().enumerate() {
                let proc_ident = arg.to_inner().to_ident();
                if j == 0 { // We have no context, only parameters
                    state.add_attribute_mapping(label.clone(), arg, quote!( 
                        let #proc_ident = state.get_attribute(#k).to_owned();
                    ));
                    state.add_ret_attr(label.clone(), arg.to_string());
                } else {
                    let skipped_k = k + prev_args.len(); // The first n arguments on the stack were call parameters. The next m are our context
                    if prev_args.contains(arg) {
                        state.add_attribute_mapping(label.clone(), arg, quote!(
                            let #proc_ident = if let Some(v) = state.get_ret_val(#counter) {
                                v
                            } else {
                                state.restore_attribute(#skipped_k)
                            }.to_owned();
                        ));
                        state.add_ret_attr(label.clone(), arg.to_string());
                        state.add_ctx_attr(label.clone(), arg.to_string());
                        counter += 1;
                    } else {
                        state.add_attribute_mapping(label.clone(), arg, quote!(
                            let #proc_ident = state.restore_attribute(#skipped_k).to_owned();
                        ));
                        state.add_ctx_attr(label.clone(), arg.to_string());
                    }
                }
            }
        	for (k, symbol) in block.into_iter().enumerate() {
                if !symbol.is_assignment() {
                    str_repr.push(symbol.to_string());
                }
        		(found_first, prev_args) = symbol.gen(state, ident.clone(), alt, j, k, label.clone(), block_size, found_first, args);
        	}
            state.str_repr.insert(label.clone(), str_repr);
            if j == blocks_count - 1 {
                let mut ret_vals = Vec::new();
                for arg in args.iter() {
                    match arg.to_inner() {
                        wagon_gll::ident::Ident::Synth(_) => {
                            let arg_ident = arg.to_inner().to_ident();
                            ret_vals.push(quote!(Some(#arg_ident)));
                            state.add_req_code_attr(label.clone(), arg.clone());
                        },
                        _ => ret_vals.push(quote!(None))
                    }
                }
                state.add_code(label.clone(), quote!(
                    state.pop(vec![#(#ret_vals,)*]);
                ));
            }
            if j == 0 {
                let weight_stream = if let Some(ref expr) = weight {
                    expr.to_tokens(state, label.clone(), true)
                } else {
                    quote!(wagon_gll::value::Value::Natural(1))
                };
                state.add_weight_code(label.clone(), weight_stream);
                let root_str = ident.to_string();
                let rule_str = format!("{}_{}", &ident, alt);
                state.add_code(ident.clone(), quote!(
                    let fst = state.get_label_by_uuid(#label_str);
                    if state.test_next(fst) {
                        let root = state.get_label_by_uuid(#root_str);
                        let rules = state.get_rule(#rule_str);
                        let slot = wagon_gll::GrammarSlot::new(root, rules, 0, 0, #label_str);
                        candidates.push(std::rc::Rc::new(slot));
                    }
                ));
            }
            firsts.push(wagon_gll::ident::Ident::Unknown(label_str).into());
		}
        state.first_queue.get_mut(&ident).unwrap().push((firsts, None))
    }
}