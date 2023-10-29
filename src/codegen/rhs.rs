
use proc_macro2::{Ident, Span};
use quote::quote;

use crate::parser::rhs::Rhs;

use super::{CodeGenState, Rc};


impl Rhs {
    pub(crate) fn gen(self, state: &mut CodeGenState, ident: Rc<Ident>, alt: usize) {
        let mut firsts = Vec::with_capacity(self.chunks.len());
        let blocks = self.blocks();
        let blocks_count = blocks.len();
        for (j, block) in blocks.into_iter().enumerate() {
            let label_str = format!("{}_{}_{}", &ident, alt, j);
            let label = Rc::new(Ident::new(&label_str, Span::call_site()));
            state.first_queue.insert(label.clone(), vec![(Vec::with_capacity(block.len()), None)]);
            let block_size = block.len();
            let mut str_repr = Vec::with_capacity(block_size);
            if block_size == 0 {
                state.first_queue.get_mut(&label).unwrap()[0].1 = Some(super::CharByte::Epsilon);
            }
        	for (k, symbol) in block.into_iter().enumerate().rev() {
                str_repr.push(symbol.to_string());
        		symbol.gen(state, ident.clone(), alt, j, k, label.clone(), block_size);
        	}
            str_repr.reverse();
            state.str_repr.insert(label.clone(), str_repr);
            if j == blocks_count - 1 {
                state.add_code(label.clone(), quote!(
                    state.pop();
                ));
            }
            if j == 0 {
                let root_str = ident.to_string();
                let rule_str = format!("{}_{}", &ident, alt);
                state.add_code(ident.clone(), quote!(
                    let fst = state.get_label_by_uuid(#label_str);
                    if state.test_next(fst) {
                        let root = state.get_label_by_uuid(#root_str);
                        let rules = state.get_rule(#rule_str);
                        let slot = wagon_gll::GrammarSlot::new(root, rules, 0, 0, #label_str);
                        state.add(std::rc::Rc::new(slot), state.gss_pointer, state.input_pointer, state.sppf_root)
                    }
                ));
            }
            firsts.push(wagon_gll::ident::Ident::Unknown(label_str));
		}
        state.first_queue.get_mut(&ident).unwrap().push((firsts, None))
    }
}