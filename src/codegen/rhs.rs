
use proc_macro2::{Ident, Span};
use quote::quote;

use crate::parser::rhs::Rhs;

use super::{CodeGenState, Rc};


impl Rhs {
    pub(crate) fn gen(self, state: &mut CodeGenState, ident: Rc<Ident>, alt: usize) {
        let mut found_first = false;
        let mut k = 0;
        let mut firsts = Vec::with_capacity(self.chunks.len());
        for (j, block) in self.blocks().into_iter().enumerate() {
            let label_str = format!("{}_{}_{}", &ident, alt, j);
            let label = Rc::new(Ident::new(&label_str, Span::call_site()));
            state.first_queue.insert(label.clone(), vec![(Vec::with_capacity(block.len()), None)]);
            let block_size = block.len();
        	for symbol in block {
                k += 1;
        		found_first = symbol.gen(state, ident.clone(), alt, j, k, found_first, label.clone(), block_size);
        	}
            state.add_code(label.clone(), quote!(
                state.pop();
            ));
            if j == 0 {
                state.add_code(ident.clone(), quote!(
                    let fst = state.get_label(#label);
                    if state.test_next(fst) {
                        state.add(fst, state.sppf_root)
                    }
                ));
            }
            firsts.push(wagon_gll::ident::Ident::Unknown(label_str));
		}
        state.first_queue.get_mut(&ident).unwrap().push((firsts, None))
    }
}