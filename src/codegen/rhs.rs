
use proc_macro2::{Ident, Span};
use quote::quote;

use crate::parser::rhs::Rhs;

use super::{CodeGenState, Rc};


impl Rhs {
    pub(crate) fn gen(self, state: &mut CodeGenState, ident: Rc<Ident>, alt: usize) {
        state.first_queue.get_mut(&ident).unwrap().push((Vec::with_capacity(self.chunks.len()), None));
        let mut found_first = false;
        let mut k = 0;
        for (j, block) in self.blocks().into_iter().enumerate() {
            let label = Rc::new(Ident::new(&format!("{}_{}_{}", &ident, alt, j), Span::call_site()));
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
		}
    }
}