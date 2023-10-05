
use crate::parser::rhs::Rhs;

use super::{CodeGenState, Rc};


impl Rhs {
    pub(crate) fn gen(self, state: &mut CodeGenState, ident: Rc<str>, alt: usize) {
        state.first_queue.get_mut(&ident).unwrap().push((Vec::with_capacity(self.chunks.len()), None));
        let mut found_first = false;
        for (j, block) in self.blocks().into_iter().enumerate() {
        	for (k, symbol) in block.into_iter().enumerate() {
        		found_first = symbol.gen(state, ident.clone(), alt, j, k, found_first);
        	}
		}
		todo!()
    }
}