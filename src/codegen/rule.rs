use std::todo;

use proc_macro2::{Ident, Span};

use crate::parser::rule::Rule;

use super::{CodeGenState, Rc};


impl Rule {
    pub(crate) fn gen(self, state: &mut CodeGenState) {
        match self {
            Rule::Analytic(ident, rhs) => {
                let pointer: Rc<Ident> = Rc::new(Ident::new(&ident, Span::call_site()));
                state.first_queue.insert(pointer.clone(), Vec::with_capacity(rhs.len()));
            	for (i, alt) in rhs.into_iter().enumerate() {
            		alt.gen(state, pointer.clone(), i);
            	}
            },
            Rule::Generate(_, _) => todo!(),
            Rule::Import(..) | Rule::Exclude(..) => panic!("{:?}", "Encountered import rule during codegen. These should have been converted away.")
        };    }
}