use std::todo;

use indexmap::IndexSet;
use proc_macro2::{Ident, Span};

use crate::parser::rule::Rule;

use super::{CodeGenState, Rc};


impl Rule {
    pub(crate) fn gen(self, state: &mut CodeGenState, fst: bool) {
        match self {
            Rule::Analytic(ident, args, rhs) => {
                let pointer: Rc<Ident> = Rc::new(Ident::new(&ident, Span::call_site()));
                if fst {
                    state.top = Some(pointer.clone());
                }
                state.first_queue.insert(pointer.clone(), Vec::with_capacity(rhs.len()));
                state.str_repr.insert(pointer.clone(), vec![ident]);
                let mut as_set = IndexSet::from_iter(args);
            	for (i, alt) in rhs.into_iter().enumerate() {
            		alt.gen(state, pointer.clone(), i, &mut as_set);
            	}
                state.roots.insert(pointer);
            },
            Rule::Generate(_, _, _) => todo!(),
            Rule::Import(..) | Rule::Exclude(..) => panic!("{:?}", "Encountered import rule during codegen. These should have been converted away.")
        };    }
}