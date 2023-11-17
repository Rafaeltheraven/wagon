use quote::quote;
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
                let alt_count = rhs.len();
                state.add_code(pointer.clone(), quote!(
                    let mut candidates = Vec::with_capacity(#alt_count);
                ));
                let mut as_set = IndexSet::from_iter(args);
            	for (i, alt) in rhs.into_iter().enumerate() {
            		alt.into_inner().gen(state, pointer.clone(), i, &mut as_set);
            	}
                state.add_code(pointer.clone(), quote!(
                    if !candidates.is_empty() {
                        if candidates[0].is_probabilistic(state) {
                            let mut rng = <rand::rngs::SmallRng as rand::SeedableRng>::from_entropy();
                            let weights = candidates.iter().map(|x| x.yank_probability(state));
                            let dist = rand::distributions::WeightedIndex::new(weights).unwrap();
                            state.add(candidates.swap_remove(rand::distributions::Distribution::sample(&dist, &mut rng)), state.gss_pointer, state.input_pointer, state.sppf_root);
                        } else {
                            let to_add = itertools::Itertools::max_set_by(candidates.into_iter(), |x, y| x.cmp(y, state));
                            for slot in to_add {
                                state.add(slot, state.gss_pointer, state.input_pointer, state.sppf_root);
                            }
                        }
                    }
                ));
                state.roots.insert(pointer);
            },
            Rule::Generate(_, _, _) => todo!(),
            Rule::Import(..) | Rule::Exclude(..) => panic!("{:?}", "Encountered import rule during codegen. These should have been converted away.")
        };    }
}