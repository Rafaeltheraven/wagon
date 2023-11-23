use quote::quote;
use std::todo;

use indexmap::IndexSet;
use proc_macro2::{Ident, Span};

use wagon_parser::parser::rule::Rule;

use super::{CodeGenArgs, CodeGen, Rc};


impl CodeGen for Rule {
   fn gen(self, gen_args: &mut CodeGenArgs) {
        match self {
            Rule::Analytic(ident, args, rhs) => {
                let pointer: Rc<Ident> = Rc::new(Ident::new(&ident, Span::call_site()));
                if gen_args.fst.is_some_and(|x| x) {
                    gen_args.state.top = Some(pointer.clone());
                }
                gen_args.state.first_queue.insert(pointer.clone(), Vec::with_capacity(rhs.len()));
                gen_args.state.str_repr.insert(pointer.clone(), vec![ident]);
                let alt_count = rhs.len();
                gen_args.state.add_code(pointer.clone(), quote!(
                    let mut candidates = Vec::with_capacity(#alt_count);
                ));
                let as_set = IndexSet::from_iter(args);
                gen_args.full_args = Some(as_set);
                gen_args.ident = Some(pointer.clone());
            	for (i, alt) in rhs.into_iter().enumerate() {
                    gen_args.alt = Some(i);
            		alt.into_inner().gen(gen_args);
            	}
                gen_args.state.add_code(pointer.clone(), quote!(
                    if let Some(check) = candidates.get(0) {
                        if check.is_probabilistic(state) {
                            let weights = candidates.iter().map(|x| x.yank_probability(state));
                            let dist = rand::distributions::WeightedIndex::new(weights).unwrap();
                            let index = rand::distributions::Distribution::sample(&dist, &mut state.rng);
                            state.add(candidates.swap_remove(index), state.gss_pointer, state.input_pointer, state.sppf_root);
                        } else {
                            let to_add = itertools::Itertools::max_set_by(candidates.into_iter(), |x, y| x.cmp(y, state));
                            for slot in to_add {
                                state.add(slot, state.gss_pointer, state.input_pointer, state.sppf_root);
                            }
                        }
                    }
                ));
                gen_args.state.roots.insert(pointer);
            },
            Rule::Generate(_, _, _) => todo!(),
            Rule::Import(..) | Rule::Exclude(..) => panic!("{:?}", "Encountered import rule during codegen. These should have been converted away.")
        };    }
}