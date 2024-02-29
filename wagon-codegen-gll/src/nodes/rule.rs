use quote::quote;
use std::todo;

use indexmap::IndexSet;
use proc_macro2::{Ident, Span};

use wagon_parser::parser::rule::Rule;
use wagon_parser::{SpannableNode, Spannable};

use crate::{CodeGenArgs, CodeGen, CodeGenResult, CodeGenError, CodeGenErrorKind};
use std::rc::Rc;


impl CodeGen for SpannableNode<Rule> {
   fn gen(self, gen_args: &mut CodeGenArgs) -> CodeGenResult<()> {
        let span = self.span();
        match self.into_inner() {
            Rule::Analytic(ident, args, rhs) => {
                let pointer: Rc<Ident> = Rc::new(Ident::new(&ident, Span::call_site()));
                if gen_args.fst.is_some_and(|x| x) {
                    gen_args.state.top = Some(pointer.clone());
                }
                gen_args.state.first_queue.insert(pointer.clone(), Vec::with_capacity(rhs.len()));
                gen_args.state.first_idents.insert(pointer.clone(), Vec::with_capacity(rhs.len()));
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
            		alt.gen(gen_args)?;
            	}
                let stream = if gen_args.weight_config.no_prune {
                    quote!(
                        for (_, slot) in candidates {
                            state.add(slot, state.gss_pointer, state.input_pointer, state.sppf_root)
                        }
                    )
                } else {
                    let comp_stream = quote!(
                        |(x, _), (y, _)| x.partial_cmp(y).map_or_else(|| Err(wagon_gll::GLLParseError::ValueError(wagon_gll::value::ValueError::ValueError(wagon_value::ValueError::ComparisonError(x.to_owned(), y.to_owned())))), Ok)
                    );
                    let to_add = if gen_args.weight_config.min_weight {
                        quote!(wagon_utils::FallibleItertools::fallible_min_set_by(candidates.into_iter(), #comp_stream))
                    } else {
                        quote!(wagon_utils::FallibleItertools::fallible_max_set_by(candidates.into_iter(), #comp_stream))
                    };
                    quote!(
                        if !candidates.is_empty() {
                            let to_add = #to_add?;
                            for (_, slot) in to_add {
                                state.add(slot, state.gss_pointer, state.input_pointer, state.sppf_root, state.gss_pointer);
                            }
                        }
                    )
                };
                gen_args.state.add_code(pointer.clone(), stream);
                gen_args.state.add_code(pointer.clone(), quote!(Ok(())));
                gen_args.state.roots.insert(pointer);
                Ok(())
            },
            Rule::Generate(_, _, _) => todo!(),
            Rule::Import(..) | Rule::Exclude(..) => Err(CodeGenError::new_spanned(CodeGenErrorKind::Fatal("Encountered import rule during codegen. These should have been converted away.".to_string()), span))
        }  
    }
}