use wagon_utils::Spannable;
use quote::quote;

use indexmap::IndexSet;
use proc_macro2::{Ident, Span, TokenStream};

use wagon_parser::parser::rule::Rule;
use wagon_parser::SpannableNode;

use crate::{CodeGenArgs, CodeGen, CodeGenResult, CodeGenError, CodeGenErrorKind, CharBytes};
use std::rc::Rc;
use wagon_codegen::ToTokensState;
use wagon_parser::parser::chunk::Chunk;
use wagon_parser::parser::symbol::Symbol;
use crate::state::CodeGenState;
use wagon_parser::parser::terminal::Terminal::LitString;


impl CodeGen for SpannableNode<Rule> {
    // All code constructed here goes into the root Label for this rule (so not any of the GLL Blocks).
    fn gen(self, gen_args: &mut CodeGenArgs) -> CodeGenResult<()> {
        let span = self.span();
        match self.into_inner() {
            Rule::Analytic(ident, args, rhs) => {
                // Construct an Rc to the identifier for this rule.
                let pointer: Rc<Ident> = Rc::new(Ident::new(&ident, Span::call_site()));
                if gen_args.fst.is_some_and(|x| x) { // If this is the first rule
                    gen_args.state.top = Some(pointer.clone()); // Tell the state.
                }
                // Set up all the maps for this rule already.
                let alt_count = rhs.len();
                gen_args.state.first_queue.insert(pointer.clone(), Vec::with_capacity(alt_count));
                gen_args.state.first_idents.insert(pointer.clone(), Vec::with_capacity(alt_count));
                gen_args.state.str_repr.insert(pointer.clone(), vec![ident]);
                gen_args.state.add_code(pointer.clone(), quote!(
                    let mut candidates = Vec::with_capacity(#alt_count);
                    let alt_count = #alt_count;
                    let mut zero_weights = 0;
                ));
                let as_set = IndexSet::from_iter(args);
                gen_args.full_args = Some(as_set);
                gen_args.ident = Some(pointer.clone());
            	for (i, alt) in rhs.into_iter().enumerate() {
                    gen_args.alt = Some(i);
            		alt.gen(gen_args)?;
            	}
                let stream = if gen_args.weight_config.no_prune { // If we don't want to prune any options, just add all slots as a valid candidate.
                    quote!(
                        for (_, slot) in candidates {
                            state.add(slot, state.gss_pointer, state.input_pointer, state.sppf_root)
                        }
                    )
                } else {
                    let comp_stream = quote!(
                        |(x, _), (y, _)| x.partial_cmp(y).map_or_else(|| Err(wagon_gll::GLLImplementationError::ValueError(wagon_gll::value::ValueError::ValueError(wagon_value::ValueError::ComparisonError(x.to_owned(), y.to_owned())))), Ok)
                    );
                    let to_add = if gen_args.weight_config.min_weight { // Either take options with the minimum or with the maximum weight
                        quote!(wagon_utils::FallibleItertools::fallible_min_set_by(candidates.into_iter(), #comp_stream))
                    } else {
                        quote!(wagon_utils::FallibleItertools::fallible_max_set_by(candidates.into_iter(), #comp_stream))
                    };
                    // Call `state.add` for all valid candidates.
                    quote!(
                        if !candidates.is_empty() {
                            let to_add = #to_add?;
                            for (_, slot) in to_add {
                                state.add(slot, state.gss_pointer, state.input_pointer, state.sppf_root, state.gss_pointer);
                            }
                        } else if zero_weights == alt_count {
                            return Err(wagon_gll::GLLError::ParseError(wagon_gll::GLLParseError::ZeroWeights{pointer: state.input_pointer, rule: self.to_string().to_owned(), context: state.get_current_gss_node()?.get_slot().to_string(state)}))
                        } else {
                            return Err(wagon_gll::GLLError::ParseError(wagon_gll::GLLParseError::NoCandidates{pointer: state.input_pointer, rule: self.to_string().to_owned(), context: state.get_current_gss_node()?.get_slot().to_string(state)}))
                        }
                    )
                };
                gen_args.state.add_code(pointer.clone(), stream);
                gen_args.state.add_code(pointer.clone(), quote!(Ok(())));
                gen_args.state.roots.insert(pointer);
                Ok(())
            },
            Rule::Generate(ident, _, rhs) => {
                let pointer: Rc<Ident> = Rc::new(Ident::new(&ident, Span::call_site()));

                gen_args.state.first_queue.insert(pointer.clone(), vec![(Vec::with_capacity(1), Some(CharBytes::Epsilon))]);
                gen_args.state.first_idents.insert(pointer.clone(), Vec::with_capacity(rhs.len()));
                gen_args.state.str_repr.insert(pointer.clone(), vec![ident]);
                gen_args.ident = Some(pointer.clone());
                gen_args.state.alt_map.insert(pointer.clone(), vec![]);

                // get the value to be returned from the generative rule
                let mut stream = quote!(
                    use rand::prelude::*;
                    let mut rng = rand::thread_rng();
                    let random_no: f32 = rng.gen(); // generates a float between 0 and 1
                    let mut lower_limit = 0.0;
                    let mut upper_limit;
                );

                let mut weight_counter = 0;
                let mut weight_decide_stream = quote!();
                let amount_rhs = rhs.len() as f32;
                let mut right_part = quote!(wagon_value::Value::from(0i32));

                for (_, alt) in rhs.into_iter().enumerate() {
                    let mut node = alt.into_inner();
                    let mut string_val: String = String::from("");

                    for chunk in node.chunks {
                        let span = chunk.span();
                        let symbols = match chunk.into_inner() {
                            Chunk { ebnf: Some(_), .. } => return Err(CodeGenError::new_spanned(CodeGenErrorKind::Fatal("Encountered an EBNF-chunk when calculating GLL-blocks. Should have been factored out".to_string()), span)),
                            c => c.extract_symbols(), // Deal with groups
                        };
                        for symbol in symbols {
                            let is_terminal = symbol.to_inner().is_terminal();
                            if is_terminal {
                                string_val = match symbol.to_inner() {
                                    Symbol::Terminal(term) => {
                                        match term.clone().into_inner() {
                                            LitString(val) => val,
                                            _ => "".parse().unwrap()
                                        }
                                    },
                                    _ => "".parse().unwrap()
                                }
                            }
                        }
                    }
                    let weight = std::mem::take(&mut node.weight);
                    let name_weight = format!("{}{}", "weight_", weight_counter);
                    let fun_call: TokenStream = name_weight.parse().unwrap();
                    right_part = quote!(
                        wagon_gll::value::Value::from(
        	                std::ops::Add::add(
        		                #fun_call(state)?,
        		                #right_part
        	                )?
                        )
                    );

                    let weight_stream = if let Some(ref expr) = weight { // Construct code for the weight if needed.
                        let label_str= "test";
                        let label = Rc::new(Ident::new(&label_str, Span::call_site()));

                        let stream_weight_expr = expr.to_tokens(&mut gen_args.state, label.clone(), CodeGenState::add_req_weight_attr);
                        let weight_attrs = gen_args.state.collect_attrs(&label, gen_args.state.get_req_weight_attrs(&label));

                        quote!(
                            fn #fun_call<'a>(state: &wagon_gll::GLLState<'a>) -> wagon_gll::ImplementationResult<'a, wagon_gll::value::Value<'a>> {
                                #(#weight_attrs)*
                                Ok(wagon_gll::value::Value::from(#stream_weight_expr))
                            };
                        )
                    } else {
                        let val: f32 = 1.0;
                        quote!(
                            fn #fun_call<'a>(state: &wagon_gll::GLLState<'a>) -> wagon_gll::ImplementationResult<'a, wagon_gll::value::Value<'a>> {
                                Ok(wagon_gll::value::Value::from(wagon_value::Value::try_from(#val)?))
                            };
                        )
                    };
                    stream.extend(weight_stream);

                    weight_decide_stream.extend(quote!(
                        upper_limit = f32::from(wagon_gll::value::Value::from(std::ops::Div::div(#fun_call(state)? ,total_weight.clone())?));
                        if random_no > lower_limit && random_no <= upper_limit + lower_limit{
	                        println!(#string_val);
                        }
                        lower_limit = upper_limit;
                    ));
                    weight_counter = weight_counter + 1;
                }
                stream.extend(quote!(let total_weight =));
                stream.extend(right_part);
                stream.extend(quote!(;));
                stream.extend(weight_decide_stream);
                println!("{}", stream);

                gen_args.state.add_code(pointer.clone(), stream);
                gen_args.state.add_code(pointer.clone(), quote!(
                    let mut buffer = String::new();
                    io::stdin().read_line(&mut buffer).unwrap();
                    state.add_input(buffer);));
                gen_args.state.add_code(pointer.clone(), quote!(Ok(state.pop(&vec![])?)));
                gen_args.state.roots.insert(pointer);
                Ok(())
            },
            Rule::Import(..) | Rule::Exclude(..) => Err(CodeGenError::new_spanned(CodeGenErrorKind::Fatal("Encountered import rule during codegen. These should have been converted away.".to_string()), span))
        }  
    }
}