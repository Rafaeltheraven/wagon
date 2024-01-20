
use proc_macro2::{Ident, Span};
use quote::quote;

use wagon_parser::parser::rhs::Rhs;
use wagon_parser::{SpannableNode, Spannable};

use wagon_codegen::ToTokensState;
use crate::{CodeGenState, CodeGenArgs, CodeGen, CharBytes, CodeGenResult, CodeGenError, CodeGenErrorKind};
use std::rc::Rc;


impl CodeGen for SpannableNode<Rhs> {
    fn gen(self, gen_args: &mut CodeGenArgs) -> CodeGenResult<()> {
        let span = self.span();
        let mut node = self.into_inner();
        let ident = gen_args.ident.as_ref().ok_or_else(|| CodeGenError::new_spanned(CodeGenErrorKind::MissingArg("ident".to_string()), span.clone()))?.clone();
        let alt = gen_args.alt.ok_or_else(|| CodeGenError::new_spanned(CodeGenErrorKind::MissingArg("alt".to_string()), span.clone()))?;
        let mut firsts = Vec::with_capacity(node.chunks.len());
        let weight = std::mem::take(&mut node.weight);
        let blocks = node.blocks()?;
        let blocks_count = blocks.len();
        gen_args.found_first = Some(false);
        gen_args.prev_args = Some(Vec::new());
        for (j, block) in blocks.into_iter().enumerate() {
            let args = gen_args.full_args.as_ref().ok_or_else(|| CodeGenError::new_spanned(CodeGenErrorKind::MissingArg("full_args".to_string()), span.clone()))?;
            let prev_args = gen_args.prev_args.as_ref().ok_or_else(|| CodeGenError::new_spanned(CodeGenErrorKind::MissingArg("prev_args".to_string()), span.clone()))?;

            let label_str = format!("{ident}_{alt}_{j}");
            let label = Rc::new(Ident::new(&label_str, Span::call_site()));
            gen_args.state.first_queue.insert(label.clone(), vec![(Vec::with_capacity(block.len()), None)]);
            let block_size = block.len();
            let mut str_repr = Vec::with_capacity(block_size);
            if block_size == 0 {
                gen_args.state.get_first(&label)?[0].1 = Some(CharBytes::Epsilon);
            } 
            let mut counter: usize = 0;
            for (k, arg) in args.iter().enumerate() {
                let proc_ident = arg.to_inner().to_ident();
                if j == 0 { // We have no context, only parameters
                    gen_args.state.add_attribute_mapping(label.clone(), arg, quote!( 
                        let #proc_ident = state.get_attribute(#k).to_owned();
                    ));
                    gen_args.state.add_ret_attr(label.clone(), arg.to_string());
                } else {
                    let skipped_k = k + prev_args.len(); // The first n arguments on the stack were call parameters. The next m are our context
                    if prev_args.contains(arg) {
                        gen_args.state.add_attribute_mapping(label.clone(), arg, quote!(
                            let #proc_ident = if let Some(v) = state.get_ret_val(#counter) {
                                v
                            } else {
                                state.restore_attribute(#skipped_k)
                            }.to_owned();
                        ));
                        gen_args.state.add_ret_attr(label.clone(), arg.to_string());
                        gen_args.state.add_ctx_attr(label.clone(), arg.to_string());
                        counter += 1;
                    } else {
                        gen_args.state.add_attribute_mapping(label.clone(), arg, quote!(
                            let #proc_ident = state.restore_attribute(#skipped_k).to_owned();
                        ));
                        gen_args.state.add_ctx_attr(label.clone(), arg.to_string());
                    }
                }
            }
            gen_args.block = Some(j);
            gen_args.label = Some(label.clone());
            gen_args.block_size = Some(block_size);
        	for (k, symbol) in block.into_iter().enumerate() {
                if !symbol.to_inner().is_assignment() {
                    str_repr.push(symbol.to_string());
                }
                gen_args.symbol = Some(k);
        		symbol.gen(gen_args)?;
        	}
            let args = gen_args.full_args.as_ref().ok_or_else(|| CodeGenError::new_spanned(CodeGenErrorKind::MissingArg("full_args".to_string()), span.clone()))?;
            gen_args.state.str_repr.insert(label.clone(), str_repr);
            if j == blocks_count - 1 {
                let mut ret_vals = Vec::new();
                for arg in args {
                    match arg.to_inner() {
                        wagon_ident::Ident::Synth(_) => {
                            let arg_ident = arg.to_inner().to_ident();
                            ret_vals.push(quote!(Some(#arg_ident)));
                            gen_args.state.add_req_code_attr(label.clone(), arg.clone());
                        },
                        _ => ret_vals.push(quote!(None))
                    }
                }
                gen_args.state.add_code(label.clone(), quote!(
                    state.pop(vec![#(#ret_vals,)*]);
                ));
            }
            if j == 0 {
                let weight_stream = if let Some(ref expr) = weight {
                    let stream = expr.to_tokens(&mut gen_args.state, label.clone(), CodeGenState::add_req_weight_attr);
                    quote!(Some(#stream))
                } else {
                    quote!(None)
                };
                gen_args.state.add_weight_code(label.clone(), weight_stream);
                let root_str = ident.to_string();
                let rule_str = format!("{}_{}", ident, gen_args.alt.ok_or_else(|| CodeGenError::new_spanned(CodeGenErrorKind::MissingArg("alt".to_string()), span.clone()))?);
                let inner_block = quote!(
                    let root = state.get_label_by_uuid(#root_str);
                    let rules = state.get_rule(#rule_str);
                    let slot = wagon_gll::GrammarSlot::new(root, rules, 0, 0, #label_str);
                    candidates.push(std::rc::Rc::new(slot));
                );
                let stream = if !gen_args.weight_config.no_first {
                    quote!(
                       let fst = state.get_label_by_uuid(#label_str);
                       if state.test_next(fst) {
                           #inner_block 
                       } 
                    )
                } else {
                    inner_block
                };
                gen_args.state.add_code(ident.clone(), stream);
            }
            firsts.push(wagon_ident::Ident::Unknown(label_str).into());
		}
        gen_args.state.get_first(&ident)?.push((firsts, None));
        Ok(())
    }
}