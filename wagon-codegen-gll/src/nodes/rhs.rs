
use wagon_utils::Spannable;
use proc_macro2::{Ident, Span};
use quote::quote;

use wagon_parser::parser::chunk::Chunk;
use wagon_parser::parser::rhs::Rhs;
use wagon_parser::SpannableNode;

use wagon_codegen::ToTokensState;
use crate::{CharBytes, CodeGen, CodeGenArgs, CodeGenError, CodeGenErrorKind, CodeGenResult, CodeGenState};

use std::rc::Rc;

impl CodeGen for SpannableNode<Rhs> {
    #[allow(clippy::too_many_lines)] // I have no clue how to take this method apart
    fn gen(self, gen_args: &mut CodeGenArgs) -> CodeGenResult<()> {
        let span = self.span();
        let mut node = self.into_inner();
        // Get all the function arguments.
        let ident = gen_args.ident.as_ref().ok_or_else(|| CodeGenError::new_spanned(CodeGenErrorKind::MissingArg("ident".to_string()), span.clone()))?.clone();
        let alt = gen_args.alt.ok_or_else(|| CodeGenError::new_spanned(CodeGenErrorKind::MissingArg("alt".to_string()), span.clone()))?;
        let mut firsts = Vec::with_capacity(node.chunks.len());
        let mut first_idents = Vec::with_capacity(node.chunks.len());
        let weight = std::mem::take(&mut node.weight);
        let blocks = node.blocks()?;
        let blocks_count = blocks.len();
        // We have not yet completed the first_set for this alternative.
        gen_args.found_first = Some(false);
        gen_args.prev_args = Some(Vec::new());
        for (j, block) in blocks.into_iter().enumerate() {
            let args = gen_args.full_args.as_ref().ok_or_else(|| CodeGenError::new_spanned(CodeGenErrorKind::MissingArg("full_args".to_string()), span.clone()))?;
            let prev_args = gen_args.prev_args.as_ref().ok_or_else(|| CodeGenError::new_spanned(CodeGenErrorKind::MissingArg("prev_args".to_string()), span.clone()))?;
            // Construct a label for this alternative + block
            let label_str = format!("{ident}_{alt}_{j}");
            let label = Rc::new(Ident::new(&label_str, Span::call_site()));
            gen_args.state.first_queue.insert(label.clone(), vec![(Vec::with_capacity(block.len()), None)]);
            gen_args.state.first_idents.insert(label.clone(), vec![Vec::with_capacity(block.len())]);
            if let Some(alts) = gen_args.state.alt_map.get_mut(&ident) { // Add this alt to the list of alts for the root.
                alts.push(label.clone());
            } else {
                gen_args.state.alt_map.insert(ident.clone(), vec![label.clone()]);
            };
            let block_size = block.len();
            let mut str_repr = Vec::with_capacity(block_size);
            if block_size == 0 { // If we have 0 blocks, this is an empty alternative.
                gen_args.state.get_first(&label)?[0].1 = Some(CharBytes::Epsilon);
            } 
            let mut counter: usize = 0;
            for (k, arg) in args.iter().enumerate() {
                let proc_ident = arg.to_inner().to_ident();
                if j == 0 { // We have no context, only parameters
                    gen_args.state.add_attribute_mapping(label.clone(), arg, quote!( 
                        let #proc_ident = state.get_attribute(#k)?.to_owned();
                    ));
                    gen_args.state.add_ret_attr(label.clone(), arg.to_string());
                } else {
                    let skipped_k = k + prev_args.len(); // The first n arguments on the stack were call parameters. The next m are our context
                    if prev_args.contains(arg) { // If this attribute was used in whatever NT came before this one.
                        // The attribute could either come from the context, or be returned from whatever NT we just finished parsing.
                        gen_args.state.add_attribute_mapping(label.clone(), arg, quote!(
                            let #proc_ident = if let Some(v) = state.get_ret_val(#counter)? {
                                v
                            } else {
                                state.restore_attribute(#skipped_k)?
                            }.to_owned();
                        ));
                        gen_args.state.add_ret_attr(label.clone(), arg.to_string());
                        gen_args.state.add_ctx_attr(label.clone(), arg.to_string());
                        counter += 1;
                    } else {
                        // The attribute can only come from the context.
                        gen_args.state.add_attribute_mapping(label.clone(), arg, quote!(
                            let #proc_ident = state.restore_attribute(#skipped_k)?.clone();
                        ));
                        gen_args.state.add_ctx_attr(label.clone(), arg.to_string());
                    }
                }
            }
            gen_args.block = Some(j);
            gen_args.label = Some(label.clone());
            gen_args.block_size = Some(block_size);
            let mut correction = 0;
        	for (k, symbol) in block.into_iter().enumerate() {
                if !symbol.to_inner().is_assignment() {
                    str_repr.push(symbol.to_string());
                }
                gen_args.symbol = Some(k - correction);
                if matches!(symbol.to_inner(), wagon_parser::parser::symbol::Symbol::Assignment(_)) { // Assignments mess up our count, so we need to correct for them.
                    correction += 1;
                }
        		symbol.gen(gen_args)?;
        	}
            let args = gen_args.full_args.as_ref().ok_or_else(|| CodeGenError::new_spanned(CodeGenErrorKind::MissingArg("full_args".to_string()), span.clone()))?;
            gen_args.state.str_repr.insert(label.clone(), str_repr);
            if j == blocks_count - 1 { // If this is the last block.
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
                // Pop all the synthesized attributes back.
                gen_args.state.add_code(label.clone(), quote!(
                    Ok(state.pop(&vec![#(#ret_vals,)*])?)
                ));
            }
            if j == 0 { // If this is the first block
                let weight_stream = if let Some(ref expr) = weight { // Construct code for the weight if needed.
                    let stream = expr.to_tokens(&mut gen_args.state, label.clone(), CodeGenState::add_req_weight_attr);
                    let weight_attrs = gen_args.state.collect_attrs(&label, gen_args.state.get_req_weight_attrs(&label));
                    quote!(
                        fn actual_weight<'a>(state: &wagon_gll::GLLState<'a>) -> wagon_gll::ImplementationResult<'a, wagon_gll::value::Value<'a>> {
                            #(#weight_attrs)*
                            Ok(wagon_gll::value::Value::from(#stream))
                        }
                        Some(actual_weight(state))
                    )
                } else {
                    quote!(None)
                };
                gen_args.state.add_weight_code(label.clone(), weight_stream);
                let root_str = ident.to_string();
                let rule_str = format!("{}_{}", ident, gen_args.alt.ok_or_else(|| CodeGenError::new_spanned(CodeGenErrorKind::MissingArg("alt".to_string()), span.clone()))?);
                // Construct the slot.
                let mut inner_block = quote!(
                    let root = state.get_label_by_uuid(#root_str)?;
                    let rules = state.get_rule(#rule_str)?;
                    let slot = wagon_gll::GrammarSlot::new(root, rules, 0, 0, #label_str);
                    let weight = slot.weight(state)?;
                );
                if gen_args.weight_config.allow_zero {
                    inner_block.extend(quote!(candidates.push((weight, std::rc::Rc::new(slot)));));
                } else { // If we do not allow 0 weights, make sure only those that resolve to `true` are added to the candidates list.
                    inner_block.extend(quote!(
                        if wagon_value::Valueable::is_truthy(&weight)? {
                            candidates.push((weight, std::rc::Rc::new(slot)));
                        } else {
                            zero_weights = zero_weights + 1;
                        }
                    ));
                }
                let stream = if gen_args.weight_config.no_first { // If we do not care about the first_set, just add the slot.
                    inner_block
                } else { // If we do care, only add it after checking whether it accepts.
                    quote!(
                       let fst = state.get_label_by_uuid(#label_str)?;
                       if state.test_next(&fst)? {
                           #inner_block 
                       } 
                    )
                };
                gen_args.state.add_code(ident.clone(), stream);
            }
            firsts.push(quote!(state.get_label_by_uuid(#label_str)?));
            first_idents.push(wagon_ident::Ident::Unknown(label_str).into());
		}
        gen_args.state.get_first(&ident)?.push((firsts, None));
        gen_args.state.get_first_ident(&ident)?.push(first_idents);
        Ok(())
    }
}

/// GLL Blocks as defined by the OOGLL paper.
#[derive(Debug, PartialEq, Eq)]
struct GLLBlocks(Vec<GLLBlock>);

#[derive(Debug, PartialEq, Eq)]
struct GLLBlock(Vec<SpannableNode<wagon_parser::parser::symbol::Symbol>>);

impl GLLBlock {
    fn len(&self) -> usize {
        let mut sum = 0;
        for s in &self.0 {
            if !matches!(s.to_inner(), wagon_parser::parser::symbol::Symbol::Assignment(_)) { // Assignments do not count for the length of a GLL block.
                sum += 1;
            }
        }
        sum
    }
}

impl GLLBlocks {
    fn len(&self) -> usize {
        self.0.len()
    }
}

impl IntoIterator for GLLBlocks {
    type Item = GLLBlock;

    type IntoIter = <Vec<GLLBlock> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl IntoIterator for GLLBlock {
    type Item = SpannableNode<wagon_parser::parser::symbol::Symbol>;

    type IntoIter = <Vec<SpannableNode<wagon_parser::parser::symbol::Symbol>> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }

}

impl From<Vec<wagon_parser::SpannableNode<wagon_parser::parser::symbol::Symbol>>> for GLLBlock {
    fn from(value: Vec<wagon_parser::SpannableNode<wagon_parser::parser::symbol::Symbol>>) -> Self {
        Self(value)
    }
}

impl From<Vec<GLLBlock>> for GLLBlocks {
    fn from(value: Vec<GLLBlock>) -> Self {
        Self(value)
    }
}

impl From<Vec<Vec<wagon_parser::SpannableNode<wagon_parser::parser::symbol::Symbol>>>> for GLLBlocks {
    fn from(value: Vec<Vec<wagon_parser::SpannableNode<wagon_parser::parser::symbol::Symbol>>>) -> Self {
        let mut conv = Vec::with_capacity(value.len());
        for v in value {
            conv.push(v.into());
        }
        Self(conv)
    }
}

trait ToGLLBlocks {
    fn blocks(self) -> CodeGenResult<GLLBlocks>;
}

impl ToGLLBlocks for Rhs {
    fn blocks(self) -> CodeGenResult<GLLBlocks> {
        let mut blocks = GLLBlocks(Vec::new());
        let mut curr = GLLBlock(Vec::new());
        for chunk in self.chunks {
            let span = chunk.span();
            let symbols = match chunk.into_inner() {
                Chunk { ebnf: Some(_), .. } => return Err(CodeGenError::new_spanned(CodeGenErrorKind::Fatal("Encountered an EBNF-chunk when calculating GLL-blocks. Should have been factored out".to_string()), span)),
                c => c.extract_symbols(), // Deal with groups
            };
            for symbol in symbols {
                let is_terminal = symbol.to_inner().is_terminal();
                curr.0.push(symbol);
                if !is_terminal {
                    blocks.0.push(curr);
                    curr = GLLBlock(Vec::new());
                }
            }
        }
        blocks.0.push(curr);
        Ok(blocks)
    }
}

#[cfg(test)]
mod tests {
    use wagon_parser::WrapSpannable;
    use wagon_parser::parser::chunk::ChunkP;
    use wagon_parser::parser::{chunk::Chunk, symbol::Symbol};

    use super::{Rhs, GLLBlocks, ToGLLBlocks, SpannableNode};

    use pretty_assertions::assert_eq;
    use wagon_macros::unspanned_tree;


    #[test]
    fn test_simple_gll_blocks() {
        let rhs = unspanned_tree!(Rhs {
            weight: None,
            chunks: vec![
                Chunk::simple_terminal("a"),
                Chunk::simple_terminal("b"),
                Chunk::simple_ident("C"),
                Chunk::simple_ident("D"),
                Chunk {
                    chunk: ChunkP::Group(vec![Chunk::simple_terminal("e"), Chunk::simple_ident("F")]),
                    ebnf: None
                }
            ],
        });
        let blocks = rhs.blocks();
        let expected: Vec<Vec<SpannableNode<Symbol>>> = vec![
            vec![
                Symbol::simple_terminal("a"),
                Symbol::simple_terminal("b"),
                Symbol::simple_ident("C"),
            ].wrap_spannable(),
            vec![
                Symbol::simple_ident("D")
            ].wrap_spannable(),
            vec![
                Symbol::simple_terminal("e"),
                Symbol::simple_ident("F")
            ].wrap_spannable(),
            vec![]
        ];
        assert_eq!(blocks.unwrap(), GLLBlocks::from(expected));
    }

    #[test]
    fn test_single_gll_block() {
        let rhs = unspanned_tree!(Rhs {
            weight: None,
            chunks: vec![
                Chunk::simple_terminal("a")
            ]
        });
        let blocks = rhs.blocks();
        let expected: Vec<Vec<SpannableNode<Symbol>>> = vec![
            vec![
                Symbol::simple_terminal("a")
            ].wrap_spannable()
        ];
        assert_eq!(blocks.unwrap(), GLLBlocks::from(expected));
    }
}
