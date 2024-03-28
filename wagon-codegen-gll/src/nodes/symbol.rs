
use wagon_utils::Spannable;
use crate::Block;
use crate::BlockSize;
use crate::FullArgs;
use crate::Ident;
use crate::PrevArgs;
use crate::Rc;
use std::matches;

use quote::quote;

use wagon_codegen::SpannableIdent;
use wagon_parser::parser::{symbol::Symbol, terminal::Terminal};
use wagon_parser::SpannableNode;
use proc_macro2::{Literal, TokenStream};

use crate::state::CodeGenState;
use crate::{CodeGenArgs, CodeGen, CharBytes, CodeGenResult, CodeGenError, CodeGenErrorKind};

type Counts = (crate::Symbol, Block, BlockSize);

type SymbolUuid = String;
type RuleUuid = String;
type UUIDs = (SymbolUuid, RuleUuid);

type FirstSymbol = bool;
type FoundFirst = bool;
type Checks = (FirstSymbol, FoundFirst);

type Args<'a> = (PrevArgs, &'a FullArgs);

/// Codegen here is directly lifted from the OOGLL paper.
impl CodeGen for SpannableNode<Symbol> {
	fn gen(self, gen_args: &mut CodeGenArgs) -> CodeGenResult<()> {
		let span = self.span();
		let node = self.into_inner();
		let ident = gen_args.ident.as_ref().ok_or_else(|| CodeGenError::new_spanned(CodeGenErrorKind::MissingArg("ident".to_string()), span.clone()))?;
		let alt = gen_args.alt.as_ref().ok_or_else(|| CodeGenError::new_spanned(CodeGenErrorKind::MissingArg("alt".to_string()), span.clone()))?;
		let block = gen_args.block.ok_or_else(|| CodeGenError::new_spanned(CodeGenErrorKind::MissingArg("block".to_string()), span.clone()))?;
		let symbol = gen_args.symbol.ok_or_else(|| CodeGenError::new_spanned(CodeGenErrorKind::MissingArg("symbol".to_string()), span.clone()))?;
		let label = gen_args.label.as_ref().ok_or_else(|| CodeGenError::new_spanned(CodeGenErrorKind::MissingArg("label".to_string()), span.clone()))?;
		let block_size = gen_args.block_size.ok_or_else(|| CodeGenError::new_spanned(CodeGenErrorKind::MissingArg("block_size".to_string()), span.clone()))?;
		let found_first = gen_args.found_first.ok_or_else(|| CodeGenError::new_spanned(CodeGenErrorKind::MissingArg("found_first".to_string()), span.clone()))?;
		let full_args = gen_args.full_args.as_ref().ok_or_else(|| CodeGenError::new_spanned(CodeGenErrorKind::MissingArg("full_args".to_string()), span.clone()))?;
		let state = &mut gen_args.state;

		let first_symbol = block == 0 && symbol == 0;
		let uuid: String = ident.to_string();
		let rule_uuid = format!("{uuid}_{alt}");
		match node {
			Symbol::NonTerminal(i, args) => {
				if let Some(new_args) = handle_non_terminal(i, label, state, block, (args, full_args), (uuid, rule_uuid), (first_symbol, found_first))? {
					gen_args.prev_args = Some(new_args);
				}
			},
			Symbol::Assignment(v) => {
				for ass in v {
					ass.gen(gen_args)?;
				}
				gen_args.prev_args = Some(Vec::new());
			},
			Symbol::Terminal(t) => {
				handle_terminal(t, label, state, (symbol, block, block_size), (uuid, rule_uuid), (first_symbol, found_first))?;
				gen_args.found_first = Some(true);
				gen_args.prev_args = Some(Vec::new());
			},
			Symbol::Epsilon => {
				state.add_code(label.clone(), quote!(
					let cr = state.get_node_t((&[]).to_vec(), state.input_pointer, state.input_pointer);
					let slot = wagon_gll::GrammarSlot::new(
						state.get_label_by_uuid(#uuid)?,
						state.get_rule(#rule_uuid)?,
						1,
						0,
						#rule_uuid
					);
					state.sppf_pointer = state.get_node_p(std::rc::Rc::new(slot), state.sppf_pointer, cr, state.gss_pointer)?;
				));
				state.get_first(label)?[0].1 = Some(CharBytes::Epsilon);
				gen_args.found_first = Some(true);
				gen_args.prev_args = Some(Vec::new());
			},
		}
		Ok(())
	}
}

/// Codegen in case the symbol is a `NonTerminal`.
fn handle_non_terminal(i: SpannableIdent, label: &Rc<Ident>, state: &mut CodeGenState, block: Block, all_args: Args, uuids: UUIDs, checks: Checks) -> CodeGenResult<Option<Vec<SpannableIdent>>> {
	let (args, full_args) = all_args;
	let (uuid, rule_uuid) = uuids;
	let (first_symbol, found_first) = checks;
	// Because there is always an empty final block, we can safely increment this.
	let next_block = block + 1;
	let args_idents = args.iter().map(|x| x.to_inner().to_ident());
	let mut full_args_idents = Vec::with_capacity(full_args.len());
	for arg in full_args {
		state.add_req_code_attr(label.clone(), arg.clone());
		full_args_idents.push(arg.to_inner().to_ident());
	}
	let base = quote!(
		state.gss_pointer = state.create(
			&std::rc::Rc::new(wagon_gll::GrammarSlot::new(
				state.get_label_by_uuid(#uuid)?, 
				state.get_rule(#rule_uuid)?,
				#next_block,
				0, 
				#rule_uuid
			)),
			vec![#(#args_idents.clone(),)*#(#full_args_idents,)*]
		)?;
		label.code(state)
	);
	if first_symbol {
		state.add_code(label.clone(), quote!(
			let label = state.get_label(&#i);
			#base
		));
	} else {
		state.add_code(label.clone(), quote!(
			let label = state.get_label(&#i);
			if state.test_next(&label)? {
				#base
			} else {
				Ok(())
			}
		));
	}
	if !found_first {
		state.add_req_first_attr(label.clone(), i.clone());
		let stream = match i.clone().into_inner() {
		    wagon_ident::Ident::Unknown(s) => quote!(state.get_label_by_uuid(#s)?),
            other => {
            	let i = other.to_ident();
            	quote!(#i.try_into()?)
            }
		};
		state.get_first(label)?[0].0.push(stream);
		state.get_first_ident(label)?[0].push(i.clone());
	}
	if !matches!(i.to_inner(), wagon_ident::Ident::Unknown(_)) {
		state.add_req_code_attr(label.clone(), i);
	}
	Ok(Some(args))
}

fn handle_terminal(t: SpannableNode<Terminal>, label: &Rc<Ident>, state: &mut CodeGenState, counts: Counts, uuids: UUIDs, checks: Checks) -> CodeGenResult<()> {
	let (symbol, block, block_size) = counts;
	let (uuid, rule_uuid) = uuids;
	let (first_symbol, found_first) = checks;
	let mut stream = quote!(let i = state.input_pointer;);
	let (next_stream, cond_stream) = match t.into_inner() {
		Terminal::Regex(r, dfa) => {
			stream.extend(quote!(
				let pattern = #r;
			));
			if !found_first {
				state.get_first(label)?[0].0.push(quote!(state.get_label_by_uuid(#r)?));
			}
			state.regexes.push((r, dfa));
			if first_symbol && block_size != 1 {
				stream.extend(quote!(
					let bytes = state.next_regex(pattern)?.ok_or_else(|| wagon_gll::GLLImplementationError::Fatal("Failed to get match with regex, even though we already checked."))?;
					let new_node = state.get_node_t(bytes, i, state.input_pointer);
					state.sppf_pointer = new_node;
				));
				state.add_code(label.clone(), stream);
				return Ok(())
			}
			let next_stream = if first_symbol && block_size == 1 {
				quote!(
					let bytes = state.next_regex(pattern)?.ok_or_else(|| wagon_gll::GLLImplementationError::Fatal("Failed to get match with regex, even though we already checked."))?;
				)
			} else {
				TokenStream::new()
			};
			(next_stream, quote!(let Some(bytes) = state.next_regex(pattern)))
		},
		Terminal::LitString(s) => {
			let bytes = Literal::byte_string(s.as_bytes());
			stream.extend(quote!(
				let bytes = (#bytes).to_vec();
			));
			if !found_first {
				state.get_first(label)?[0].1 = Some(CharBytes::Bytes(bytes));
			}
			if first_symbol && block_size != 1 {
				stream.extend(quote!(
					state.next(bytes.clone())?;
					let new_node = state.get_node_t(bytes, i, state.input_pointer);
					state.sppf_pointer = new_node;
				));
				state.add_code(label.clone(), stream);
				return Ok(());
			}
			(quote!(state.next(bytes)?;), quote!(state.has_next(bytes)))
		},
	};
	let (dot, pos) = if symbol == block_size-1 {
		(block+1, 0)
	} else {
		(block, symbol+1)
	};
	let base = quote!(
		#next_stream
		let node = state.get_node_t(bytes, i, state.input_pointer);
		let slot = wagon_gll::GrammarSlot::new(
			state.get_label_by_uuid(#uuid)?, 
			state.get_rule(#rule_uuid)?,
			#dot, 
			#pos,
			#rule_uuid
		);
		state.sppf_pointer = state.get_node_p(std::rc::Rc::new(slot), state.sppf_pointer, node, state.gss_pointer)?;
	);
	if !first_symbol {
		stream.extend(quote!(
			if #cond_stream {
				#base
			} else {
				return Ok(())
			}
		));
		state.add_code(label.clone(), stream);
	} else if block_size == 1 {
		stream.extend(base);
		state.add_code(label.clone(), stream);
	}
	Ok(())
}