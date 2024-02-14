
use wagon_parser::{SpannableNode, Spannable};
use wagon_parser::parser::assignment::Assignment;

use quote::quote;

use wagon_codegen::ToTokensState;
use crate::{CodeGenState, CodeGenArgs, CodeGen, CodeGenResult, CodeGenErrorKind, CodeGenError};

impl CodeGen for SpannableNode<Assignment> {
	fn gen(self, gen_args: &mut CodeGenArgs) -> CodeGenResult<()> {
		let state = &mut gen_args.state;
		let label = gen_args.label.as_ref().ok_or_else(|| CodeGenError::new_spanned(CodeGenErrorKind::MissingArg("label".to_string()), self.span()))?;
		let args = gen_args.full_args.as_mut().ok_or_else(|| CodeGenError::new_spanned(CodeGenErrorKind::MissingArg("full_args".to_string()), self.span()))?;

		let node = self.into_inner();
		let expr = node.expr;
		let ident = node.ident;
		let ident_label = ident.to_inner().to_ident();
		args.insert(ident);
		let expr_stream = expr.to_tokens(state, label.clone(), CodeGenState::add_req_code_attr);
		state.add_code(label.clone(), quote!(
			let #ident_label: wagon_gll::value::Value = (#expr_stream).into();
		));
		Ok(())
	}
}