
use wagon_parser::parser::assignment::Assignment;

use quote::quote;
use super::{CodeGenArgs, CodeGen, ToTokensState};

impl CodeGen for Assignment {
	fn gen(self, gen_args: &mut CodeGenArgs) {
		let mut state = &mut gen_args.state;
		let label = gen_args.label.as_ref().unwrap();
		let args = gen_args.full_args.as_mut().unwrap();

		let expr = self.expr;
		let ident = self.ident;
		let ident_label = ident.to_inner().to_ident();
		args.insert(ident);
		let expr_stream = expr.to_tokens(&mut state, label.clone(), false);
		state.add_code(label.clone(), quote!(
			let #ident_label: wagon_gll::value::Value = #expr_stream;
		));
	}
}