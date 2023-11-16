use indexmap::IndexSet;
use crate::parser::assignment::Assignment;
use proc_macro2::{Ident};
use quote::quote;
use super::{Rc, CodeGenState};

impl Assignment {
	pub(crate) fn gen(self, state: &mut CodeGenState, label: Rc<Ident>, args: &mut IndexSet<wagon_gll::ident::Ident>) {
		let expr = self.expr;
		let ident = self.ident;
		let ident_label = ident.to_ident();
		args.insert(ident);
		let expr_stream = expr.to_tokens(state, label.clone(), false);
		state.add_code(label, quote!(
			let #ident_label: wagon_gll::value::Value = #expr_stream;
		));
	}
}