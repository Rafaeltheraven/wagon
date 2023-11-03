use crate::parser::assignment::Assignment;
use proc_macro2::Ident;
use quote::quote;
use super::{Rc, CodeGenState};

impl Assignment {
	pub(crate) fn gen(self, state: &mut CodeGenState, label: Rc<Ident>) {
		let expr = self.expr;
		let ident = self.ident;
		let ident_label = ident.to_ident();
		state.add_code(label, quote!(
			#ident_label = #expr;
			state.set_attribute(#ident, #ident_label);
		));
	}
}