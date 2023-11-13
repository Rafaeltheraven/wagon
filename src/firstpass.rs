use std::collections::HashMap;

use indexmap::IndexSet;
use wagon_gll::ident::Ident;

pub(crate) trait Rewrite<T> {
	fn rewrite(&mut self, depth: usize, state: &mut FirstPassState) -> FirstPassResult<T>;
}

#[derive(Debug, Default)]
pub(crate) struct FirstPassState {
	parameter_map: HashMap<String, IndexSet<Ident>>
}

pub(crate) type FirstPassResult<T> = Result<T, WagCheckError>;

#[derive(PartialEq, Debug)]
pub(crate) enum WagCheckError {
	DuplicateParameters(Ident),
	DisparateParameters(String, Vec<Ident>, Vec<Ident>)
}

impl FirstPassState {
	pub(crate) fn add_parameter(&mut self, nt: String, param: Ident) -> FirstPassResult<()> {
		if let Some(params) = self.parameter_map.get_mut(&nt) {
			if !params.contains(&param) {
				params.insert(param);
				Ok(())
			} else {
				Err(WagCheckError::DuplicateParameters(param))
			}
		} else {
			let mut set = IndexSet::new();
			set.insert(param);
			self.parameter_map.insert(nt, set);
			Ok(())
		}
	}

	pub(crate) fn get_parameters(&self, s: &str) -> &IndexSet<Ident> {
		self.parameter_map.get(s).unwrap()
	}
}
