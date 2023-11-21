use crate::lexer::Spannable;
use crate::parser::SpannableNode;
use logos::Span;
use std::collections::HashMap;

use indexmap::IndexSet;
use wagon_gll::ident::Ident;

pub(crate) trait Rewrite<T> {
	fn rewrite(&mut self, depth: usize, state: &mut FirstPassState) -> FirstPassResult<T>;
}

#[derive(Debug, Default)]
pub(crate) struct FirstPassState {
	parameter_map: HashMap<String, IndexSet<SpannableNode<Ident>>>
}

pub(crate) type FirstPassResult<T> = Result<T, WagCheckError>;

#[derive(PartialEq, Debug)]
pub(crate) enum WagCheckError {
	DuplicateParameters(String, SpannableNode<Ident>),
	DisparateParameters {
		terminal: String,
		offender: Vec<SpannableNode<Ident>>,
		expected: Vec<SpannableNode<Ident>>,
		span: Span
	}
}

impl WagCheckError {
	pub(crate) fn msg(&self) -> (String, String) {
		match self {
		    WagCheckError::DuplicateParameters(nt, i) => ("Duplicate Parameter!".to_string(), format!("Nonterminal {} uses parameter {} multiple times. This makes no sense.", nt, i)),
    		WagCheckError::DisparateParameters { terminal, offender, expected, ..} => ("Disparate Parameters!".to_string(), format!("Instance of terminal {} uses parameters {:?} while it was defined earlier as {:?}. This is unsupported", terminal, offender, expected)),
		}
	}

	pub(crate) fn span(self) -> Span {
		match self {
		    WagCheckError::DuplicateParameters(_, mut i) => i.span(),
    		WagCheckError::DisparateParameters { span, .. } => span,
		}
	}
}

impl FirstPassState {
	pub(crate) fn add_parameter(&mut self, nt: String, param: SpannableNode<Ident>) -> FirstPassResult<()> {
		if let Some(params) = self.parameter_map.get_mut(&nt) {
			if !params.contains(&param) {
				params.insert(param);
				Ok(())
			} else {
				Err(WagCheckError::DuplicateParameters(nt, param))
			}
		} else {
			let mut set = IndexSet::new();
			set.insert(param);
			self.parameter_map.insert(nt, set);
			Ok(())
		}
	}

	pub(crate) fn get_parameters(&self, s: &str) -> &IndexSet<SpannableNode<Ident>> {
		self.parameter_map.get(s).unwrap()
	}
}
