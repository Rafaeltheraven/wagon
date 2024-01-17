use std::fmt::Display;
use wagon_lexer::{Span, Spannable};
use crate::SpannableNode;
use std::{collections::HashMap, error::Error};

use indexmap::IndexSet;
use wagon_ident::Ident;

/// Any node that can be rewritten in a different way for any reason should implement this trait.
pub(crate) trait Rewrite<T> {
	fn rewrite(&mut self, depth: usize, state: &mut FirstPassState) -> FirstPassResult<T>;
}

/// A state object to keep track of things during rewriting.
#[derive(Debug, Default)]
pub(crate) struct FirstPassState {
	parameter_map: HashMap<String, IndexSet<SpannableNode<Ident>>>
}

/// After rewriting/typechecking, we either return something or we have a [`WagCheckError`].
pub type FirstPassResult<T> = Result<T, WagCheckError>;

#[derive(PartialEq, Debug)]
/// Any error that can occur during the rewriting/checking process.
pub enum WagCheckError {
	/// A rule wants multiple parameters, but they are the exact same.
	DuplicateParameters(String, SpannableNode<Ident>),
	/// Two alternative instances of a rule want different parameters.
	DisparateParameters {
		/// The non-terminal which has the issue.
		terminal: String,
		/// The specific [`Ident`] which caused the issue.
		offender: Vec<SpannableNode<Ident>>,
		/// The [`Ident`] we expected to see.
		expected: Vec<SpannableNode<Ident>>,
		/// The span information of this node.
		span: Span
	},
}

impl Error for WagCheckError{}

impl WagCheckError {
	pub(crate) fn msg(&self) -> (String, String) {
		match self {
		    WagCheckError::DuplicateParameters(nt, i) => ("Duplicate Parameter!".to_string(), format!("Nonterminal {} uses parameter {} multiple times. This makes no sense.", nt, i)),
    		WagCheckError::DisparateParameters { terminal, offender, expected, ..} => ("Disparate Parameters!".to_string(), format!("Instance of terminal {} uses parameters {:?} while it was defined earlier as {:?}. This is unsupported", terminal, offender, expected)),
		}
	}

	pub(crate) fn span(self) -> Span {
		match self {
		    WagCheckError::DuplicateParameters(_, i) => i.span(),
    		WagCheckError::DisparateParameters { span, .. } => span,
		}
	}
}

impl Display for WagCheckError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    	let (head, msg) = self.msg();
    	write!(f, "{}: {}", head, msg)
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

	// pub(crate) fn get_parameters(&self, s: &str) -> Option<&IndexSet<SpannableNode<Ident>>> {
	// 	self.parameter_map.get(s)
	// }
}
