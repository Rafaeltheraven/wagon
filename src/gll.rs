
use crate::{parser::rhs::Rhs};

mod sppf;
mod state;
mod gss;
mod descriptor;

#[derive(Debug, PartialEq, Eq, Hash)]
pub(crate) struct GrammarSlot<'a> {
	label: String,
	rule: &'a Rhs,
	dot: usize
}

impl<'a> GrammarSlot<'a> {
	fn new(label: String, rule: &'a Rhs, dot: usize) -> Self {
		Self {label, rule, dot}
	}

	fn is_special(&self) -> bool {
		self.dot == 1 && !self.is_last()
	}

	fn is_eps(&self) -> bool {
		self.len() == 0 
	}

	fn is_last(&self) -> bool {
		self.dot == self.len()
	}

	fn len(&self) -> usize {
		self.rule.chunks.len()
	}
}