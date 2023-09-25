
use crate::{parser::rhs::Rhs};

mod sppf;
mod state;
mod gss;
mod descriptor;

#[derive(Debug, PartialEq, Eq, Hash)]
pub(crate) struct GrammarSlot<'a> {
	label: String,
	rules: Vec<&'a Rhs>,
	alt: usize,
	dot: usize
}

impl<'a> GrammarSlot<'a> {
	fn new(label: String, rules: Vec<&'a Rhs>, alt: usize, dot: usize) -> Self {
		Self {label, rules, alt, dot}
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
		self.rules[self.alt].chunks[0].symbols.len()
	}
}