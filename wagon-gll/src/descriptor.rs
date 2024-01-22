use std::{rc::Rc, hash::Hash};
use super::{GrammarSlot, gss::GSSNodeIndex, sppf::SPPFNodeIndex};

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Descriptor<'a> {
	pub(crate) slot: Rc<GrammarSlot<'a>>,
	pub(crate) gss: GSSNodeIndex,
	pub(crate) pointer: usize,
	pub(crate) sppf: SPPFNodeIndex,
	pub(crate) context_pointer: GSSNodeIndex
}

impl<'a> Descriptor<'a> {
	pub fn new(slot: Rc<GrammarSlot<'a>>, gss: GSSNodeIndex, pointer: usize, sppf: SPPFNodeIndex, context_pointer: GSSNodeIndex) -> Self {
		Self { slot, gss, pointer, sppf, context_pointer }
	}
}