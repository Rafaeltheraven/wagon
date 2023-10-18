use std::{rc::Rc, hash::{Hash}};
use super::{GrammarSlot, gss::GSSNodeIndex, sppf::SPPFNodeIndex};

#[derive(Debug, PartialEq, Eq, Hash)]
pub(crate) struct Descriptor<'a> {
	pub(crate) slot: Rc<GrammarSlot<'a>>,
	pub(crate) gss: GSSNodeIndex,
	pub(crate) pointer: usize,
	pub(crate) sppf: SPPFNodeIndex
}

impl<'a> Descriptor<'a> {
	pub fn new(slot: Rc<GrammarSlot<'a>>, gss: GSSNodeIndex, pointer: usize, sppf: SPPFNodeIndex) -> Self {
		Self { slot, gss, pointer, sppf }
	}
}