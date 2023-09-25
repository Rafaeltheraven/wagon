use std::rc::Rc;
use super::{GrammarSlot, gss::GSSNodeIndex, sppf::SPPFNodeIndex};

#[derive(Eq, Hash, PartialEq)]
pub(crate) struct Descriptor<'a> {
	slot: Rc<GrammarSlot<'a>>,
	gss: GSSNodeIndex,
	pointer: usize,
	sppf: SPPFNodeIndex

}

impl<'a> Descriptor<'a> {
	pub fn new(slot: Rc<GrammarSlot<'a>>, gss: GSSNodeIndex, pointer: usize, sppf: SPPFNodeIndex) -> Self {
		Self { slot, gss, pointer, sppf }
	}
}