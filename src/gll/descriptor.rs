use std::{rc::Rc, hash::{Hash, Hasher}};
use super::{GrammarSlot, gss::GSSNodeIndex, sppf::SPPFNodeIndex};

pub(crate) struct Descriptor<'a> {
	slot: Rc<GrammarSlot<'a>>,
	gss: GSSNodeIndex,
	pointer: usize,
	sppf: SPPFNodeIndex
}

impl<'a> PartialEq for Descriptor<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.slot == other.slot && self.gss == other.gss && self.pointer == other.pointer && self.sppf == other.sppf
    }
}

impl<'a> Eq for Descriptor<'a> {}

impl<'a> Hash for Descriptor<'a> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.slot.hash(state);
        self.gss.hash(state);
        self.pointer.hash(state);
        self.sppf.hash(state);
    }
}

impl<'a> Descriptor<'a> {
	pub fn new(slot: Rc<GrammarSlot<'a>>, gss: GSSNodeIndex, pointer: usize, sppf: SPPFNodeIndex) -> Self {
		Self { slot, gss, pointer, sppf }
	}
}