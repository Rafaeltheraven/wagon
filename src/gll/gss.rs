use std::{rc::Rc, hash::{Hash, Hasher}};

use petgraph::{Graph, graph::{DefaultIx, NodeIndex}, Directed};

use super::{GrammarSlot, sppf::SPPFNodeIndex};

pub(crate) type GSSNodeIndex = NodeIndex<GSSIx>;

type GSSIx = DefaultIx;

#[derive(Debug)]
pub(crate) struct GSSNode<'a> {
	pub(crate) slot: Rc<GrammarSlot<'a>>,
	inp_pointer: usize
}

impl<'a> PartialEq for GSSNode<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.slot == other.slot && self.inp_pointer == other.inp_pointer
    }
}

impl<'a> Eq for GSSNode<'a> {}

impl<'a> Hash for GSSNode<'a> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.slot.hash(state);
        self.inp_pointer.hash(state);
    }
}

pub(crate) type GSS<'a> = Graph<Rc<GSSNode<'a>>, SPPFNodeIndex, Directed, GSSIx>;

impl<'a> GSSNode<'a> {
	pub(crate) fn new(slot: Rc<GrammarSlot<'a>>, inp_pointer: usize) -> Self {
		Self {slot, inp_pointer}
	}
}