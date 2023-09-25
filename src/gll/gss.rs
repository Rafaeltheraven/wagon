use std::rc::Rc;

use petgraph::{Graph, graph::{DefaultIx, NodeIndex}, Directed};

use super::{GrammarSlot, sppf::SPPFNodeIndex};

pub(crate) type GSSNodeIndex = NodeIndex<GSSIx>;

type GSSIx = DefaultIx;

#[derive(Debug, PartialEq, Eq, Hash)]
pub(crate) struct GSSNode<'a> {
	pub(crate) slot: Rc<GrammarSlot<'a>>,
	inp_pointer: usize
}

pub(crate) type GSS<'a> = Graph<Rc<GSSNode<'a>>, SPPFNodeIndex, Directed, GSSIx>;

impl<'a> GSSNode<'a> {
	pub(crate) fn new(slot: Rc<GrammarSlot<'a>>, inp_pointer: usize) -> Self {
		Self {slot, inp_pointer}
	}
}