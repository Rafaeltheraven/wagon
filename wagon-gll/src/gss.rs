use std::{rc::Rc, hash::{Hash, Hasher}, format};

use petgraph::{Graph, graph::{DefaultIx, NodeIndex}, Directed};

use crate::{state::GLLState, value::Value, AttributeMap, AttributeKey};

use super::{GrammarSlot, sppf::SPPFNodeIndex};

pub(crate) type GSSNodeIndex = NodeIndex<GSSIx>;

type GSSIx = DefaultIx;

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct GSSNode<'a> {
	pub(crate) slot: Rc<GrammarSlot<'a>>,
	inp_pointer: usize,
	attributes: AttributeMap<'a>
}

pub type GSS<'a> = Graph<Rc<GSSNode<'a>>, SPPFNodeIndex, Directed, GSSIx>;

impl<'a> GSSNode<'a> {
	pub fn new(slot: Rc<GrammarSlot<'a>>, inp_pointer: usize, attributes: AttributeMap<'a>) -> Self {
		Self {slot, inp_pointer, attributes}
	}

	pub fn to_string(&self, state: &GLLState<'a>) -> String {
		format!("[{},{}]", self.slot.to_string(state), self.inp_pointer)
	}

	pub fn get_attribute(&self, i: AttributeKey) -> Option<&Value<'a>> {
		self.attributes.get(i)
	}

	pub fn get_slot(&self) -> &Rc<GrammarSlot<'a>> {
		&self.slot
	}

	pub(crate) fn cmp_attributes(orig: &Self, other: &Self) -> bool {
		orig.attributes == other.attributes
	}

	pub(crate) fn hash_attributes<H: Hasher>(cand: &Self, state: &mut H) {
		cand.attributes.hash(state);
	}
}