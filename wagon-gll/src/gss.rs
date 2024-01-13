use std::{rc::Rc, hash::{Hash, Hasher}, format};

use petgraph::{Graph, graph::{DefaultIx, NodeIndex}, Directed};

use crate::{state::GLLState, value::Value, AttributeMap, AttributeKey};

use super::{GrammarSlot, sppf::SPPFNodeIndex};

pub(crate) type GSSNodeIndex = NodeIndex<GSSIx>;

type GSSIx = DefaultIx;

/// A [`petgraph::Graph`] representing the GSS.
///
/// This is a directed graph with [`GSSNode`]s on the vertices, and [`SPPFNodeIndex`] on the edges.
pub type GSS<'a> = Graph<Rc<GSSNode<'a>>, SPPFNodeIndex, Directed, GSSIx>;

#[derive(Debug, PartialEq, Eq, Hash)]
/// The data for the vertices on the [`GSS`].
pub struct GSSNode<'a> {
	/// The [`GrammarSlot`] at this node.
	pub(crate) slot: Rc<GrammarSlot<'a>>,
	/// Where the input pointer was at this point.
	inp_pointer: usize,
	/// All the attributes either stored in the context or passed along to the non-terminal.
	attributes: AttributeMap<'a>
}

impl<'a> GSSNode<'a> {
	/// Construct a new node.
	pub fn new(slot: Rc<GrammarSlot<'a>>, inp_pointer: usize, attributes: AttributeMap<'a>) -> Self {
		Self {slot, inp_pointer, attributes}
	}

	/// Represent the GSS node in the format `[{self.slot}, {self.inp_pointer}]`.
	pub fn to_string(&self, state: &GLLState<'a>) -> String {
		format!("[{},{}]", self.slot.to_string(state), self.inp_pointer)
	}

	/// Get an attribute from the attributes list.
	pub fn get_attribute(&self, i: AttributeKey) -> Option<&Value<'a>> {
		self.attributes.get(i)
	}

	/// Get a reference to the [`GrammarSlot`].
	pub fn get_slot(&self) -> &Rc<GrammarSlot<'a>> {
		&self.slot
	}

	/// Compare the attributes at this node to those at another node.
	pub(crate) fn cmp_attributes(orig: &Self, other: &Self) -> bool {
		orig.attributes == other.attributes
	}

	/// Call [`hash`] on the attributes only.
	pub(crate) fn hash_attributes<H: Hasher>(cand: &Self, state: &mut H) {
		cand.attributes.hash(state);
	}
}