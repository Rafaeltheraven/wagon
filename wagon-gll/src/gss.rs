use petgraph::visit::EdgeRef;
use petgraph::Outgoing;
use crate::ImplementationResult;
use std::{rc::Rc, hash::{Hash, Hasher}, format};

use petgraph::{Graph, graph::{DefaultIx, NodeIndex}, Directed};

use crate::{state::GLLState, value::Value, AttributeMap, AttributeKey};

use super::{GrammarSlot, sppf::SPPFNodeIndex};

pub(crate) type GSSNodeIndex = NodeIndex<GSSIx>;

type GSSIx = DefaultIx;

/// A [`petgraph::Graph`] representing the GSS.
///
/// This is a directed graph with [`GSSNode`]s on the vertices, and [`SPPFNodeIndex`] on the edges.
pub type GSSGraph<'a> = Graph<Rc<GSSNode<'a>>, SPPFNodeIndex, Directed, GSSIx>;
#[derive(Debug, Clone)]
/// A struct around the [`GSSGraph`] so that we can define functions for it.
pub struct GSS<'a>(GSSGraph<'a>);

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
	#[must_use] 
	pub fn new(slot: Rc<GrammarSlot<'a>>, inp_pointer: usize, attributes: AttributeMap<'a>) -> Self {
		Self {slot, inp_pointer, attributes}
	}

	/// Represent the GSS node in the format `[{self.slot}, {self.inp_pointer}]`.
	#[must_use] 
	pub fn to_string(&self, state: &GLLState<'a>, math_mode: bool) -> String {
		format!("[{},{}]", self.slot.to_string(state, math_mode), self.inp_pointer)
	}

	/// Get an attribute from the attributes list.
	#[must_use] 
	pub fn get_attribute(&self, i: AttributeKey) -> Option<&Value<'a>> {
		self.attributes.get(i)
	}

	/// Get a reference to the [`GrammarSlot`].
	#[must_use] 
	pub const fn get_slot(&self) -> &Rc<GrammarSlot<'a>> {
		&self.slot
	}

	/// Compare the attributes at this node to those at another node.
	pub(crate) fn cmp_attributes(orig: &Self, other: &Self) -> bool {
		orig.attributes == other.attributes
	}

	/// Call [`Hash::hash`] on the attributes only.
	pub(crate) fn hash_attributes<H: Hasher>(cand: &Self, state: &mut H) {
		cand.attributes.hash(state);
	}
}

impl<'a> std::ops::Deref for GSS<'a> { // This is fine because every GSS is a graph, so methods can be safely passed along
    type Target = GSSGraph<'a>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'a> std::ops::DerefMut for GSS<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<'a> Default for GSS<'a> {
    fn default() -> Self {
        Self(GSSGraph::new())
    }
}

impl<'a> GSS<'a> {
	/// Convert the [`GSS`] to `.dot` representation.
    ///
    /// # Errors
    /// This method will return a [`GLLImplementationError`] if it fails to represent a [`SPPFNode`] stored on an edge properly.
    ///
    /// # Panics
    /// This function will panic if, while iterating through all the node indices in the graph, it fails to get any node from the graph by index.
    /// This should be fundamentally impossible.
	pub fn to_dot(&self, state: &GLLState<'a>, math_mode: bool) -> ImplementationResult<'a, String> {
		let mut res = String::new();
		res.push_str("digraph {\n");
		for ix in self.0.node_indices() {
			#[allow(clippy::expect_used)]
            let node = self.0.node_weight(ix).expect("Getting node from graph by index returned by graph itself. Should be impossible to fail");
            res.push_str(&format!("{} [label=\"{}\"]\n", ix.index(), node.to_string(state, math_mode)));
            for edge in self.0.edges_directed(ix, Outgoing) {
                let child = edge.target();
                res.push_str(&format!("{} -> {}", ix.index(), child.index()));
                let sppf_node = edge.weight();
                let label = if let Ok(n) = state.get_sppf_node(*sppf_node) {
                	n.to_string(state, math_mode)? 
                } else {
                	"PRUNED".to_string()
                };
                res.push_str(&format!(" [label=\"{}\"]", label));
                res.push('\n');
            }
		}
		res.push('}');
		Ok(res)
	}
}