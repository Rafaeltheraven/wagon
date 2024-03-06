use std::{rc::Rc, hash::Hash};
use super::{GrammarSlot, gss::GSSNodeIndex, sppf::SPPFNodeIndex};

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
/// A GLL Descriptor
pub struct Descriptor<'a> {
	/// The slot of this descriptor.
	pub(crate) slot: Rc<GrammarSlot<'a>>,
	/// The gss node this descriptor belongs to.
	pub(crate) gss: GSSNodeIndex,
	/// The input pointer when this descriptor was made.
	pub(crate) pointer: usize,
	/// The sppf node this descriptor belongs to.
	pub(crate) sppf: SPPFNodeIndex,
	/// The attribute context for this descriptor.
	pub(crate) context_pointer: GSSNodeIndex
}

impl<'a> Descriptor<'a> {
	pub fn new(slot: Rc<GrammarSlot<'a>>, gss: GSSNodeIndex, pointer: usize, sppf: SPPFNodeIndex, context_pointer: GSSNodeIndex) -> Self {
		Self { slot, gss, pointer, sppf, context_pointer }
	}
}