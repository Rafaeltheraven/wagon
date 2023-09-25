use std::rc::Rc;
use petgraph::{Graph, Directed, graph::{DefaultIx, NodeIndex}};

use super::{GrammarSlot};

/// SPPF Nodes are stored into a tuple, and references are stored as integer
pub(crate) type SPPFNodeIndex = NodeIndex<SPPFIx>;

type SPPFIx = DefaultIx;

pub(crate) type SPPFGraph<'a> = Graph<SPPFNode<'a>, (), Directed, SPPFIx>;

#[derive(Debug, PartialEq, Eq, Hash)]
pub(crate) enum SPPFNode<'a> {
    /// $ node in original paper
    Dummy,
    /// (slot, left, right, children)
    Intermediate {
    	slot: Rc<GrammarSlot<'a>>, 
    	left: usize, 
    	right: usize
    },
    /// (label, split, children)
    Packed {
    	slot: Rc<GrammarSlot<'a>>, 
    	split: usize
    },
}

impl<'a> SPPFNode<'a> {

    pub(crate) fn right_extend(&self) -> Option<usize> {
        match self {
            Self::Intermediate { right, .. } => Some(*right),
            _ => None
        }
    }

    pub(crate) fn left_extend(&self) -> Option<usize> {
        match self {
            Self::Intermediate { left, .. } => Some(*left),
            _ => None
        }
    }
}