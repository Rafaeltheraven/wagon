use std::{rc::Rc, hash::{Hash}};
use petgraph::{Graph, Directed, graph::{DefaultIx, NodeIndex}};

use super::{GrammarSlot, Terminal};

/// SPPF Nodes are stored into a tuple, and references are stored as integer
pub(crate) type SPPFNodeIndex = NodeIndex<SPPFIx>;

type SPPFIx = DefaultIx;

pub(crate) type SPPFGraph<'a> = Graph<SPPFNode<'a>, (), Directed, SPPFIx>;

#[derive(Debug, PartialEq, Eq, Hash)]
pub(crate) enum SPPFNode<'a> {
    /// $ node in original paper
    Dummy,
    Symbol {
        terminal: Terminal<'a>,
        left: usize,
        right: usize
    },
    Intermediate {
    	slot: Rc<GrammarSlot<'a>>, 
    	left: usize, 
    	right: usize
    },
    Packed {
    	slot: Rc<GrammarSlot<'a>>, 
    	split: usize
    },
}

impl<'a> SPPFNode<'a> {

    pub(crate) fn right_extend(&self) -> Option<usize> {
        match self {
            Self::Intermediate { right, .. } | Self::Symbol { right, .. } => Some(*right),
            _ => None
        }
    }

    pub(crate) fn left_extend(&self) -> Option<usize> {
        match self {
            Self::Intermediate { left, .. } | Self::Symbol { left, .. } => Some(*left),
            _ => None
        }
    }
}