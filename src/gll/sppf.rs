use std::{rc::Rc, hash::{Hash, Hasher}};
use petgraph::{Graph, Directed, graph::{DefaultIx, NodeIndex}};

use super::{GrammarSlot, Terminal};

/// SPPF Nodes are stored into a tuple, and references are stored as integer
pub(crate) type SPPFNodeIndex = NodeIndex<SPPFIx>;

type SPPFIx = DefaultIx;

pub(crate) type SPPFGraph<'a> = Graph<SPPFNode<'a>, (), Directed, SPPFIx>;

#[derive(Debug)]
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

impl<'a> PartialEq for SPPFNode<'a> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Dummy, Self::Dummy) => true,
            (Self::Intermediate { slot: slot1, left: left1, right: right1 }, Self::Intermediate { slot: slot2, left: left2, right: right2 }) => slot1 == slot2 && left1 == left2 && right1 == right2,
            (Self::Packed { slot: slot1, split: split1 }, Self::Packed { slot: slot2, split: split2 }) => slot1 == slot2 && split1 == split2,
            _ => false
        }
    }
}

impl<'a> Eq for SPPFNode<'a> {}

impl<'a> Hash for SPPFNode<'a> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            SPPFNode::Dummy => state.write_u8(1),
            SPPFNode::Symbol { terminal, left, right } => {
                state.write_u8(2);
                terminal.hash(state);
                left.hash(state);
                right.hash(state);
            },
            SPPFNode::Intermediate { slot, left, right } => {
                state.write_u8(3);
                slot.hash(state);
                left.hash(state);
                right.hash(state);
            },
            SPPFNode::Packed { slot, split } => {
                state.write_u8(4);
                slot.hash(state);
                split.hash(state);
            },
        }
    }
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