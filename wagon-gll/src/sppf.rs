use std::{rc::Rc, str::from_utf8, collections::HashMap};
use petgraph::{Graph, Directed, graph::{DefaultIx, NodeIndex}};
use derivative::Derivative;

use crate::{state::GLLState, value::Value, AttributeKey, ReturnMap, gss::{GSSNode, GSSNodeIndex}};

use super::{GrammarSlot, Terminal};

/// SPPF Nodes are stored into a tuple, and references are stored as integer
pub type SPPFNodeIndex = NodeIndex<SPPFIx>;

type SPPFIx = DefaultIx;

pub type SPPFGraph<'a> = Graph<SPPFNode<'a>, Option<f32>, Directed, SPPFIx>;

#[derive(Debug, Eq, Clone, Derivative)]
#[derivative(PartialEq, Hash)]
pub enum SPPFNode<'a> {
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
    	right: usize,
        ret: ReturnMap<'a>,
        #[derivative(Hash(hash_with="crate::gss::GSSNode::hash_attributes"))]
        #[derivative(PartialEq(compare_with="crate::gss::GSSNode::cmp_attributes"))]
        // #[derivative(Hash="ignore")]
        // #[derivative(PartialEq="ignore")]
        context: Rc<GSSNode<'a>>,
    },
    Packed {
    	slot: Rc<GrammarSlot<'a>>, 
    	split: usize,
        context: GSSNodeIndex
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

    pub(crate) fn to_string(&self, state: &GLLState<'a>) -> String {
        match self {
            SPPFNode::Dummy => "D".to_string(),
            SPPFNode::Symbol { terminal, left, right } => {
                let term = if terminal.is_empty() {
                    "ε"
                } else {
                    from_utf8(terminal).unwrap()
                };
                format!("({},{},{})", term, left, right)
            },
            SPPFNode::Intermediate { slot, left, right, ret, context } => {
                let mut attr_map: HashMap<&str, &Value> = HashMap::new();
                let label = state.get_label(&slot.rule[slot.dot-2]);
                let (from_ret, from_ctx) = label.attr_rep_map();
                let ret_len = from_ret.len();
                for (i, attr) in from_ctx.iter().enumerate() {
                    attr_map.insert(attr, context.get_attribute(i + ret_len).unwrap());
                }
                for (i, attr) in from_ret.iter().enumerate() {
                    if let Some(Some(v)) = ret.get(i) {
                        attr_map.insert(attr, v);
                    }
                }
                let mut attr_rep: String = attr_map
                    .iter()
                    .fold(String::new(), |mut output, (key, value)| {
                        use std::fmt::Write as _;
                        let _ = write!(output, "{}: {}, ", key, value);
                        output
                    });
                attr_rep.pop();
                attr_rep.pop();
                format!("({},{},{},<{}>)", slot.to_string(state), left, right, attr_rep)
            },
            SPPFNode::Packed { slot, split, .. } => format!("({}, {})", slot.to_string(state), split),
        }
    }

    pub(crate) fn dot_shape(&self) -> &str {
        match self {
            SPPFNode::Dummy => "diamond", 
            SPPFNode::Symbol { .. } => "oval",
            SPPFNode::Intermediate { .. } => "box",
            SPPFNode::Packed { .. } => "oval",
        }
    }

    pub(crate) fn get_ret_val(&self, i: AttributeKey) -> Option<&Value<'a>> {
        match self {
            SPPFNode::Intermediate { ret, .. } => {
                if let Some(v) = ret.get(i) {
                    v.as_ref()
                } else {
                    None
                }
            },
            _ => panic!("This method should only be called on intermediate nodes")
        }
    }

    pub(crate) fn add_ret_vals(&mut self, atts: &mut ReturnMap<'a>) {
        match self {
            SPPFNode::Intermediate { ret, .. } => ret.append(atts),
            _ => panic!("This method should only be called on intermediate nodes")
        }
    }
}