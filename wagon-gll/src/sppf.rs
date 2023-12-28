use std::{rc::Rc, str::from_utf8, collections::{HashMap, HashSet}, ops::{DerefMut, Deref}};
use petgraph::{Graph, Directed, graph::{DefaultIx, NodeIndex}, Incoming, Outgoing, visit::EdgeRef};
use derivative::Derivative;

use crate::{state::GLLState, value::Value, AttributeKey, ReturnMap, gss::{GSSNode, GSSNodeIndex}};

use wagon_codegen::value::Valueable;

use super::{GrammarSlot, Terminal};

/// SPPF Nodes are stored into a tuple, and references are stored as integer
pub type SPPFNodeIndex = NodeIndex<SPPFIx>;

type SPPFIx = DefaultIx;

pub type SPPFGraph<'a> = Graph<SPPFNode<'a>, Option<Value<'a>>, Directed, SPPFIx>;

#[derive(Debug, Clone)]
pub struct SPPF<'a>(SPPFGraph<'a>);

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

impl<'a> Deref for SPPF<'a> { // This is fine because every SPPF is a graph, so methods can be safely passed along
    type Target = SPPFGraph<'a>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'a> DerefMut for SPPF<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<'a> SPPF<'a> {
    pub fn find_accepting_roots(&self, input_target: Option<usize>) -> Vec<SPPFNodeIndex> {
        let mut roots = Vec::new();
        for ix in self.0.node_indices() {
            let node = self.0.node_weight(ix).unwrap();
            let has_parents = self.0.neighbors_directed(ix, Incoming).next().is_some();
            match node {
                SPPFNode::Intermediate { slot, left, right, .. } if !has_parents && slot.dot == slot.rule.len()+1 && left == &0 && (input_target.is_none() || input_target.is_some_and(|x| &x == right)) => roots.push(ix),
                _ => {}
            }
        }
        roots
    }

    pub fn crop(&mut self, roots: Vec<SPPFNodeIndex>) -> Result<(), ()> {
        if !roots.is_empty() {
            let distances = roots.into_iter().flat_map(|x| petgraph::algo::dijkstra(&self.0, x, None, |_| 1).into_keys());
            let reachable: HashSet<SPPFNodeIndex> = distances.collect();
            self.0.retain_nodes(|_, x| reachable.contains(&x));
            Ok(())
        } else {
            Err(())
        }
    }

    pub fn to_dot(&self, state: &GLLState<'a>) -> String {
        let mut res = String::new();
        res.push_str("digraph {\n");
        for ix in self.0.node_indices() {
            let node = self.0.node_weight(ix).unwrap();
            res.push_str(&format!("{} [label=\"{}\" shape={}]\n", ix.index(), node.to_string(state), node.dot_shape()));
            for edge in self.0.edges_directed(ix, Outgoing) {
                let child = edge.target();
                res.push_str(&format!("{} -> {}", ix.index(), child.index()));
                if let Some(value) = edge.weight() {
                    res.push_str(&format!(" [label=\"{}\"]", value.display_numerical()))
                }
                res.push('\n');
            }
        }
        res.push('}');
        res
    }

    pub fn find_leafs(&self) -> Vec<SPPFNodeIndex> {
        let mut leafs = Vec::new();
        for ix in self.0.node_indices() {
            let has_children = self.0.neighbors_directed(ix, Outgoing).next().is_some();
            if !has_children {
                leafs.push(ix)
            }
        }
        leafs
    }

    pub fn deforest_indices(&self, roots: Vec<SPPFNodeIndex>) -> Result<Vec<HashSet<SPPFNodeIndex>>, ()> {
        let mut trees = Vec::new();
        for root in roots {
            let mut tree = HashSet::new();
            let mut candidates = vec![root];
            let mut subtrees = Vec::new();
            while let Some(ix) = candidates.pop() {
                let node = self.0.node_weight(ix).unwrap();
                tree.insert(ix);
                match node {
                    SPPFNode::Dummy => {},
                    SPPFNode::Symbol { .. } | SPPFNode::Packed { .. } => {
                        for child in self.0.neighbors_directed(ix, Outgoing) {
                            candidates.push(child);
                        }
                    },
                    SPPFNode::Intermediate { .. } => {
                        let children: Vec<SPPFNodeIndex> = self.0.neighbors_directed(ix, Outgoing).collect();
                        subtrees.extend(self.deforest_indices(children)?);
                    },
                };
            }
            if subtrees.is_empty() {
                trees.push(tree);
            } else {
                for subtree in subtrees {
                    let mut new_tree = tree.clone();
                    new_tree.extend(subtree);
                    trees.push(new_tree);
                }
            }   
        }
        Ok(trees)
    }

    pub fn deforest(self, roots: Vec<SPPFNodeIndex>) -> Result<Vec<SPPF<'a>>, ()> {
        let trees = self.deforest_indices(roots)?;
        let mut new_graphs = Vec::with_capacity(trees.len());
        for tree in trees {
            let mut clone = self.clone();
            clone.retain_nodes(|_, x| tree.contains(&x));
            new_graphs.push(clone);
        }
        Ok(new_graphs)
    }
}

impl Default for SPPF<'_> {
    fn default() -> Self {
        Self(SPPFGraph::new())
    }
}