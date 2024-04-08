use std::{rc::Rc, str::from_utf8, collections::{HashMap, HashSet}, ops::{DerefMut, Deref}};
use petgraph::{Graph, Directed, graph::{DefaultIx, NodeIndex}, Incoming, Outgoing, visit::EdgeRef};
use derivative::Derivative;

use crate::{state::GLLState, value::Value, AttributeKey, ReturnMap, gss::{GSSNode, GSSNodeIndex}, ImplementationResult, GLLImplementationError, ProcessResult, GLLProcessError};

use wagon_value::Valueable;

use super::{GrammarSlot, Terminal};

/// The type of index used internally by the [`SPPFGraph`].
pub type SPPFNodeIndex = NodeIndex<SPPFIx>;

type SPPFIx = DefaultIx;

/// A [`petgraph::Graph`] representing the SPPF.
///
/// This is a directed graph with [`SPPFNode`]s on the vertices and optionally [`wagon-gll::value::Value`] on the edges. 
pub type SPPFGraph<'a> = Graph<SPPFNode<'a>, Option<Value<'a>>, Directed, SPPFIx>;

#[derive(Debug, Clone)]
/// A struct around the [`SPPFGraph`] so that we can define functions for it.
pub struct SPPF<'a>(SPPFGraph<'a>);

#[derive(Debug, Eq, Clone, Derivative)]
#[derivative(PartialEq, Hash)]
/// All possible SPPF nodes.
///
/// For explanations of what these nodes are. See any GLL paper.
pub enum SPPFNode<'a> {
    /// $ node in original paper
    Dummy,
    /// Symbol node
    Symbol {
        /// The [`Terminal`] this symbol node represents
        terminal: Terminal<'a>,
        /// Where this terminal starts in the input string.
        left: usize,
        /// Where it ends.
        right: usize
    },
    /// Intermediate node
    Intermediate {
        /// The [`GrammarSlot`] this node represents.
    	slot: Rc<GrammarSlot<'a>>, 
        /// Where it starts in the input string.
    	left: usize, 
        /// Where it ends.
    	right: usize,
        /// All the attribute values returned by this slot.
        ret: ReturnMap<'a>,
        #[derivative(Hash(hash_with="crate::gss::GSSNode::hash_attributes"))]
        #[derivative(PartialEq(compare_with="crate::gss::GSSNode::cmp_attributes"))]
        /// Pointer to where the context of this slot can be found.
        ///
        /// During parsing, attributes are stored on an `GSSNode`. We'll call this specific node the "context".
        /// Later on, we want to restore this context. In order to easily find what context we want to restore after we are done parsing,
        /// this pointer tells us exactly where we can find it. 
        ///
        /// When we are comparing 2 intermediate nodes, we want to take the context into account.
        /// As such, we hash and compare this part of the node with 2 specific methods found in [`GSSNode`]. 
        context: Rc<GSSNode<'a>>,
    },
    /// Packed node
    Packed {
        /// What [`GrammarSlot`] this node represents.
    	slot: Rc<GrammarSlot<'a>>, 
        /// At what point of the input string this split occurs.
    	split: usize,
        /// Pointer to the context at this split.
        context: GSSNodeIndex
    },
}

impl<'a> SPPFNode<'a> {

    pub(crate) fn right_extend(&self) -> ImplementationResult<'a, usize> {
        match self {
            Self::Intermediate { right, .. } | Self::Symbol { right, .. } => Ok(*right),
            other => Err(GLLImplementationError::IncorrectSPPFType(vec!["Intermediate", "Symbol"], std::mem::discriminant(other)))
        }
    }

    pub(crate) fn left_extend(&self) -> ImplementationResult<'a, usize> {
        match self {
            Self::Intermediate { left, .. } | Self::Symbol { left, .. } => Ok(*left),
            other => Err(GLLImplementationError::IncorrectSPPFType(vec!["Intermediate", "Symbol"], std::mem::discriminant(other)))
        }
    }

    pub(crate) fn to_string(&self, state: &GLLState<'a>) -> ImplementationResult<'a, String> {
        Ok(match self {
            SPPFNode::Dummy => "D".to_string(),
            SPPFNode::Symbol { terminal, left, right } => {
                let term = if terminal.is_empty() {
                    "ε"
                } else {
                    from_utf8(terminal)?.trim_start()
                };
                format!("({term},{left},{right})")
            },
            SPPFNode::Intermediate { slot, left, right, ret, context } => {
                let mut attr_map: HashMap<&str, &Value> = HashMap::new();
                let dot = if slot.is_complete() {
                    slot.dot - 2
                } else {
                    slot.dot
                };
                let label = state.get_label(&slot.rule[dot]);
                let (from_ret, from_ctx) = label.attr_rep_map();
                let ret_len = from_ret.len();
                let slot_str = slot.to_string(state);
                for (i, attr) in from_ctx.iter().enumerate() {
                    attr_map.insert(attr, context.get_attribute(i + ret_len).ok_or_else(|| GLLImplementationError::MissingContext(i + ret_len, context.clone()))?);
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
                        let _ = write!(output, "{key}: {value}, ");
                        output
                    });
                attr_rep.pop(); // Remove leftover ", " from string rep
                attr_rep.pop();
                format!("({slot_str},{left},{right},<{attr_rep}>)")
            },
            SPPFNode::Packed { slot, split, .. } => format!("({}, {})", slot.to_string(state), split),
        })
    }

    pub(crate) const fn dot_shape(&self) -> &str {
        match self {
            SPPFNode::Dummy => "diamond", 
            SPPFNode::Symbol { .. } | SPPFNode::Packed { .. } => "oval",
            SPPFNode::Intermediate { .. } => "box",
        }
    }

    pub(crate) fn get_ret_val(&self, i: AttributeKey) -> ImplementationResult<'a, Option<&Value<'a>>> {
        match self {
            SPPFNode::Intermediate { ret, .. } => {
                Ok(ret.get(i).and_then(|v| v.as_ref()))
            },
            other => Err(GLLImplementationError::IncorrectSPPFType(vec!["Intermediate"], std::mem::discriminant(other)))
        }
    }

    pub(crate) fn add_ret_vals(&mut self, atts: &mut ReturnMap<'a>) -> ImplementationResult<'a, ()> {
        match self {
            SPPFNode::Intermediate { ret, .. } => {ret.append(atts); Ok(())},
            other => Err(GLLImplementationError::IncorrectSPPFType(vec!["Intermediate"], std::mem::discriminant(other)))
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
    /// Try to find a node that accepts the whole input string.
    ///
    /// Goes through the entire SPPF and attempts to locate the top-most node that consumes the string from `0` to `input_target`.
    ///
    /// If `input_target` is `None`, we just find the top-most node that consumes from `0`.
    ///
    /// # Panics
    /// This function will panic if, while iterating through all the node indices in the graph, it fails to get any node from the graph by index.
    /// This should be fundamentally impossible.
    #[must_use] 
    pub fn find_accepting_roots(&self, input_target: Option<usize>) -> Vec<SPPFNodeIndex> {
        let mut roots = Vec::new();
        for ix in self.0.node_indices() {
            #[allow(clippy::expect_used)]
            let node = self.0.node_weight(ix).expect("Getting node from graph by index returned by graph itself. Should be impossible to fail");
            let has_parents = self.0.neighbors_directed(ix, Incoming).next().is_some();
            match node {
                SPPFNode::Intermediate { slot, left, right, .. } if !has_parents && slot.dot == slot.rule.len()+1 && left == &0 && (input_target.is_none() || input_target.is_some_and(|x| &x <= right)) => roots.push(ix),
                _ => {}
            }
        }
        roots
    }

    /// Remove all nodes that can not be reached from the given set of nodes.
    ///
    /// Uses Dijkstra's algorithm to find all reachable nodes from a set of nodes and removes the rest.
    ///
    /// Mostly used together with [`find_accepting_roots`](`SPPF::find_accepting_roots`) in order to prune invalid parses.
    pub fn crop(&mut self, roots: Vec<SPPFNodeIndex>) {
        if !roots.is_empty() {
            let distances = roots.into_iter().flat_map(|x| petgraph::algo::dijkstra(&self.0, x, None, |_| 1).into_keys());
            let reachable: HashSet<SPPFNodeIndex> = distances.collect();
            self.0.retain_nodes(|_, x| reachable.contains(&x));
        } 
    }

    /// Convert the [`SPPF`] to `.dot` representation.
    ///
    /// # Errors
    /// This method will return a [`GLLImplementationError`] if it fails to represent either a [`SPPFNode`] or a [`Value`] as a string properly.
    ///
    /// # Panics
    /// This function will panic if, while iterating through all the node indices in the graph, it fails to get any node from the graph by index.
    /// This should be fundamentally impossible.
    pub fn to_dot(&self, state: &GLLState<'a>) -> ImplementationResult<'a, String>  {
        let mut res = String::new();
        res.push_str("digraph {\n");
        for ix in self.0.node_indices() {
            #[allow(clippy::expect_used)]
            let node = self.0.node_weight(ix).expect("Getting node from graph by index returned by graph itself. Should be impossible to fail");
            res.push_str(&format!("{} [label=\"{}\" shape={}]\n", ix.index(), node.to_string(state)?, node.dot_shape()));
            for edge in self.0.edges_directed(ix, Outgoing) {
                let child = edge.target();
                res.push_str(&format!("{} -> {}", ix.index(), child.index()));
                if let Some(value) = edge.weight() {
                    res.push_str(&format!(" [label=\"{}\"]", value.display_numerical()?));
                }
                res.push('\n');
            }
        }
        res.push('}');
        Ok(res)
    }

    /// Find all the leaves in the [`SPPF`].
    #[must_use] pub fn find_leafs(&self) -> Vec<SPPFNodeIndex> {
        let mut leafs = Vec::new();
        for ix in self.0.node_indices() {
            let has_children = self.0.neighbors_directed(ix, Outgoing).next().is_some();
            if !has_children {
                leafs.push(ix);
            }
        }
        leafs
    }

    /// Attempt to extract every possible tree from the [`SPPF`].
    ///
    /// As evident in the name, an SPPF is a type of graph known as a [Forest](https://en.wikipedia.org/wiki/Tree_(graph_theory)#Forest). 
    /// This method returns every single possible tree that could be drawn from the forest, starting from the given set of root nodes.
    /// Each resulting SPPF can be seen as a singular valid interpretation of the parse.
    ///
    /// This method simply recursively calls [`deforest_indices`](`SPPF::deforest_indices`) and returns a set of complete [`SPPF`]s that represent these trees.
    ///
    /// # Errors
    /// Returns a [`GLLProcessError::MissingSPPFNode`] if any of the nodes in `roots` does not actyally exist in the graph.
    pub fn deforest(self, roots: Vec<SPPFNodeIndex>) -> ProcessResult<Vec<SPPF<'a>>> {
        let trees = self.deforest_indices(roots)?;
        let mut new_graphs = Vec::with_capacity(trees.len());
        for tree in trees {
            let mut clone = self.clone();
            clone.retain_nodes(|_, x| tree.contains(&x));
            new_graphs.push(clone);
        }
        Ok(new_graphs)
    }

    /// Given a forest and some root nodes, attempt to extract sets of nodes that represent unique trees.
    ///
    /// The workhorse of [`deforest`](`SPPF::deforest`). Given a set of root nodes, explore the forest and split it up into forests.
    ///
    /// Each set in the result vector represents the nodes that are in a possible tree of the forest. We can then remove all other nodes from
    /// this forest such that in the end, only the tree remains.
    ///
    /// # How does it work?
    /// We start with an empty tree. We also create a queue of "candidate" nodes, in which we put the root we are currently working from.
    ///
    /// As long as there are nodes in the candidates queue, we pop one from the queue, add it to the current tree and inspect it.
    /// 
    /// ## In case of a packed node
    /// In an SPPF, every packed node represents a specific choice made during the parse. We either took rule X or rule Y to reach this node.
    /// Every child of a packed node is then an intermediate node or symbol node which we can just add to the queue. 
    ///
    /// ### In case of a symbol node
    /// If we have a symbol node, no splits are possible and we also just add the children to the queue.
    ///
    /// ## In case of an intermediate node
    /// If we have an intermediate node, we know that all the children are packed nodes. Each child represents a specific alternative that could
    /// be chosen during the parsing process. As such, each child is a different option in the "path" from the root to the leaves.
    ///
    /// We can see these child nodes as the roots of a set of subforests, which we themselves can again split into trees.
    /// We recursively call `deforest_indices` on all the children of this node and add them to a list of subtrees. We do **not** add them to the candidates queue.
    ///
    /// ## Finally
    /// Once the candidates queue is done, we check if we have any subtrees. If we do, we clone the tree we have currently created and create `n` new
    /// trees, each connected to one of the subtrees. We then add these new trees to the list of trees we want to return. If there are no subtrees, we
    /// just add the current tree to the return list. At the end, we should have a complete list of all possible trees.
    ///
    /// # Errors 
    /// Returns a [`GLLProcessError::MissingSPPFNode`] if any of the nodes in `roots` does not actually exist in the graph.
    pub fn deforest_indices(&self, roots: Vec<SPPFNodeIndex>) -> ProcessResult<Vec<HashSet<SPPFNodeIndex>>> {
        let mut trees = Vec::new();
        for root in roots {
            let mut tree = HashSet::new();
            let mut candidates = vec![root];
            let mut subtrees = Vec::new();
            while let Some(ix) = candidates.pop() {
                let node = self.0.node_weight(ix).ok_or(GLLProcessError::MissingSPPFNode(ix))?;
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
}

/// An empty forest
impl Default for SPPF<'_> {
    fn default() -> Self {
        Self(SPPFGraph::new())
    }
}