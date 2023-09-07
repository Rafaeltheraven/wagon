use petgraph::{Graph, Undirected, graph::DefaultIx};

use super::wag::Wag;

pub(crate) enum WagNode {
	Root(Wag)
}

pub(crate) type WagTree = Graph<WagNode, (), Undirected, DefaultIx>;

pub(crate) trait ToGraph {
	fn to_graph(graph: &mut WagTree) -> ();
	fn to_node(self) -> WagNode;
}
