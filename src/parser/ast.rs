use petgraph::{Graph, Directed, graph::DefaultIx, graph::NodeIndex, Direction::Incoming};

use crate::lexer::{productions::EbnfType, ident::Ident};

use super::{metadata::Metadata, rule::Arrow, terminal::Terminal, inverse::InverseNode, comp::CompOp, sum::Op1, term::Op2, atom::AtomNode};

pub(crate) enum WagNode {
	Root(Metadata),
	Rule(String, Arrow),
	Rhs(bool),
	Chunk(Option<EbnfType>),
	Assignments,
	Generic(String),
	Ident(Ident),
	Terminal(Terminal),
	SubProc(String),
	Disjunct,
	Conjunct,
	If,
	Inverse(InverseNode),
	Comparison(Option<CompOp>),
	Sum(Option<Op1>),
	Term(Option<Op2>),
	Atom(AtomNode),
	Factor,
	Empty,
}

pub(crate) type WagIx = NodeIndex<DefaultIx>;

pub(crate) type WagTree = Graph<WagNode, (), Directed, DefaultIx>;

pub(crate) trait ToAst {
	fn to_ast(self, ast: &mut WagTree) -> WagIx;
	fn add_vec_children<T: ToAst>(node: WagNode, children: Vec<T>, ast: &mut WagTree) -> WagIx {
		let node = ast.add_node(node);
		for child in children {
			let child_ix = child.to_ast(ast);
			ast.add_edge(node, child_ix, ());
		}
		node
	}
}

trait TreeFuncs {
	fn get_root(&self) -> WagIx;
}

impl TreeFuncs for WagTree {
    fn get_root(&self) -> WagIx {
        for vertex in self.node_indices() {
        	if self.neighbors_directed(vertex, Incoming).next().is_none() {
        		return vertex
        	}
        }
        panic!("{:?}", "Tree has no root!");
    }
}
