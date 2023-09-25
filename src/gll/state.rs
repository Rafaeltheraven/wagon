use std::{collections::{HashSet, HashMap}, rc::Rc};

use logos::{Lexer, Logos};
use petgraph::Direction::Outgoing;

use super::{gss::{GSS, GSSNodeIndex, GSSNode}, sppf::{SPPFGraph, SPPFNodeIndex, SPPFNode}, descriptor::Descriptor, GrammarSlot};

pub(crate) struct GLLState<'a, 'b, T: Logos<'b>> {
	// Main structures
	pub input: Lexer<'b, T>,
	pub gss: GSS<'a>,
	pub sppf: SPPFGraph<'a>,
	// Pointers
	pub input_pointer: usize, //C_i
	pub gss_pointer: GSSNodeIndex, // C_u
	pub sppf_pointer: SPPFNodeIndex, // C_n
	// Memoization
	pub todo: HashSet<Descriptor<'a>>, // R
	pub visited: HashSet<Descriptor<'a>>, // U
	pub pop: HashMap<GSSNodeIndex, Vec<SPPFNodeIndex>>, // P
	// Easy Maps
	pub gss_map: HashMap<Rc<GSSNode<'a>>, GSSNodeIndex>,
	pub sppf_map: HashMap<SPPFNode<'a>, GSSNodeIndex>
}

impl<'a, 'b, T: Logos<'b>> GLLState<'a, 'b, T> {
	fn create(&'a mut self, slot: Rc<GrammarSlot<'a>>) {
		let candidate = Rc::new(GSSNode::new(slot.clone(), self.input_pointer));
		let v = if let Some(i) = self.gss_map.get(&candidate) {
			i.to_owned()
		} else {
			let i = self.gss.add_node(candidate.clone());
			self.gss_map.insert(candidate, i);
			i
		};
		if self.gss.find_edge(v, self.gss_pointer).is_none() {
			self.gss.add_edge(v, self.gss_pointer, self.sppf_pointer);
			if let Some(nodes) = self.pop.get(&v) {
				for sppf_node in nodes.clone() {
					let y = self.get_node_p(slot.clone(), self.sppf_pointer, sppf_node);
					self.add(slot.clone(), y);
				}
			}
		}
	}

	fn get_packed_node(&self, parent: SPPFNodeIndex, ref_slot: Rc<GrammarSlot>, i: usize) -> Option<SPPFNodeIndex> {
		for child in self.sppf.neighbors_directed(parent, Outgoing) {
			match self.sppf.node_weight(child) {
				Some(SPPFNode::Packed { slot, split }) if *slot == ref_slot && *split == i => return Some(child),
				_ => {} 
			}
		}
		None
	}

	fn get_node_p(&mut self, slot: Rc<GrammarSlot<'a>>, left: SPPFNodeIndex, right: SPPFNodeIndex) -> SPPFNodeIndex {
		if slot.is_special() {
			right
		} else {
			let left_node = self.sppf.node_weight(left).unwrap();
			let right_node = self.sppf.node_weight(right).unwrap();
			let j =  right_node.right_extend().unwrap();
			if let SPPFNode::Dummy = left_node {
				let i = right_node.left_extend().unwrap();
				let node = self.find_or_create_sppf(slot.clone(), i, j);
				if self.get_packed_node(node, slot.clone(), i).is_none() {
					let packed = SPPFNode::Packed { slot, split: i };
					let ix = self.sppf.add_node(packed);
					self.sppf.add_edge(ix, right, ());
				}
				node
			} else {
				let (i, k) = (left_node.left_extend().unwrap(), left_node.right_extend().unwrap());
				let node = self.find_or_create_sppf(slot.clone(), i, j);
				if self.get_packed_node(node, slot.clone(), k).is_none() {
					let packed = SPPFNode::Packed { slot, split: k };
					let ix = self.sppf.add_node(packed);
					self.sppf.add_edge(ix, right, ());
					self.sppf.add_edge(ix, left, ());
				}
				node
			}
		}
	}

	fn get_node_t(&mut self, slot: Rc<GrammarSlot<'a>>) -> SPPFNodeIndex {
		let left = self.input_pointer;
		let right = if slot.is_eps() { left } else { left + 1 };
		self.find_or_create_sppf(slot, left, right)
	}

	fn find_or_create_sppf(&mut self, slot: Rc<GrammarSlot<'a>>, left: usize, right: usize) -> SPPFNodeIndex {
		let candidate = SPPFNode::Intermediate { slot: slot.clone(), left, right};
		*(self.sppf_map.get(&candidate).unwrap_or(&self.sppf.add_node(candidate)))
	}

	fn add(&mut self, slot: Rc<GrammarSlot<'a>>, s: SPPFNodeIndex) {
		let d = Descriptor::new(slot, self.gss_pointer, self.input_pointer, s);
		if !self.visited.contains(&d) {
			self.todo.insert(d);
		}
	}

	fn pop(&mut self) {
		if self.gss_pointer != 0.into() {
			let curr_map = self.pop.get_mut(&self.gss_pointer);
			if let Some(map) = curr_map {
				map.push(self.sppf_pointer); 
			} else {
				let map = vec![self.sppf_pointer];
				self.pop.insert(self.gss_pointer, map);
			}
			let slot = self.gss.node_weight(self.gss_pointer).unwrap().slot.clone();
			let gss = std::mem::take(&mut self.gss); // This is scary
			for child in gss.edges_directed(self.gss_pointer, Outgoing) {
				let y = self.get_node_p(slot.clone(), self.sppf_pointer, *child.weight());
				self.add(slot.clone(), y);
			}
			self.gss = gss;
		}
	}
}