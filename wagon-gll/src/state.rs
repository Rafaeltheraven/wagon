use std::{collections::{HashSet, HashMap}, rc::Rc, format};

use indexmap::IndexSet;
use petgraph::{Direction::{Outgoing, Incoming}, visit::{EdgeRef}};

use crate::{value::Value, AttributeMap, AttributeKey, ReturnMap};

use super::{gss::{GSS, GSSNodeIndex, GSSNode}, sppf::{SPPFGraph, SPPFNodeIndex, SPPFNode}, descriptor::Descriptor, GrammarSlot, ParseResult, GLLParseError, Terminal, Ident, ROOT_UUID, GLLBlockLabel};

pub struct GLLState<'a> {
	// Main structures
	input: &'a [u8],
	gss: GSS<'a>,
	sppf: SPPFGraph<'a>,
	// Pointers
	pub input_pointer: usize, //C_i
	pub gss_pointer: GSSNodeIndex, // C_u
	gss_root: GSSNodeIndex, // Points to <⊥, 0>
	context_pointer: GSSNodeIndex, // Points to where the current context is stored
	pub sppf_pointer: SPPFNodeIndex, // C_n
	pub sppf_root: SPPFNodeIndex, // Points to $
	// Memoization
	todo: IndexSet<Descriptor<'a>>, // R
	visited: HashSet<Descriptor<'a>>, // U
	pop: HashMap<GSSNodeIndex, Vec<SPPFNodeIndex>>, // P
	// Easy Maps
	gss_map: HashMap<Rc<GSSNode<'a>>, GSSNodeIndex>,
	sppf_map: HashMap<SPPFNode<'a>, SPPFNodeIndex>,
	label_map: HashMap<&'a str, GLLBlockLabel<'a>>,
	rule_map: HashMap<&'a str, Rc<Vec<Ident>>>,
}

impl<'a> GLLState<'a> {
	pub fn init(input: &'a [u8], label_map: HashMap<&'a str, GLLBlockLabel<'a>>, rule_map: HashMap<&'a str, Rc<Vec<Ident>>>) -> Self {
		let mut sppf = SPPFGraph::new();
		let mut gss = GSS::new();
		let mut sppf_map = HashMap::new();
		let mut gss_map = HashMap::new();
		let root_slot = Rc::new(GrammarSlot::new(label_map.get(ROOT_UUID).unwrap().clone(), rule_map.get(ROOT_UUID).unwrap().clone(), 0, 0, ROOT_UUID, false));
		let gss_root_node = Rc::new(GSSNode::new(root_slot.clone(), 0, Default::default()));
		let sppf_root = sppf.add_node(SPPFNode::Dummy);
		let gss_root = gss.add_node(gss_root_node.clone());
		sppf_map.insert(SPPFNode::Dummy, sppf_root);
		gss_map.insert(gss_root_node, gss_root);
		let mut state = GLLState { 
			input, 
			gss, 
			sppf, 
			input_pointer: 0, 
			gss_pointer: gss_root,
			gss_root, 
			context_pointer: gss_root,
			sppf_pointer: sppf_root, 
			sppf_root, 
			todo: Default::default(), 
			visited: Default::default(), 
			pop: Default::default(), 
			gss_map: Default::default(), 
			sppf_map: Default::default(), 
			rule_map,
			label_map,
		};
		state.add(root_slot, gss_root, 0, sppf_root);
		state
	}

	/*
	L => slot
	u => gss_pointer
	i => input_pointer
	w => sppf_pointer
	*/
	pub fn create(&mut self, slot: Rc<GrammarSlot<'a>>, args: AttributeMap<'a>) -> GSSNodeIndex {
		let candidate = GSSNode::new(slot.clone(), self.input_pointer, args);
		let v = if let Some(i) = self.gss_map.get(&candidate) {
			i.to_owned()
		} else {
			let rc = Rc::new(candidate);
			let i = self.gss.add_node(rc.clone());
			self.gss_map.insert(rc, i);
			i
		};
		if self.gss.find_edge(v, self.gss_pointer).is_none() {
			self.gss.add_edge(v, self.gss_pointer, self.sppf_pointer);
			let pop = std::mem::take(&mut self.pop); // scary again
			if let Some(nodes) = pop.get(&v) {
				for sppf_node in nodes {
					let y = self.get_node_p(slot.clone(), self.sppf_pointer, *sppf_node, v);
					self.add(slot.clone(), self.gss_pointer, self.sppf.node_weight(*sppf_node).unwrap().right_extend().unwrap(), y);
				}
			}
			self.pop = pop;
		}
		v
	}

	fn get_packed_node(&self, parent: SPPFNodeIndex, ref_slot: Rc<GrammarSlot<'a>>, i: usize) -> Option<SPPFNodeIndex> {
		for child in self.sppf.neighbors_directed(parent, Outgoing) {
			match self.sppf.node_weight(child) {
				Some(SPPFNode::Packed { slot, split, .. }) if *slot == ref_slot && *split == i => return Some(child),
				_ => {} 
			}
		}
		None
	}

	pub fn get_node_p(&mut self, slot: Rc<GrammarSlot<'a>>, left: SPPFNodeIndex, right: SPPFNodeIndex, context_pointer: GSSNodeIndex) -> SPPFNodeIndex {
		if self.is_special_slot(&slot) {
			right
		} else {
			let left_node = self.sppf.node_weight(left).unwrap();
			let right_node = self.sppf.node_weight(right).unwrap();
			let j =  right_node.right_extend().unwrap();
			let t = if slot.is_last(self) {
				Rc::new(GrammarSlot { label: slot.label.clone(), rule: slot.rule.clone(), dot: slot.rule.len()+1, pos: 0, uuid: slot.uuid, probabilistic: slot.is_probabilistic() })
			} else {
				slot.clone()
			};
			if let SPPFNode::Dummy = left_node {
				let i = right_node.left_extend().unwrap();
				let node = self.find_or_create_sppf_intermediate(t.clone(), i, j, context_pointer);
				if self.get_packed_node(node, slot.clone(), i).is_none() {
					let packed = SPPFNode::Packed { slot, split: i, context: self.gss_pointer };
					let ix = self.sppf.add_node(packed);
					self.sppf.add_edge(ix, right, None);
					self.sppf.add_edge(node, ix, None);
				}
				node
			} else {
				let (i, k) = (left_node.left_extend().unwrap(), left_node.right_extend().unwrap());
				let node = self.find_or_create_sppf_intermediate(t.clone(), i, j, context_pointer);
				if self.get_packed_node(node, slot.clone(), k).is_none() {
					let packed = SPPFNode::Packed { slot, split: k, context: self.gss_pointer };
					let ix = self.sppf.add_node(packed);
					self.sppf.add_edge(ix, left, None);
					self.sppf.add_edge(ix, right, None);
					self.sppf.add_edge(node, ix, None);
				}
				node
			}
		}
	}

	pub fn get_node_t(&mut self, terminal: &'a [u8]) -> SPPFNodeIndex {
		let left = self.input_pointer;
		let right = left + terminal.len();
		self.find_or_create_sppf_symbol(terminal, left, right)
	}

	pub fn get_current_gss_node(&self) -> &Rc<GSSNode<'a>> {
		self.get_gss_node_unchecked(self.gss_pointer)
	}

	pub fn get_current_sppf_node(&self) -> &SPPFNode<'a> {
		self.get_sppf_node_unchecked(self.sppf_pointer)
	}

	fn get_sppf_node_unchecked(&self, i: SPPFNodeIndex) -> &SPPFNode<'a> {
		self.sppf.node_weight(i).unwrap()
	}

	fn get_gss_node_unchecked(&self, i: GSSNodeIndex) -> &Rc<GSSNode<'a>> {
		self.gss.node_weight(i).unwrap()
	}

	fn find_or_create_sppf_symbol(&mut self, terminal: &'a [u8], left: usize, right: usize) -> SPPFNodeIndex {
		let candidate = SPPFNode::Symbol { terminal, left, right };
		self.find_or_create_sppf(candidate)
	}

	fn find_or_create_sppf_intermediate(&mut self, slot: Rc<GrammarSlot<'a>>, left: usize, right: usize, context_pointer: GSSNodeIndex) -> SPPFNodeIndex {
		let candidate = SPPFNode::Intermediate { 
			slot: slot.clone(), 
			left, 
			right, 
			ret: Default::default(), 
			context: self.get_gss_node_unchecked(context_pointer).clone(),
		};
		self.find_or_create_sppf(candidate)
	}

	fn find_or_create_sppf(&mut self, candidate: SPPFNode<'a>) -> SPPFNodeIndex {
		if let Some(ix) = self.sppf_map.get(&candidate) {
			*ix
		} else {
			let ix = self.sppf.add_node(candidate.clone());
			self.sppf_map.insert(candidate, ix);
			ix
		}
	}

	pub fn add(&mut self, slot: Rc<GrammarSlot<'a>>, g: GSSNodeIndex, i: usize, s: SPPFNodeIndex) {
		let d = Descriptor::new(slot, g, i, s, self.gss_pointer);
		if !self.visited.contains(&d) {
			self.visited.insert(d.clone());
			self.todo.insert(d);
		}
	}

	/*
	From the original paper:
	u => Cu => gss_pointer (always)
	i => Ci => input_pointer (always)
	z => Cn => sppf_pointer (always)
	*/
	pub fn pop(&mut self, ret_vals: ReturnMap<'a>) {
		if self.gss_pointer != self.gss_root {
			let curr_map = self.pop.get_mut(&self.gss_pointer);
			if let Some(map) = curr_map {
				map.push(self.sppf_pointer); 
			} else {
				let map = vec![self.sppf_pointer];
				self.pop.insert(self.gss_pointer, map);
			}
			let slot = self.gss.node_weight(self.gss_pointer).unwrap().slot.clone();
			let mut detached = self.gss.neighbors_directed(self.gss_pointer, Outgoing).detach();
			while let Some(edge) = detached.next_edge(&self.gss) {
				let v = self.gss.edge_endpoints(edge).unwrap().1;
				let y = self.get_node_p(slot.clone(), *self.gss.edge_weight(edge).unwrap(), self.sppf_pointer, self.gss_pointer);
				self.sppf.node_weight_mut(y).unwrap().add_ret_vals(&mut ret_vals.clone());
				self.add(slot.clone(), v, self.input_pointer, y);
			}
		}
	}

	pub fn next(&mut self, bytes: Terminal<'a>) -> ParseResult<()> {
		let mut pointer = self.input_pointer;
		for expected in bytes {
			if pointer >= self.input.len() {
				return Err(GLLParseError::TooLong { pointer, offender: bytes })
			}
			let check = self.input[pointer];
			if check != *expected {
				return Err(GLLParseError::UnexpectedByte { pointer, expected: *expected, offender: check })
			}
			pointer += 1;
		};
		self.input_pointer = pointer;
		Ok(())
	}

	pub fn has_next(&mut self, bytes: Terminal<'a>) -> bool {
		let curr = self.input_pointer;
		let ret = self.next(bytes).is_ok();
		self.input_pointer = curr;
		ret
	}

	pub fn test_next(&mut self, label: GLLBlockLabel<'a>) -> bool {
		label.first(self)
	}

	pub fn get_rule(&self, ident: &str) -> Rc<Vec<Ident>> {
		self.rule_map.get(ident).unwrap_or_else(|| panic!("Issue unwrapping rule map. {} not in keyset", ident)).clone()
	}

	pub fn get_label(&self, ident: &Ident) -> GLLBlockLabel<'a> {
		let raw_string = ident.extract_string();
		if let Some(label) = self.label_map.get(raw_string) {
			label.clone()
		} else { // Must evaluate at runtime
			todo!()
		}
	}

	pub fn get_label_by_uuid(&self, label: &str) -> GLLBlockLabel<'a> {
		self.label_map.get(label).unwrap().clone()
	}

	pub fn get_attribute(&self, i: AttributeKey) -> &Value<'a> {
		self.get_attribute_at_gss_node(self.gss_pointer, i).expect("Not enough attributes passed to NT.")
	}

	pub fn restore_attribute(&self, i: AttributeKey) -> &Value<'a> {
		self.get_attribute_at_gss_node(self.context_pointer, i).expect("Error restoring context.")
	}

	pub(crate) fn get_attribute_at_gss_node(&self, pointer: GSSNodeIndex, i: AttributeKey) -> Option<&Value<'a>> {
		self.gss.node_weight(pointer).unwrap().get_attribute(i)
	}

	pub fn get_ret_val(&self, i: AttributeKey) -> Option<&Value<'a>> {
		self.sppf.node_weight(self.sppf_pointer).unwrap().get_ret_val(i)
	}

	fn is_special_slot(&self, slot: &GrammarSlot<'a>) -> bool {
		if slot.dot == 1 && slot.pos == 0 && !slot.is_last(self) {
			if let Some(r) = slot.rule.get(0) {
				let a = self.get_label(r);
				a.str_parts().len() == 1 && (a.is_terminal() || !a.is_nullable(self, &mut Default::default()))
			} else {
				false
			}
		} else {
			false
		}
	}

	fn get_current_label_slot(&self, slot: &GrammarSlot<'a>) -> GLLBlockLabel<'a> {
		self.get_label(slot.rule.get(slot.dot).unwrap())
	}

	fn goto(&mut self, slot: &GrammarSlot<'a>) {
		let label = self.get_current_label_slot(slot);
		label.code(self)
	}

	pub fn main(&mut self) {
		while let Some(Descriptor {slot, gss, pointer, sppf, context_pointer}) = self.todo.pop() {
			self.sppf_pointer = sppf;
			self.gss_pointer = gss;
			self.input_pointer = pointer;
			self.context_pointer = context_pointer;
			self.goto(&slot);
		}
	}

	/// Print current SPPF graph in graphviz format
    pub fn print_sppf_dot(&mut self, crop: bool) -> String {
    	let curr_pointer = self.gss_pointer;
    	if crop {
    		self.crop_sppf().unwrap();
    	}
    	// if probs {
    	// 	self.calc_probabilities();
    	// }
        let mut res = String::new();
        res.push_str("digraph {\n");
        for ix in self.sppf.node_indices() {
        	let node = self.sppf.node_weight(ix).unwrap();
        	res.push_str(&format!("{} [label=\"{}\" shape={}]\n", ix.index(), node.to_string(self), node.dot_shape()));
        	for edge in self.sppf.edges_directed(ix, Outgoing) {
        		let child = edge.target();
        		res.push_str(&format!("{} -> {}", ix.index(), child.index()));
        		res.push('\n');
        	}
        }
        res.push('}');
        self.gss_pointer = curr_pointer;
        res
    }

	/// Print current GSS graph in graphviz format
    pub fn print_gss_dot(&self) -> String {
        format!("{:?}", petgraph::dot::Dot::new(&self.gss))
    }
 	
 	/// Checks whether the current parser state has accepted the string
    pub fn accepts(&self) -> bool {
    	!self.find_accepting_root().is_empty()
    }

    fn find_accepting_root(&self) -> Vec<SPPFNodeIndex> {
    	let mut roots = Vec::new();
		for ix in self.sppf.node_indices() {
			let node = self.sppf.node_weight(ix).unwrap();
			let has_parents = self.sppf.neighbors_directed(ix, Incoming).next().is_some();
			match node {
				SPPFNode::Intermediate { slot, left, right, .. } if !has_parents && slot.dot == slot.rule.len()+1 && left == &0 && right == &(self.input.len()) => roots.push(ix),
				_ => {}
			}
		}
    	roots
    }

    fn crop_sppf(&mut self) -> Result<(), ()> {
    	let roots = self.find_accepting_root();
    	if !roots.is_empty() {
    		let distances = roots.into_iter().flat_map(|x| petgraph::algo::dijkstra(&self.sppf, x, None, |_| 1).into_keys());
    		let reachable: HashSet<SPPFNodeIndex> = distances.collect();
		    self.sppf.retain_nodes(|_, x| reachable.contains(&x));
		    Ok(())
    	} else {
    		Err(())
    	}
    }

    fn _find_leafs(&self) -> Vec<SPPFNodeIndex> {
    	let mut leafs = Vec::new();
    	for ix in self.sppf.node_indices() {
			let has_children = self.sppf.neighbors_directed(ix, Outgoing).next().is_some();
			if !has_children {
				leafs.push(ix)
			}
    	}
    	leafs
    }

    // fn calc_probabilities(&mut self) {
    // 	let roots = self.find_accepting_root();
    // 	for root in roots {
    // 		self.calc_probability(root);
    // 	}
    	
    // }

    // fn _calc_probabilities(&mut self, nodes: Vec<SPPFNodeIndex>) {
    // 	for ix in nodes {
    // 		let mut parents = self.sppf.neighbors_directed(ix, Incoming);
    // 		match self.sppf.node_weight(ix) {
	// 	        Some(SPPFNode::Dummy) | None => {}
	// 	        Some(SPPFNode::Symbol { .. }) | Some(SPPFNode::Intermediate { .. }) => self._calc_probabilities(parents.collect(), cur_prob),
	// 	        Some(SPPFNode::Packed { slot, context, .. }) => {
	// 	        	self.gss_pointer = *context;
	// 	        	if let Some(prob) = slot.get_probability(self) {
	// 		    		let mut detached_parents = parents.detach();
	// 		    		cur_prob *= prob;
	// 		    		while let Some(parent) = detached_parents.next_node(&self.sppf) {
	// 		    			self.sppf.update_edge(parent, ix, Some(cur_prob));
	// 		    		}
	// 		    		parents = self.sppf.neighbors_directed(ix, Incoming);
	// 		    	}
	// 		    	self._calc_probabilities(parents.collect(), cur_prob)
	// 	        },
	// 	    }
    // 	}
    // }

    // fn calc_probability(&mut self, ix: SPPFNodeIndex) -> (f32, bool) {
    // 	let children = self.sppf.neighbors_directed(ix, Outgoing);
    // 	match self.sppf.node_weight(ix) {
    // 		Some(SPPFNode::Dummy) | None | Some(SPPFNode::Symbol { .. }) => (1.0, false),
    // 		Some(SPPFNode::Intermediate { .. }) => {
    			
    // 		},
    // 		Some(SPPFNode::Packed { slot, context, .. }) => {
    // 			self.gss_pointer = *context;
    // 			let sub_prob = self._calc_probabilities(children).into_iter().fold(1.0, |acc, (x, _)| acc * x);
    // 			if let Some(prob) = slot.get_probability(self) {
    // 				(sub_prob *= prob, true)
    // 			} else {
    // 				(sub_prob, false)
    // 			}
    // 		}
    // 	}
    // }
}