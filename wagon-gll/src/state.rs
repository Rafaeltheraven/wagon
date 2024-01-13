use std::{collections::{HashSet, HashMap}, rc::Rc, format};

use indexmap::IndexSet;
use petgraph::Direction::Outgoing;

use crate::{value::Value, AttributeMap, AttributeKey, ReturnMap};

use super::{gss::{GSS, GSSNodeIndex, GSSNode}, sppf::{SPPF, SPPFNodeIndex, SPPFNode}, descriptor::Descriptor, GrammarSlot, ParseResult, GLLParseError, Terminal, Ident, ROOT_UUID, GLLBlockLabel};

/// The state object for the GLL parse process.
///
/// This object handles the bulk of the GLL parsing. It runs the code for the labels as needed, keeps track of
/// the [`GSS`] and the [`SPPF`], holds the common methods etc.
///
/// # Example
/// ```
/// let l_map = HashMap::new();
/// let r_map = HashMap::new();
/// let input = "abc".as_bytes();
/// ...
/// let state = GLLState::init(input, l_map, r_map);
/// state.main();
/// state.accepts();
pub struct GLLState<'a> {
	// Main structures
	input: &'a [u8],
	gss: GSS<'a>,
	sppf: SPPF<'a>,
	// Pointers
	/// A pointer to where in the input we currently are.
	///
	/// `C_i` in the original paper.
	pub input_pointer: usize, //C_i
	/// A pointer to where in the GSS we currently are.
	///
	/// `C_u` in the original paper.
	pub gss_pointer: GSSNodeIndex, // C_u
	gss_root: GSSNodeIndex, // Points to <⊥, 0>
	context_pointer: GSSNodeIndex, // Points to where the current context is stored
	/// A pointer to where in the SPPF we currently are.
	///
	/// `C_n` in the original paper.
	pub sppf_pointer: SPPFNodeIndex, // C_n
	/// A simple pointer to $ for comparison purposes.
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
	/// Initialize the state.
	///
	/// Takes the input data as a byte-array. As well as a mapping of specific [`GLLBlockLabel::uuid`] to the associated label and another mapping of a uuid to a specific rule.
	pub fn init(input: &'a [u8], label_map: HashMap<&'a str, GLLBlockLabel<'a>>, rule_map: HashMap<&'a str, Rc<Vec<Ident>>>) -> Self {
		let mut sppf = SPPF::default();
		let mut gss = GSS::new();
		let mut sppf_map = HashMap::new();
		let mut gss_map = HashMap::new();
		let root_slot = Rc::new(GrammarSlot::new(label_map.get(ROOT_UUID).unwrap().clone(), rule_map.get(ROOT_UUID).unwrap().clone(), 0, 0, ROOT_UUID));
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

	/// Create a new GSS node.
	///
	/// This is the `create` method in the original paper. The arguments to that method are mapped as follows:
	/// * `L` => `slot`.
	/// * `u` => `self.gss_pointer`.
	/// * `i` => `self.input_pointer`.
	/// * `w` => `self.sppf_pointer`.
	///
	/// Differently from the paper, this method also takes a list of attributes that are passed along to the `GSS`.
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

	/// Find or create an [`SPPFNode::Packed`].
	///
	/// This is `get_node_p` from the original paper. Differently from that paper, this also takes a `context_pointer`, which tells the packed node we are
	/// retrieving/creating where it can find it's context.
	pub fn get_node_p(&mut self, slot: Rc<GrammarSlot<'a>>, left: SPPFNodeIndex, right: SPPFNodeIndex, context_pointer: GSSNodeIndex) -> SPPFNodeIndex {
		if self.is_special_slot(&slot) {
			right
		} else {
			let left_node = self.sppf.node_weight(left).unwrap();
			let right_node = self.sppf.node_weight(right).unwrap();
			let j =  right_node.right_extend().unwrap();
			let (t, weight) = if slot.is_last(self) {
				let new_slot = Rc::new(GrammarSlot { label: slot.label.clone(), rule: slot.rule.clone(), dot: slot.rule.len()+1, pos: 0, uuid: slot.uuid});
				let weight = self.get_label(&slot.rule[0])._weight(self);
				(new_slot, weight)
			} else {
				(slot.clone(), None)
			};
			if let SPPFNode::Dummy = left_node {
				let i = right_node.left_extend().unwrap();
				let node = self.find_or_create_sppf_intermediate(t.clone(), i, j, context_pointer);
				if self.get_packed_node(node, slot.clone(), i).is_none() {
					let packed = SPPFNode::Packed { slot, split: i, context: self.gss_pointer };
					let ix = self.sppf.add_node(packed);
					self.sppf.add_edge(ix, right, None);
					self.sppf.add_edge(node, ix, weight);
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
					self.sppf.add_edge(node, ix, weight);
				}
				node
			}
		}
	}

	/// Find or create an [`SPPFNode::Symbol`].
	///
	/// `get_node_t` from the original paper.
	pub fn get_node_t(&mut self, terminal: &'a [u8]) -> SPPFNodeIndex {
		let left = self.input_pointer;
		let right = left + terminal.len();
		self.find_or_create_sppf_symbol(terminal, left, right)
	}

	/// Get the [`GSSNode`] `self.gss_pointer` is currently pointing to.
	pub fn get_current_gss_node(&self) -> &Rc<GSSNode<'a>> {
		self.get_gss_node_unchecked(self.gss_pointer)
	}

	/// Get the [`SPPFNode`] `self.sppf_pointer` is currently pointing to.
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

	/// Add a new slot to the `self.visited` and `self.todo` sets.
	///
	/// `add` from the original paper.
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
	/// Pop context back after a non-terminal was parsed.
	///
	/// `pop` from the original paper. The arguments to that method are mapped as follows:
	/// * `u` => `self.gss_pointer`
	/// * `i` => `self.input_pointer`
	/// * `z` => `self.sppf_pointer`
	///
	/// Additionally, this method takes a list of attributes that are returned after the non-terminal was parsed.
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

	/// Consume the following bytes from the input string. 
	///
	/// If the bytes we just consumed are not the expected bytes, we return an error.
	///
	/// If no error is returned, we move `self.input_pointer` forward as much as needed.
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

	/// Check if the following bytes **can** be consumed, but do not consume them.
	pub fn has_next(&mut self, bytes: Terminal<'a>) -> bool {
		let curr = self.input_pointer;
		let ret = self.next(bytes).is_ok();
		self.input_pointer = curr;
		ret
	}

	/// Check if, given the current state, the [`Label`]'s first-follow set is accepting.
	pub fn test_next(&mut self, label: GLLBlockLabel<'a>) -> bool {
		label.first(self)
	}

	/// Get a specific rule by its uuid.
	pub fn get_rule(&self, ident: &str) -> Rc<Vec<Ident>> {
		self.rule_map.get(ident).unwrap_or_else(|| panic!("Issue unwrapping rule map. {} not in keyset", ident)).clone()
	}

	/// Get a specific [`Label`] as identified by the given [`Ident`].
	pub fn get_label(&self, ident: &Ident) -> GLLBlockLabel<'a> {
		let raw_string = ident.extract_string();
		if let Some(label) = self.label_map.get(raw_string) {
			label.clone()
		} else { // Must evaluate at runtime
			todo!()
		}
	}

	/// Get a specific [`Label`] by its uuid.
	pub fn get_label_by_uuid(&self, label: &str) -> GLLBlockLabel<'a> {
		self.label_map.get(label).unwrap().clone()
	}

	/// Get an attribute from the node pointed at by `self.gss_pointer`.
	pub fn get_attribute(&self, i: AttributeKey) -> &Value<'a> {
		self.get_attribute_at_gss_node(self.gss_pointer, i).expect("Not enough attributes passed to NT.")
	}

	/// Get an attribute from the node pointed at by `self.context_pointer`.
	pub fn restore_attribute(&self, i: AttributeKey) -> &Value<'a> {
		self.get_attribute_at_gss_node(self.context_pointer, i).expect("Error restoring context.")
	}

	pub(crate) fn get_attribute_at_gss_node(&self, pointer: GSSNodeIndex, i: AttributeKey) -> Option<&Value<'a>> {
		self.gss.node_weight(pointer).unwrap().get_attribute(i)
	}

	/// Get an attribute from the return arguments at the node currently pointed to by `self.sppf_pointer`.
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

	/// Run the parsing process.
	///
	/// Once this has finished running, we either completed parsing or ran into an error somewhere.
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
    	if crop {
    		self.sppf.crop(self.find_roots_sppf()).unwrap();
    	}
        self.sppf.to_dot(self)
    }

	/// Print current GSS graph in graphviz format
    pub fn print_gss_dot(&self) -> String {
        format!("{:?}", petgraph::dot::Dot::new(&self.gss))
    }
 	
 	/// Checks whether the current parser state has accepted the string
    pub fn accepts(&self) -> bool {
    	!self.find_roots_sppf().is_empty()
    }

    fn find_roots_sppf(&self) -> Vec<SPPFNodeIndex> {
    	self.sppf.find_accepting_roots(Some(self.input.len()))
    }
}