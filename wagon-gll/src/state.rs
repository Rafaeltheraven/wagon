use std::{collections::{HashSet, HashMap}, rc::Rc, format, usize};

use indexmap::IndexSet;
use petgraph::{Direction::Outgoing, prelude::EdgeIndex};
use regex_automata::dfa::Automaton;

use crate::{label::RegexTerminal, value::Value, AttributeKey, AttributeMap, GLLResult, ReturnMap};
use crate::{GLLImplementationError, ImplementationResult, GLLError};
use crate::{gss::{GSS, GSSNodeIndex, GSSNode}, sppf::{SPPF, SPPFNodeIndex, SPPFNode}, descriptor::Descriptor, GrammarSlot, ParseResult, GLLParseError, Terminal, Ident, ROOT_UUID, GLLBlockLabel};

/// A map from a uuid to a specific [`GLLBlockLabel`].
pub type LabelMap<'a> = HashMap<&'a str, GLLBlockLabel<'a>>;
/// A map representing a rule and the constituent [`Ident`]s of that rule. 
pub type RuleMap<'a> = HashMap<&'a str, Rc<Vec<Ident>>>;
/// A map from a regular expression to the [`RegexTerminal`] that it represents.
pub type RegexMap<'a> = HashMap<&'a str, Rc<RegexTerminal<'a>>>;

/// The state object for the GLL parse process.
///
/// This object handles the bulk of the GLL parsing. It runs the code for the labels as needed, keeps track of
/// the [`GSS`] and the [`SPPF`], holds the common methods etc.
///
/// # Example
/// ```
/// # use std::collections::HashMap;
/// use wagon_gll::{GLLState, ParseResult, ROOT_UUID, Label, GLLBlockLabel, value::Value, LabelMap, RuleMap, RegexMap, ImplementationResult, GLLResult};
/// use wagon_ident::Ident;
/// use std::rc::Rc;
/// #[derive(Debug)]
/// struct Root;
/// impl<'a> Label<'a> for Root {
///#    fn first_set(&self, state: &wagon_gll::GLLState<'a>) -> ImplementationResult<'a, Vec<(Vec<wagon_gll::GLLBlockLabel<'a>>, Option<wagon_gll::Terminal<'a>>)>> {
///#        Ok(vec![(vec![state.get_label_by_uuid(ROOT_UUID)?], None)])
///#    }
///#    fn is_eps(&self) -> bool {
///#        false
///#    }
///#    fn uuid(&self) -> &str {
///#        ROOT_UUID
///#    }
///#    fn to_string(&self) -> &str {
///#        ROOT_UUID
///#    }
///#    fn str_parts(&self) -> Vec<&str> {
///#        vec![ROOT_UUID]
///#    }
///#    fn code(&self, _: &mut wagon_gll::GLLState<'a>) -> GLLResult<'a, ()> {
///#        unreachable!("This should never be called");
///#    }
///#    fn attr_rep_map(&self) -> (Vec<&str>, Vec<&str>) { 
///#        (Vec::new(), Vec::new())
///#    }
///#    fn _weight(&self, _state: &wagon_gll::GLLState<'a>) -> Option<ImplementationResult<'a, Value<'a>>> {
///#        unreachable!("This should never be called");
///#    }
/// }
///# fn main() -> GLLResult<'static, ()> {
///     let mut l_map: LabelMap = HashMap::new();
///     let mut r_map: RuleMap = HashMap::new();
///     let mut regex_map: RegexMap = HashMap::new();
///     let root_label = Rc::new(Root{});
///     let root_rule = Rc::new(vec![]);
///     l_map.insert(ROOT_UUID, root_label);
///     r_map.insert(ROOT_UUID, root_rule);
///     let input = "".as_bytes();
///     let mut state = GLLState::init(input, l_map, r_map, regex_map)?;
///     state.main();
///#    Ok(())
///# }
/// ```
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
    label_map: LabelMap<'a>,
    rule_map: RuleMap<'a>,
    regex_map: RegexMap<'a>,
    /// All the errors
    pub errors: Vec<GLLError<'a>>
}

impl<'a> GLLState<'a> {
    /// Initialize the state.
    ///
    /// Takes the input data as a byte-array. As well as a mapping of specific [`Label::uuid`](`crate::Label::uuid`) to the associated label and another mapping of a uuid to a specific rule. 
    ///
    /// # Errors
    /// Returns [`GLLImplementationError::MissingRoot`] if no data was found in the `label_map` or `rule_map` for [`ROOT_UUID`]. 
    pub fn init(input: &'a [u8], label_map: LabelMap<'a>, rule_map: RuleMap<'a>, regex_map: RegexMap<'a>) -> ImplementationResult<'a, Self> {
        let mut sppf = SPPF::default();
        let mut gss = GSS::new();
        let mut sppf_map = HashMap::new();
        let mut gss_map = HashMap::new();
        let root_slot = Rc::new(GrammarSlot::new(label_map.get(ROOT_UUID).ok_or(GLLImplementationError::MissingRoot)?.clone(), rule_map.get(ROOT_UUID).ok_or(GLLImplementationError::MissingRoot)?.clone(), 0, 0, ROOT_UUID));
        let gss_root_node = Rc::new(GSSNode::new(root_slot.clone(), 0, Vec::default()));
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
            todo: IndexSet::default(), 
            visited: HashSet::default(), 
            pop: HashMap::default(), 
            gss_map, 
            sppf_map,
            rule_map,
            label_map,
            regex_map,
            errors: Vec::default(),
        };
        state.add(root_slot, gss_root, 0, sppf_root, gss_root);
        Ok(state)
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
    ///
    /// # Errors
    /// Returns a [`GLLParseError`] if something unexpected happens.
    pub fn create(&mut self, slot: &Rc<GrammarSlot<'a>>, args: AttributeMap<'a>) -> ImplementationResult<'a, GSSNodeIndex> {
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
                    let y = self.get_node_p(slot.clone(), self.sppf_pointer, *sppf_node, v)?;
                    self.add(
                        slot.clone(), 
                        self.gss_pointer, 
                        self.get_sppf_node(*sppf_node)?.right_extend()?, 
                        y,
                        v
                    );
                }
            }
            self.pop = pop;
        }
        Ok(v)
    }

    /// Try finding a packed node that is a child of `parent` and matches `ref_slot` and `i`.
    fn get_packed_node(&self, parent: SPPFNodeIndex, ref_slot: &Rc<GrammarSlot<'a>>, i: usize) -> Option<SPPFNodeIndex> {
        for child in self.sppf.neighbors_directed(parent, Outgoing) {
            match self.sppf.node_weight(child) {
                Some(SPPFNode::Packed { slot, split, .. }) if slot == ref_slot && *split == i => return Some(child),
                _ => {} 
            }
        }
        None
    }

    /// Find or create an [`SPPFNode::Packed`].
    ///
    /// This is `get_node_p` from the original paper. Differently from that paper, this also takes a `context_pointer`, which tells the packed node we are
    /// retrieving/creating where it can find it's context.
    ///
    /// # Errors
    /// Returns an error either because something is inexplicably missing in one of the state datastructures, or because the weight evaluation failed.
    pub fn get_node_p(&mut self, slot: Rc<GrammarSlot<'a>>, left: SPPFNodeIndex, right: SPPFNodeIndex, context_pointer: GSSNodeIndex) -> ImplementationResult<'a, SPPFNodeIndex> {
        if self.is_special_slot(&slot)? {
            Ok(right)
        } else {
            let left_node = self.get_sppf_node(left)?;
            let right_node = self.get_sppf_node(right)?;
            let j =  right_node.right_extend()?;
            let (t, weight) = if slot.is_last(self) {
                let new_slot = Rc::new(GrammarSlot { label: slot.label.clone(), rule: slot.rule.clone(), dot: slot.rule.len()+1, pos: 0, uuid: slot.uuid});
                let weight = self.get_label(&slot.rule[0])._weight(self);
                (new_slot, weight)
            } else {
                (slot.clone(), None)
            };
            if matches!(left_node, SPPFNode::Dummy) {
                let i = right_node.left_extend()?;
                let node = self.find_or_create_sppf_intermediate(&t, i, j, context_pointer)?;
                if self.get_packed_node(node, &slot, i).is_none() {
                    let packed = SPPFNode::Packed { slot, split: i, context: self.gss_pointer };
                    let ix = self.sppf.add_node(packed);
                    self.sppf.add_edge(ix, right, None);
                    self.sppf.add_edge(node, ix, weight.transpose()?);
                }
                Ok(node)
            } else {
                let (i, k) = (left_node.left_extend()?, left_node.right_extend()?);
                let node = self.find_or_create_sppf_intermediate(&t, i, j, context_pointer)?;
                if self.get_packed_node(node, &slot, k).is_none() {
                    let packed = SPPFNode::Packed { slot, split: k, context: self.gss_pointer };
                    let ix = self.sppf.add_node(packed);
                    self.sppf.add_edge(ix, left, None);
                    self.sppf.add_edge(ix, right, None);
                    self.sppf.add_edge(node, ix, weight.transpose()?);
                }
                Ok(node)
            }
        }
    }

    /// Find or create an [`SPPFNode::Symbol`].
    ///
    /// `get_node_t` from the original paper.
    pub fn get_node_t(&mut self, terminal: &'a [u8], left: usize, right: usize) -> SPPFNodeIndex {
        self.find_or_create_sppf_symbol(terminal, left, right)
    }

    /// Get the [`GSSNode`] `self.gss_pointer` is currently pointing to.
    ///
    /// # Errors
    /// Returns [`GLLImplementationError::MissingGSSNode`] if for some inexplicable reason the node does not exist.
    pub fn get_current_gss_node(&self) -> ImplementationResult<'a, &Rc<GSSNode<'a>>> {
        self.get_gss_node(self.gss_pointer)
    }

    /// Get the [`SPPFNode`] `self.sppf_pointer` is currently pointing to.
    ///
    /// # Errors
    /// Returns [`GLLImplementationError::MissingSPPFNode`] if for some inexplicable reason the node does not exist.
    pub fn get_current_sppf_node(&self) -> ImplementationResult<'a, &SPPFNode<'a>> {
        self.get_sppf_node(self.sppf_pointer)
    }

    fn get_sppf_node(&self, i: SPPFNodeIndex) -> ImplementationResult<'a, &SPPFNode<'a>> {
        self.sppf.node_weight(i).ok_or_else(|| GLLImplementationError::MissingSPPFNode(i))
    }

    fn get_sppf_node_mut(&mut self, i: SPPFNodeIndex) -> ImplementationResult<'a, &mut SPPFNode<'a>> {
        self.sppf.node_weight_mut(i).ok_or_else(|| GLLImplementationError::MissingSPPFNode(i))
    }

    fn get_gss_node(&self, i: GSSNodeIndex) -> ImplementationResult<'a, &Rc<GSSNode<'a>>> {
        self.gss.node_weight(i).ok_or_else(|| GLLImplementationError::MissingGSSNode(i))
    }

    fn get_gss_edge_endpoints(&self, i: EdgeIndex) -> ImplementationResult<'a, (GSSNodeIndex, GSSNodeIndex)> {
        self.gss.edge_endpoints(i).ok_or_else(|| GLLImplementationError::MissingGSSEdge(i))
    }

    fn get_gss_edge_weight(&self, i: EdgeIndex) -> ImplementationResult<'a, &SPPFNodeIndex> {
        self.gss.edge_weight(i).ok_or_else(|| GLLImplementationError::MissingGSSEdge(i))
    }

    fn find_or_create_sppf_symbol(&mut self, terminal: &'a [u8], left: usize, right: usize) -> SPPFNodeIndex {
        let candidate = SPPFNode::Symbol { terminal, left, right };
        self.find_or_create_sppf(candidate)
    }

    fn find_or_create_sppf_intermediate(&mut self, slot: &Rc<GrammarSlot<'a>>, left: usize, right: usize, context_pointer: GSSNodeIndex) -> ImplementationResult<'a, SPPFNodeIndex> {
        let candidate = SPPFNode::Intermediate { 
            slot: slot.clone(), 
            left, 
            right, 
            ret: Vec::default(), 
            context: self.get_gss_node(context_pointer)?.clone(),
        };
        Ok(self.find_or_create_sppf(candidate))
    }

    /// Creates/Returns the index of an [`SPPFNode`].
    ///
    /// If a vertex with the exact same data as `candidate` already exists in the SPPF, we return that vertex. 
    /// Otherwise, we create a new vertex with `candidate` as the data and return that.
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
    pub fn add(&mut self, slot: Rc<GrammarSlot<'a>>, g: GSSNodeIndex, i: usize, s: SPPFNodeIndex, context_pointer: GSSNodeIndex) {
        let d = Descriptor::new(slot, g, i, s, context_pointer);
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
    ///
    /// # Errors
    /// Returns an error for the same reasons as [`GLLState::get_node_p`].
    pub fn pop(&mut self, ret_vals: &ReturnMap<'a>) -> ImplementationResult<'a, ()> {
        if self.gss_pointer != self.gss_root {
            if let Some(map) = self.pop.get_mut(&self.gss_pointer) {
                map.push(self.sppf_pointer); 
            } else {
                let map = vec![self.sppf_pointer];
                self.pop.insert(self.gss_pointer, map);
            }
            let slot = self.get_current_gss_node()?.slot.clone();
            let mut detached = self.gss.neighbors_directed(self.gss_pointer, Outgoing).detach();
            while let Some(edge) = detached.next_edge(&self.gss) {
                let v = self.get_gss_edge_endpoints(edge)?.1;
                let y = self.get_node_p(slot.clone(), *self.get_gss_edge_weight(edge)?, self.sppf_pointer, self.gss_pointer)?;
                self.get_sppf_node_mut(y)?.add_ret_vals(&mut ret_vals.clone())?;
                self.add(slot.clone(), v, self.input_pointer, y, self.gss_pointer);
            }
        }
        Ok(())
    }

    // TODO: Make this cached
    fn __next(bytes: Terminal<'a>, start_pointer: usize, input: &'a [u8]) -> ParseResult<'a, usize> {
        let mut pointer = start_pointer;
        let input_len = input.len();
        while pointer < input_len && input[pointer].is_ascii_whitespace() { // left trim the input
            pointer += 1;
        }
        for expected in bytes {
            if pointer >= input_len {
                return Err(GLLParseError::TooLong { pointer, offender: bytes })
            }
            let check = input[pointer];
            if check != *expected && !check.is_ascii_whitespace() {
                return Err(GLLParseError::UnexpectedByte { pointer, expected: *expected, offender: check })
            }
            pointer += 1;
        }
        Ok(pointer)
    }

    // This one only exists to work around the borrow checker.
    fn _next(&self, bytes: Terminal<'a>) -> ParseResult<'a, usize> {
        Self::__next(bytes, self.input_pointer, self.input)
    }

    /// Consume the following bytes from the input string. 
    ///
    /// If the bytes we just consumed are not the expected bytes, we return an error.
    ///
    /// If no error is returned, we move `self.input_pointer` forward as much as needed.
    ///
    /// # Errors
    /// Returns either a [`GLLParseError::TooLong`] or [`GLLParseError::UnexpectedByte`] depending on the expected bytes and state of the input.
    pub fn next(&mut self, bytes: Terminal<'a>) -> ParseResult<'a, ()> {
        let pointer = self._next(bytes)?;
        self.input_pointer = pointer;
        Ok(())
    }

    /// Check if the following bytes **can** be consumed, but do not consume them.
    pub fn has_next(&mut self, bytes: Terminal<'a>) -> bool {
        self._next(bytes).is_ok()
    }

    #[must_use]
    fn _next_regex(regex: &RegexTerminal<'a>, start_pointer: usize, input: &[u8]) -> Option<usize> {
        let current_byte = &input[start_pointer..=start_pointer];
        let Ok(mut curr_state) = regex.automaton.start_state_forward(&current_byte.into()) else { // Check if we have a valid start state.
            return None
        };
        let input_len = input.len();
        let mut i = 0;
        let mut last_match = None;
        while !regex.automaton.is_dead_state(curr_state) && !regex.automaton.is_quit_state(curr_state)  { // Until we encounter a dead state or a quit state
            let pointer = start_pointer + i; // Increment the pointer.
            if pointer >= input_len { // If our pointer exceeds the input, stop looping.
                break;
            }
            let byte = input[pointer];
            curr_state = regex.automaton.next_state(curr_state, byte); // Move the automaton forward based on the current byte.
            if regex.automaton.is_match_state(curr_state) { // If this is a potential match, store it.
                last_match = Some(i); // We do not break as this is a greedy algorithm and we may find a longer match.
            }
            i += 1;
        }
        if regex.automaton.is_quit_state(curr_state) || regex.automaton.is_dead_state(curr_state) { // If we stopped the loop because we reached a dead or quit state.
            last_match // Return the last found match
        } else { // If we stopped the loop because the pointer exceeded the input
            let state = regex.automaton.next_eoi_state(curr_state); // Move the automaton forward by 1 step (because of library reasons).
            if regex.automaton.is_match_state(state) { // Check if this last state is accepting.
                Some(i)
            } else {
                last_match // If it is not, return the last found match.
            }
        }
    }

    /// Check if the given regex is accepting.
    ///
    /// If it is, we move the pointer forwards and return the accepted bytes. If no bytes are accepted, we return [`None`].
    ///
    /// # Errors
    /// Returns an error if the regex completely fails to build.
    pub fn next_regex(&mut self, pattern: &'a str) -> GLLResult<'a, Option<Terminal<'a>>> {
        let regex = self.get_regex_automaton(pattern)?;
        if let Some(j) = Self::_next_regex(&regex, self.input_pointer, self.input) {
            let result = &self.input[self.input_pointer..self.input_pointer + j];
            self.input_pointer += j + 1;
            Ok(Some(result))
        } else {
            Ok(None)
        }
    }

    /// Check if the following pattern **can** be matched, but do not consume the resulting bytes.
    ///
    /// # Errors
    /// Returns an error if the regex completely fails to build.
    pub fn has_regex(&self, pattern: &'a str) -> GLLResult<'a, bool> {
        let regex = self.get_regex_automaton(pattern)?;
        Ok(Self::_next_regex(&regex, self.input_pointer, self.input).is_some())
    }

    /// Get the bytes matched by the pattern based on where the current input pointer is, but do not consume these bytes.
    ///
    /// Similar to [`GLLState::next_regex`] but without consuming bytes.
    ///
    /// # Errors
    /// Returns an error if the regex completely fails to build.
    pub fn regex_bytes(&self, pattern: &'a str) -> GLLResult<'a, Option<Terminal<'a>>> {
        let regex = self.get_regex_automaton(pattern)?;
        Ok(Self::_next_regex(&regex, self.input_pointer, self.input).map(|j| &self.input[self.input_pointer..self.input_pointer + j]))
    }

    /// Get the current input byte for the state
    #[must_use]
    pub fn current_byte(&self) -> &[u8] {
        &self.input[self.input_pointer..=self.input_pointer]
    }

    /// Check if, given the current state, the [`Label`](crate::Label)'s first-follow set is accepting.
    ///
    /// # Errors
    /// Returns an error if something goes wrong during the first checking.
    pub fn test_next(&mut self, label: &GLLBlockLabel<'a>) -> GLLResult<'a, bool> {
        label.first(self)
    }

    /// Get a specific rule by its uuid.
    ///
    /// # Errors
    /// Returns a [`GLLImplementationError::UnknownRule`] if the rule does not exist.
    pub fn get_rule(&self, ident: &'a str) -> ImplementationResult<'a, Rc<Vec<Ident>>> {
        Ok(self.rule_map.get(ident).ok_or_else(|| GLLImplementationError::UnknownRule(ident))?.clone())
    }

    /// Get a specific [`Label`](crate::Label) as identified by the given [`Ident`].
    #[must_use] 
    pub fn get_label(&self, ident: &Ident) -> GLLBlockLabel<'a> {
        let raw_string = ident.extract_string();
        self.label_map.get(raw_string).map_or_else(|| todo!(), std::clone::Clone::clone)
    }

    /// Get a specific [`Label`](crate::Label) by its uuid.
    ///
    /// # Errors
    /// Returns a [`GLLImplementationError::UnknownLabel`] if the label can not be found.
    pub fn get_label_by_uuid(&self, label: &'a str) -> ImplementationResult<'a, GLLBlockLabel<'a>> {
        Ok(self.label_map.get(label).ok_or_else(|| GLLImplementationError::UnknownLabel(label))?.clone())
    }

    /// Get a specific [`RegexTerminal`] by its pattern.
    ///
    /// This differs from [`Self::get_label_by_uuid`] in that it specifically returns a [`RegexTerminal`], as opposed to some trait object.
    ///
    /// # Errors
    /// Returns a [`GLLImplementationError::UnknownLabel`] if the dfa can not be found.
    pub fn get_regex_automaton(&self, regex: &'a str) -> ImplementationResult<'a, Rc<RegexTerminal<'a>>> {
        Ok(self.regex_map.get(regex).ok_or_else(|| GLLImplementationError::UnknownLabel(regex))?.clone())
    }

    /// Get an attribute from the node pointed at by `self.gss_pointer`.
    ///
    /// # Errors
    /// Returns a [`GLLImplementationError::MissingAttribute`] if the `i`th attribute was never passed.
    pub fn get_attribute(&self, i: AttributeKey) -> ImplementationResult<'a, &Value<'a>> {
        let node = self.get_gss_node(self.gss_pointer)?;
        node.get_attribute(i).ok_or_else(|| GLLImplementationError::MissingAttribute(i, node.clone()))
    }

    /// Get an attribute from the node pointed at by `self.context_pointer`.
    ///
    /// # Errors
    /// Returns a [`GLLImplementationError::MissingContext`] if the `i`th attribute is not in context.
    pub fn restore_attribute(&self, i: AttributeKey) -> ImplementationResult<'a, &Value<'a>> {
        let node = self.get_gss_node(self.context_pointer)?;
        node.get_attribute(i).ok_or_else(|| GLLImplementationError::MissingContext(i, node.clone()))
    }

    // pub(crate) fn get_attribute_at_gss_node(&self, pointer: GSSNodeIndex, i: AttributeKey) -> ParseResult<'a, Option<&Value<'a>>> {
    //  Ok(self.get_gss_node(pointer)?.get_attribute(i))
    // }

    /// Get an attribute from the return arguments at the node currently pointed to by `self.sppf_pointer`.
    ///
    /// # Errors
    /// Returns a [`GLLImplementationError::MissingSPPFNode`] if [`GLLState::sppf_pointer`] inexplicably points at a non-existant SPPF node.
    pub fn get_ret_val(&self, i: AttributeKey) -> ImplementationResult<'a, Option<&Value<'a>>> {
        self.get_sppf_node(self.sppf_pointer)?.get_ret_val(i)
    }

    /// Check if we have a special case slot.
    ///
    /// This concept comes from the OOGLL paper and is required in [`Self::get_node_p`] to instantly return the `right`.
    ///
    /// A special slot is defined as any slot `S -> α•β` where |α| == 1 && α is non-terminal or a non-nullable terminal && |β| != 0. 
    fn is_special_slot(&self, slot: &GrammarSlot<'a>) -> ImplementationResult<'a, bool> {
        Ok(if slot.dot == 1 && slot.pos == 0 && !slot.is_last(self) {
            match slot.rule.first() {
                Some(r) => {
                    let a = self.get_label(r);
                    a.str_parts().len() == 1 && (a.is_terminal() || !(a.is_nullable(self)?))
                },
                None => false
            }
        } else {
            false
        })
    }

    fn get_current_label_slot(&self, slot: &GrammarSlot<'a>) -> ImplementationResult<'a, GLLBlockLabel<'a>> {
        Ok(self.get_label(slot.rule.get(slot.dot).ok_or_else(|| GLLImplementationError::CompletedSlot(slot.to_string(self)))?))
    }

    /// The goto function of the OOGLL paper.
    ///
    /// Calls [`crate::label::Label::code`]. If an error occurs for any reason, we store it in the `errors` vector.
    fn goto(&mut self, slot: &GrammarSlot<'a>) {
        match self.get_current_label_slot(slot) {
            Ok(label) => {
                if let Err(e) = label.code(self) {
                    self.errors.push(e);
                }
            },
            Err(e) => self.errors.push(e.into())
        }
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
    ///
    /// # Errors
    /// Returns a [`GLLImplementationError::Utf8Error`] if there is non-utf8 data anywhere in the SPPF.
    pub fn print_sppf_dot(&mut self, crop: bool) -> ImplementationResult<'a, String> {
        if crop {
            self.sppf.crop(self.find_roots_sppf());
        }
        self.sppf.to_dot(self)
    }

    /// Print current GSS graph in graphviz format
    #[must_use] 
    pub fn print_gss_dot(&self) -> String {
        format!("{:?}", petgraph::dot::Dot::new(&self.gss))
    }
    
    /// Checks whether the current parser state has accepted the string
    #[must_use] 
    pub fn accepts(&self) -> bool {
        !self.find_roots_sppf().is_empty()
    }

    fn find_roots_sppf(&self) -> Vec<SPPFNodeIndex> {
        self.sppf.find_accepting_roots(Some(self.input.len()))
    }
}