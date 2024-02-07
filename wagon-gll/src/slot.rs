use crate::ParseResult;
use crate::GLLParseError;
use crate::ValueError;
use crate::InnerValueError;
use crate::GLLState;
use crate::GLLBlockLabel;
use crate::Rc;
use crate::Ident;
use crate::Hash;
use crate::Hasher;

#[derive(Debug)]
/// A `GrammarSlot` as defined by the original paper.
pub struct GrammarSlot<'a> {
	/// The non-terminal that this slot represents.
	pub label: GLLBlockLabel<'a>,
	/// The specific alternative of that non-terminal that this slot represents.
	pub rule: Rc<Vec<Ident>>,
	/// The location of the `•` inside this rule.
	pub dot: usize,
	/// The position of the pointer inside this rule (may be seperate from the dot)
	pub pos: usize,
	/// A unique identifier for this slot.
	pub uuid: &'a str
}

impl<'a> Eq for GrammarSlot<'a> {}

impl<'a> PartialEq for GrammarSlot<'a> {
    fn eq(&self, other: &Self) -> bool {
    	if self.is_complete() && other.is_complete() { // Any completed alt for a rule is the same except when probabilistic
    		self.label.to_string() == other.label.to_string()
    	} else {
    		self.uuid == other.uuid && self.dot == other.dot && self.pos == other.pos
    	}
    }
}

impl<'a> Hash for GrammarSlot<'a> {
    fn hash<H: Hasher>(&self, state: &mut H) {
    	if self.is_complete() {
    		self.label.to_string().hash(state);
    	} else {
    		self.uuid.hash(state);
        	self.dot.hash(state);
        	self.pos.hash(state);
    	}
    }
}

impl<'a> GrammarSlot<'a> {
	/// Construct a new slot.
	pub fn new(label: GLLBlockLabel<'a>, rule: Rc<Vec<Ident>>, dot: usize, pos: usize, uuid: &'a str) -> Self {
		Self {label, rule, dot, pos, uuid}
	}

	/// Is this slot just epsilon?
	#[must_use] 
	pub fn is_eps(&self) -> bool {
		self.is_empty()
	}

	/// Have we completely consumed this grammar slot?
	///
	/// This is defined as either the dot being at the end (`S -> A•`) or right before the end and the next label is epsilon (`S -> A•ε`) 
	#[must_use] 
	pub fn is_last(&self, state: &GLLState<'a>) -> bool {
		self.dot == self.len() || (self.dot == self.len()-1 && self.curr_block(state).is_eps())
	}

	/// The length of the rule
	#[must_use] 
	pub fn len(&self) -> usize {
		self.rule.len()
	}

	/// Whether this is an empty rule
	#[must_use] 
	pub fn is_empty(&self) -> bool {
		self.len() == 0
	}

	/// A string representation of the grammar slot.
	///
	/// For example, `S -> A•B` if `self.label = S`, `self.rule = [A, B]` and `self.dot = 1`. 
	#[must_use] 
	pub fn to_string(&self, state: &GLLState<'a>) -> String {
		let mut res = String::new();
		res.push_str(self.label.to_string());
		if self.dot == self.len() + 1 {
			return res
		}
		res.push_str(" -> ");
		for (i, r) in self.rule.iter().enumerate() {
			let label = state.get_label(r);
			if i == self.dot {
				let parts = label.str_parts();
				for (j, s) in parts.iter().enumerate() {
					if j == self.pos {
						res.push('•');
						res.push(' ');
					}
					res.push_str(s);
					res.push(' ');
				}
			} else {
				res.push_str(label.to_string());
				res.push(' ');
			}
		}
		if self.is_last(state) {
			res.push('•');
		};
		res
	}

	/// Get the label for the part of the rule we are currently at as defined by the dot.
	fn curr_block(&self, state: &GLLState<'a>) -> GLLBlockLabel<'a> {
		state.get_label(&self.rule[self.dot])
	}

	/// Have we completely parsed this rule?
	///
	/// This is defined as the dot being 1 higher than the length of the rule.
	fn is_complete(&self) -> bool {
		self.dot == self.len()+1
	}

	/// Compare the weight of this slot with the weight of another.
	///
	/// # Errors
	/// Returns a wrapped [`ValueError::ComparisonError`](`InnerValueError::ComparisonError`) if the comparison is not possible.
	pub fn partial_cmp(&self, other: &Self, state: &GLLState<'a>) -> ParseResult<'a, std::cmp::Ordering> {
		let left_weight = self.curr_block(state).weight(state)?;
		let right_weight = other.curr_block(state).weight(state)?;
		left_weight.partial_cmp(&right_weight).map_or_else(|| Err(GLLParseError::ValueError(ValueError::ValueError(InnerValueError::ComparisonError(left_weight, right_weight)))), Ok)
	}
}