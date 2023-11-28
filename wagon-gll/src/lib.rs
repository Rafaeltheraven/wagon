use std::{hash::{Hash, Hasher}, fmt::{Debug}, rc::Rc, str::from_utf8, collections::{HashSet}, matches};

use self::{state::GLLState, value::Value};
use wagon_ident::Ident;

mod sppf;
pub mod state;
pub mod gss;
mod descriptor;
pub mod value;

pub type TerminalBit<'a> = &'a u8;
pub type Terminal<'a> = &'a[u8];

pub const ROOT_UUID: &str = "S'";

pub type GLLBlockLabel<'a> = Rc<dyn Label<'a>>;

pub type AttributeMap<'a> = Vec<Value<'a>>;
pub type ReturnMap<'a> = Vec<Option<Value<'a>>>;
pub type AttributeKey = usize;

pub trait Label<'a>: Debug {
	fn is_eps(&self) -> bool {
		false
	}
	fn first_set(&self, state: &GLLState<'a>) -> Vec<(Vec<GLLBlockLabel<'a>>, Option<Terminal<'a>>)>;
	fn code(&self, state: &mut GLLState<'a>);
	fn first(&self, state: &mut GLLState<'a>) -> bool {
		let fst = self.first_set(state);
		for (alt, fin) in fst {
			let mut check_fin = true;
			for sub in alt {
				if sub.first(state) {
					return true;
				} else if !sub.is_nullable(state, &mut HashSet::new()) {
					check_fin = false;
				    break;
				}
			}
			if check_fin {
				if let Some(last) = fin {
					return last.is_empty() || state.has_next(last)
				}
			}
		}
		false
	}
	fn is_terminal(&self) -> bool {
		false
	}
	fn is_nullable(&self, state: &GLLState<'a>, seen: &mut HashSet<Rc<str>>) -> bool {
		if self.is_eps() {
			true
		} else {
			let str_repr = self.uuid();
			if !seen.contains(str_repr) {
				seen.insert(str_repr.into());
				let fst = self.first_set(state);
				for (alt, _) in fst {
					if let Some(sub) = alt.into_iter().next() {
						if sub.is_nullable(state, seen) {
							return true
						}
					}
				}
			}
			false
		}
	}
	fn weight(&self, state: &GLLState<'a>) -> Value<'a>;
	fn str_parts(&self) -> Vec<&str>;
	fn to_string(&self) -> &str;
	fn uuid(&self) -> &str;
	fn attr_rep_map(&self) -> (Vec<&str>, Vec<&str>);
}

impl<'a> Label<'a> for Terminal<'a> {
    fn is_eps(&self) -> bool {
        self.is_empty()
    }

    fn first_set(&self, _: &GLLState<'a>) -> Vec<(Vec<GLLBlockLabel<'a>>, Option<Terminal<'a>>)> {
        vec![(Vec::new(), Some(*self))]
    }

    fn first(&self, state: &mut GLLState<'a>) -> bool {
        self.is_eps() || state.has_next(self)
    }

    fn code(&self, _: &mut GLLState<'a>) {
        unreachable!("Should never run this method on terminals");
    }

    fn is_terminal(&self) -> bool {
        true
    }

    fn to_string(&self) -> &str {
        from_utf8(self).unwrap()
    }

    fn is_nullable(&self, _: &GLLState<'a>, _: &mut HashSet<Rc<str>>) -> bool {
        self.is_eps()
    }

    fn uuid(&self) -> &str {
        self.to_string()
    }

	fn str_parts(&self) -> Vec<&str> { 
		vec![self.to_string()]
	}

	fn attr_rep_map(&self) -> (Vec<&str>, Vec<&str>) { 
		(Vec::new(), Vec::new())
	}

	fn weight(&self, _state: &GLLState<'a>) -> Value<'a> {
		unreachable!("Should never run this method on terminals");
	}
}

impl<'a> Hash for dyn Label<'a> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.uuid().hash(state);
    }
}

impl<'a> PartialEq for dyn Label<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.uuid() == other.uuid()
    }
}

impl<'a> Eq for dyn Label<'a>{}


#[derive(Debug)]
pub struct GrammarSlot<'a> {
	label: GLLBlockLabel<'a>,
	rule: Rc<Vec<Ident>>,
	dot: usize,
	pos: usize,
	uuid: &'a str,
	probabilistic: bool
}

impl<'a> Eq for GrammarSlot<'a> {}

impl<'a> PartialEq for GrammarSlot<'a> {
    fn eq(&self, other: &Self) -> bool {
    	if !self.is_probabilistic() && !other.is_probabilistic() && self.is_complete() && other.is_complete() { // Any completed alt for a rule is the same except when probabilistic
    		self.label.to_string() == other.label.to_string()
    	} else {
    		self.uuid == other.uuid && self.dot == other.dot && self.pos == other.pos
    	}
    }
}

impl<'a> Hash for GrammarSlot<'a> {
    fn hash<H: Hasher>(&self, state: &mut H) {
    	if !self.is_probabilistic() && self.is_complete() {
    		self.label.to_string().hash(state);
    	} else {
    		self.uuid.hash(state);
        	self.dot.hash(state);
        	self.pos.hash(state);
    	}
    }
}

impl<'a> GrammarSlot<'a> {
	pub fn new(label: GLLBlockLabel<'a>, rule: Rc<Vec<Ident>>, dot: usize, pos: usize, uuid: &'a str, probabilistic: bool) -> Self {
		Self {label, rule, dot, pos, uuid, probabilistic}
	}

	pub fn is_eps(&self) -> bool {
		self.is_empty()
	}

	pub fn is_last(&self, state: &GLLState<'a>) -> bool {
		self.dot == self.len() || (self.dot == self.len()-1 && self.curr_block(state).is_eps())
	}

	pub fn len(&self) -> usize {
		self.rule.len()
	}

	pub fn is_empty(&self) -> bool {
		self.len() == 0
	}

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
					}
					res.push_str(s);
				}
			} else {
				res.push_str(label.to_string());
			}
		}
		if self.is_last(state) {
			res.push('•');
		};
		res
	}

	fn curr_block(&self, state: &GLLState<'a>) -> GLLBlockLabel<'a> {
		state.get_label(&self.rule[self.dot])
	}

	fn is_complete(&self) -> bool {
		self.dot == self.len()+1
	}

	pub fn cmp(&self, other: &Self, state: &GLLState<'a>) -> std::cmp::Ordering {
		self.curr_block(state).weight(state).cmp(&other.curr_block(state).weight(state))
	}

	pub fn partial_cmp(&self, other: &Self, state: &GLLState<'a>) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other, state))
    }

    pub fn is_probabilistic(&self) -> bool {
    	self.probabilistic
    }

    pub fn yank_probability(&self, state: &GLLState<'a>) -> f32 {
    	match self.curr_block(state).weight(state) {
	        Value::Float(f) => *f,
	        other => panic!("Associated slot has non-float value {:?}", other)
	    }
    }

    pub fn get_probability(&self, state: &GLLState<'a>) -> Option<f32> {
    	let ident = &self.rule[0];
    	match state.get_label(ident).weight(state) {
	        Value::Float(f) => Some(*f),
	        _ => None
	    }
    }
}

pub type ParseResult<'a, T> = Result<T, GLLParseError<'a>>;

#[derive(Debug)]
pub enum GLLParseError<'a> {
	UnexpectedByte {
		pointer: usize,
		expected: u8,
		offender: u8
	},
	TooLong {
		pointer: usize,
		offender: Terminal<'a>
	}
}