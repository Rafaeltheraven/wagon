
use std::{hash::{Hash, Hasher}, fmt::{Debug}, rc::Rc, str::from_utf8, collections::HashSet};

use self::{state::GLLState, ident::Ident};

mod sppf;
mod state;
mod gss;
mod descriptor;
pub(crate) mod ident;

type TerminalBit<'a> = &'a u8;
type Terminal<'a> = &'a[u8];

const ROOT_UUID: &str = "S'";

pub(crate) trait Label<'a>: Debug {
	fn is_eps(&self) -> bool {
		false
	}
	fn first_set(&'a self, state: &GLLState<'a>) -> Vec<(Vec<&'a dyn Label>, Option<TerminalBit>)>;
	fn code(&self, state: &mut GLLState<'a>);
	fn first(&'a self, token: TerminalBit, state: &GLLState<'a>) -> bool {
		let fst = self.first_set(state);
		for (alt, fin) in fst {
			let mut check_fin = true;
			for sub in alt {
				if sub.first(token, state) {
					return true;
				} else if !sub.is_nullable(state, &mut HashSet::new()) {
					check_fin = false;
				    break;
				}
			}
			if check_fin {
				if let Some(last) = fin {
					return token == last
				}
			}
		}
		false
	}
	fn is_terminal(&self) -> bool {
		false
	}
	fn is_nullable(&'a self, state: &GLLState<'a>, seen: &mut HashSet<&'a str>) -> bool {
		if self.is_eps() {
			true
		} else {
			let str_repr = self.to_string();
			if !seen.contains(str_repr) {
				seen.insert(str_repr);
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
	fn to_string(&self) -> &str;
}

impl<'a> Label<'a> for Terminal<'a> {
    fn is_eps(&self) -> bool {
        self.is_empty()
    }

    fn first_set(&self, _: &GLLState<'a>) -> Vec<(Vec<&dyn Label>, Option<TerminalBit>)> {
    	let bit = if !self.is_eps() {
    		&self[0]
    	} else {
    		&0 // null byte should never be a char
    	};
        vec![(Vec::new(), Some(bit))]
    }

    fn first(&'a self, token: TerminalBit, _: &GLLState<'a>) -> bool {
        let bit = if !self.is_eps() {
    		&self[0]
    	} else {
    		&0 // null byte should never be a char
    	};
    	token == bit
    }

    fn code(&self, _: &mut GLLState<'a>) {
        unimplemented!("Should never run this method on terminals");
    }

    fn is_terminal(&self) -> bool {
        true
    }

    fn to_string(&self) -> &str {
        from_utf8(self).unwrap()
    }

    fn is_nullable(&'a self, _: &GLLState<'a>, _: &mut HashSet<&'a str>) -> bool {
        self.is_eps()
    }
}

impl<'a> Hash for dyn Label<'a> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.to_string().hash(state);
    }
}

impl<'a> PartialEq for dyn Label<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.to_string() == other.to_string()
    }
}

impl<'a> Eq for dyn Label<'a>{}


#[derive(Debug)]
pub(crate) struct GrammarSlot<'a> {
	label: Rc<dyn Label<'a>>,
	rule: Rc<Vec<Ident>>,
	dot: usize,
	uuid: &'a str
}

impl<'a> Eq for GrammarSlot<'a> {}

impl<'a> PartialEq for GrammarSlot<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.uuid == other.uuid && self.dot == other.dot
    }
}

impl<'a> Hash for GrammarSlot<'a> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.uuid.hash(state);
        self.dot.hash(state);
    }
}

impl<'a> GrammarSlot<'a> {
	fn new(label: Rc<dyn Label<'a>>, rule: Rc<Vec<Ident>>, dot: usize, uuid: &'a str) -> Self {
		Self {label, rule, dot, uuid}
	}

	fn is_eps(&self) -> bool {
		self.len() == 0 
	}

	fn is_last(&self) -> bool {
		self.dot == self.len()
	}

	fn len(&self) -> usize {
		self.rule.len()
	}
}

type ParseResult<T> = Result<T, GLLParseError>;

enum GLLParseError {
	UnexpectedByte {
		pointer: usize,
		expected: u8,
		offender: u8
	}
}