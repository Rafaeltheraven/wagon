
use std::{hash::{Hash, Hasher}, fmt::{Debug}, rc::Rc, str::from_utf8};

use self::state::GLLState;

mod sppf;
mod state;
mod gss;
mod descriptor;
pub(crate) mod ident;

type TerminalBit<'a> = &'a u8;
type Terminal<'a> = &'a[u8];

pub(crate) trait Label<'a>: Debug {
	fn is_eps(&self) -> bool;
	fn first_set(&self, state: &mut GLLState<'a>) -> (Vec<&dyn Label>, Option<TerminalBit>);
	fn code(&self, state: &mut GLLState<'a>);
	fn first(&'a self, token: TerminalBit, state: &mut GLLState<'a>) -> bool {
		let (fst, fin) = self.first_set(state);
		for sub in fst {
			if sub.first(token, state) {
				return true;
			} else if !sub.is_nullable(state) {
			    return false;
			}
		}
		if let Some(last) = fin {
			token == last
		} else {
			false
		}
	}
	fn is_terminal(&self) -> bool {
		false
	}
	fn is_nullable(&'a self, state: &mut GLLState<'a>) -> bool {
		if self.is_eps() {
			true
		} else {
			let root = self.get_root();
			let (fst, _) = root.first_set(state);
			for sub in fst {
				if sub.is_eps() {
					return true
				}
			}
			false
		}
	}
	fn get_root(&self) -> &dyn Label;
	fn to_string(&self) -> &str;
}

impl<'a> Label<'a> for Terminal<'a> {
    fn is_eps(&self) -> bool {
        self.is_empty()
    }

    fn first_set(&self, _: &mut GLLState<'a>) -> (Vec<&dyn Label>, Option<TerminalBit>) {
    	let bit = if !self.is_eps() {
    		&self[0]
    	} else {
    		&0 // null byte should never be a char
    	};
        (Vec::new(), Some(bit))
    }

    fn code(&self, _: &mut GLLState<'a>) {
        unimplemented!("Should never run this method on terminals");
    }

    fn is_terminal(&self) -> bool {
        true
    }

    fn get_root(&self) -> &dyn Label {
        self
    }

    fn to_string(&self) -> &str {
        from_utf8(self).unwrap()
    }
}

#[derive(Debug)]
pub(crate) struct GrammarSlot<'a> {
	label: Box<dyn Label<'a>>,
	rule: Rc<Vec<&'a dyn Label<'a>>>,
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
	fn new(label: Box<dyn Label<'a>>, rule: Rc<Vec<&'a dyn Label<'a>>>, dot: usize, uuid: &'a str) -> Self {
		Self {label, rule, dot, uuid}
	}

	fn is_special(&self) -> bool {
		let a = self.rule[0];
		self.dot == 1 && !self.is_last() && (a.is_terminal() || !a.is_eps())
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