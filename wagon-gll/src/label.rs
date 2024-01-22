use crate::InnerValueError;
use crate::Hash;
use crate::Hasher;
use crate::GLLParseError;
use crate::from_utf8;
use crate::ValueError;
use crate::GLLBlockLabel;
use crate::Terminal;
use crate::ParseResult;
use crate::HashSet;
use crate::Rc;
use crate::ValueResult;
use crate::Value;
use std::fmt::Debug;

use crate::GLLState;

/// The main trait all elements in a GLL grammar should implement.
///
/// Every single element, both non-terminals and terminals are defined as a "Label". 
/// This trait defines methods that those labels should implement. For [`Terminal`]s, it is
/// already implemented, but non-terminals should be implemented specifically based on how they should
/// operate.
pub trait Label<'a>: Debug {
	/// Is this Label epsilon?
	fn is_eps(&self) -> bool {
		false
	}
	/// Returns the first-follow set of the label.
	///
	/// Encoded as a vector of tuples. The outer vector represents all the alternatives of this label, 
	/// while the inner vector represents all other labels that we need to check the first set from (since they may be epsilon or not).
	/// 
	/// After we have exhausted the inner vector, if the optional [`Terminal`] is not `None` then we can stop after checking whether the current
	/// character is this terminal.
	///
	/// # Example
	/// Assume we have the following rules: 
	/// ```ignore
	/// S -> A B 'b' | B;
	/// A -> 'a';
	/// B -> 'b'
	/// ```
	/// The result of this method for S should then be:
	///
	/// `[([A, B], 'b'), ([], 'b')]`
	///
	/// # Why calculate this at runtime?
	/// The possible existence of weights makes it so that the first-follow set of any non-terminal can change at any point. 
	/// As such, we must calculate the set at runtime, depending on the context. This is the main cause of inefficiency in this library.
	///
	/// There is a possibility that this will change in the future, as the meaning of the first set in the context of WAGs is re-evaluated.
	/// But for now, it remains in it's current functional state.
	fn first_set(&self, state: &GLLState<'a>) -> Vec<(Vec<GLLBlockLabel<'a>>, Option<Terminal<'a>>)>;
	/// Any code to run when encountering this label.
	///
	/// This is called by `GLLState::goto` and is used to make the `goto` from the original paper work.
	///
	/// # Errors
	/// Should return a `GLLParseError` if an error occurs during the parsing or evaluation of attributes.
	fn code(&self, state: &mut GLLState<'a>) -> ParseResult<'a, ()>;
	/// Check if the next token in the current state is accepted by this label's first-follow set.
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
	/// Is this label a terminal?
	fn is_terminal(&self) -> bool {
		false
	}
	/// Could this label resolve to epsilon?
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
	/// Optionally return the weight of this label.
	///
	/// This should either calculate the weight for this label, as denoted in the WAGon DSL, or `None`.
	fn _weight(&self, state: &GLLState<'a>) -> Option<ValueResult<'a, Value<'a>>>;
	/// Returns either the weight of this label as calculated by [`_weight`](`Label::_weight`), or `1`.
	///
	/// # Errors
	/// Should return a [`ValueError`] if something goes wrong during the evaluation of the weight.
	fn weight(&self, state: &GLLState<'a>) -> ValueResult<'a, Value<'a>> {
		self._weight(state).map_or_else(|| Ok(1.into()), |weight| weight)
	}
	/// A string representation of the chunk (likely a GLL block) that this label represents.
	fn to_string(&self) -> &str;
	/// The chunk represented by [`Label::to_string`], but split by symbol into a vector.
	fn str_parts(&self) -> Vec<&str>;
	/// A unique identifier for this label.
	fn uuid(&self) -> &str;
	/// A tuple of string representations for any associated attributes.
	///
	/// The first element is a vector of all the inherited or local attributes. The second elements is a vector
	/// of all currently synthesized attributes.
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

    fn code(&self, _: &mut GLLState<'a>) -> ParseResult<'a, ()> {
        Err(GLLParseError::Fatal("Attempted running the `code` method on a terminal."))
    }

    fn is_terminal(&self) -> bool {
        true
    }

    /// The string represented by the byte array
    ///
    /// # Panics
    /// This will panic if the byte array is not a string. 
    /// This should never happen
    /// and also is an issue specifically only for [`Terminal`] (any other [`Label`] can not crash here.)
    /// As such, it was decided that this function will not return a proper [`ParseResult`].
    fn to_string(&self) -> &str {
    	#[allow(clippy::expect_used)]
        from_utf8(self).expect("Terminal was non-ut8")
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

	fn _weight(&self, _state: &GLLState<'a>) -> Option<ValueResult<'a, Value<'a>>> {
		Some(Err(ValueError::ValueError(InnerValueError::Fatal("Attempted running the `_weight` method on a terminal.".to_string()))))
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
