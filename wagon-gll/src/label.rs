use regex_automata::dfa::Automaton;
use regex_automata::dfa::dense::DFA;

use crate::Hash;
use crate::Hasher;
use crate::GLLImplementationError;
use crate::ImplementationResult;
use crate::GLLError;
use crate::from_utf8;
use crate::GLLBlockLabel;
use crate::Terminal;
use crate::GLLResult;
use crate::HashSet;
use crate::Rc;
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
	///
	/// Defaults to `false`.
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
	/// S -> A B 'b' | 'b';
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
	///
	/// # Errors
	/// Should return an error if we fail to retrieve the label somewhere.
	fn first_set(&self, state: &GLLState<'a>) -> ImplementationResult<'a, Vec<(Vec<GLLBlockLabel<'a>>, Option<Terminal<'a>>)>>;
	/// Any code to run when encountering this label.
	///
	/// This is called by `GLLState::goto` and is used to make the `goto` from the original paper work.
	///
	/// # Errors
	/// Should return a `GLLParseError` if an error occurs during the parsing or evaluation of attributes.
	fn code(&self, state: &mut GLLState<'a>) -> GLLResult<'a, ()>;
	/// Check if the next token in the current state is accepted by this label's first-follow set.
	///
	/// # Errors
	/// Returns an error if any label in the first set can't be found.
	fn first(&self, state: &mut GLLState<'a>) -> GLLResult<'a, bool> {
		let fst = self.first_set(state)?;
		for (alt, fin) in fst {
			let mut check_fin = true;
			for sub in alt {
				if sub.first(state)? {
					return Ok(true);
				} else if !sub.is_nullable(state)? {
					check_fin = false;
				    break;
				}
			}
			if check_fin {
				if let Some(last) = fin {
					return Ok(last.is_empty() || state.has_next(last))
				}
			}
		}
		Ok(false)
	}
	/// Is this label a terminal? 
	///
	/// Defaults to `false`.
	fn is_terminal(&self) -> bool {
		false
	}
	/// Could this label resolve to epsilon?
	///
	/// # Errors
	/// Returns an error if any label in the first set can't be found.
	fn is_nullable(&self, state: &GLLState<'a>) -> ImplementationResult<'a, bool> {
		self._is_nullable(state, &mut HashSet::default())
	}

	/// Internal method for [`Label::is_nullable`] to do the recursive step.
	///
	/// Rust does not allow me to make this private, so it will be public.
	///
	/// # Errors
	/// Returns an error if any label in the first set can't be found.
	fn _is_nullable(&self, state: &GLLState<'a>, seen: &mut HashSet<Rc<str>>) -> ImplementationResult<'a, bool> {
		if self.is_eps() {
			Ok(true)
		} else {
			let str_repr = self.uuid();
			if !seen.contains(str_repr) {
				seen.insert(str_repr.into());
				let fst = self.first_set(state)?;
				for (alt, _) in fst {
					if let Some(sub) = alt.into_iter().next() {
						if sub._is_nullable(state, seen)? {
							return Ok(true)
						}
					}
				}
			}
			Ok(false)
		}
	}

	/// Optionally return the weight of this label.
	///
	/// This should either calculate the weight for this label, as denoted in the WAGon DSL, or `None`.
	fn _weight(&self, state: &GLLState<'a>) -> Option<ImplementationResult<'a, Value<'a>>>;
	/// Returns either the weight of this label as calculated by [`_weight`](`Label::_weight`), or `1`.
	///
	/// # Errors
	/// Should return a [`ValueError`] if something goes wrong during the evaluation of the weight.
	fn weight(&self, state: &GLLState<'a>) -> ImplementationResult<'a, Value<'a>> {
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

    fn first_set(&self, _: &GLLState<'a>) -> ImplementationResult<'a, Vec<(Vec<GLLBlockLabel<'a>>, Option<Terminal<'a>>)>> {
        Ok(vec![(Vec::new(), Some(*self))])
    }

    fn first(&self, state: &mut GLLState<'a>) -> GLLResult<'a, bool> {
        Ok(self.is_eps() || state.has_next(self))
    }

    fn code(&self, _: &mut GLLState<'a>) -> GLLResult<'a, ()> {
        Err(GLLError::ImplementationError(GLLImplementationError::Fatal("Attempted running the `code` method on a terminal.")))
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
        from_utf8(self).expect("Terminal was non-utf8")
    }

    fn _is_nullable(&self, _: &GLLState<'a>, _: &mut HashSet<Rc<str>>) -> ImplementationResult<'a, bool> {
        Ok(self.is_eps())
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

	fn _weight(&self, _state: &GLLState<'a>) -> Option<ImplementationResult<'a, Value<'a>>> {
		Some(Err(GLLImplementationError::Fatal("Attempted running the `_weight` method on a terminal.")))
	}
}

#[derive(Debug)]
/// A special type of Terminal which is a regex recognizer.
///
/// Implements label so that regex machines can be used. The string representation/uuid of the machine is its regex pattern.
pub struct RegexTerminal<'a> {
	pattern: &'a str,
	/// The regex automaton this terminal represents.
	pub automaton: DFA<&'a [u32]>
}

impl<'a> RegexTerminal<'a> {
	/// Construct a new `RegexTerminal`.
	#[must_use]
	pub const fn new(pattern: &'a str, automaton: DFA<&'a [u32]>) -> Self {
		Self { pattern, automaton }
	}
}

impl<'a> Label<'a> for RegexTerminal<'a> {
    fn first_set(&self, _: &GLLState<'a>) -> ImplementationResult<'a, Vec<(Vec<GLLBlockLabel<'a>>, Option<Terminal<'a>>)>> {
        Err(GLLImplementationError::Fatal("Attempted running the `first_set` method on a regex."))
    }

    fn code(&self, _: &mut GLLState<'a>) -> GLLResult<'a, ()> {
        Err(GLLError::ImplementationError(GLLImplementationError::Fatal("Attempted running the `code` method on a regex.")))
    }

    fn _weight(&self, _: &GLLState<'a>) -> Option<ImplementationResult<'a, Value<'a>>> {
        Some(Err(GLLImplementationError::Fatal("Attempted running the `weight` method on a regex.")))
    }

    fn to_string(&self) -> &str {
        self.pattern
    }

    fn str_parts(&self) -> Vec<&str> {
        vec![self.to_string()]
    }

    fn uuid(&self) -> &str {
        self.to_string()
    }

    fn attr_rep_map(&self) -> (Vec<&str>, Vec<&str>) {
        (Vec::new(), Vec::new())
    }
    fn is_eps(&self) -> bool {
	    self.automaton.pattern_len() == 0
    }

    /// A regex is sort of between a non-terminal and a terminal. They way `first` is used, we want
    /// it to return `true` if some terminal is parsable from this point. In the case of a regex, this means the pattern is accepting.
    fn first(&self, state: &mut GLLState<'a>) -> GLLResult<'a, bool> {
	    state.has_regex(self.pattern)
    }

    fn is_terminal(&self) -> bool {
	    true
    }

    fn _is_nullable(&self, _: &GLLState<'a>, _: &mut HashSet<Rc<str>>) -> ImplementationResult<'a, bool> {
	    Ok(self.automaton.has_empty())
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
