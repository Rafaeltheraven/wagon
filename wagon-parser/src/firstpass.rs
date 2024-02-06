use std::fmt::Display;
use wagon_lexer::Spannable;
use crate::{SpannableNode, ErrorReport, Span};
use std::{collections::HashMap, error::Error};

use indexmap::IndexSet;
use wagon_ident::Ident;

pub(crate) type ReqAttributes = IndexSet<SpannableNode<Ident>>;

/// Any node that can be rewritten in a different way for any reason should implement this trait.
pub(crate) trait Rewrite<T> {
	fn rewrite(&mut self, depth: usize, state: &mut FirstPassState) -> FirstPassResult<T>;
}

/// A state object to keep track of things during rewriting.
#[derive(Debug, Default)]
pub(crate) struct FirstPassState {
	parameter_map: HashMap<String, IndexSet<SpannableNode<Ident>>>
}

/// After rewriting/typechecking, we either return something or we have a [`WagCheckError`].
pub type FirstPassResult<T> = Result<T, WagCheckError>;

#[derive(PartialEq, Eq, Debug)]
/// Any error that can occur during the rewriting/checking process.
pub enum WagCheckError {
	/// A rule wants multiple parameters, but they are the exact same.
	DuplicateParameters(String, SpannableNode<Ident>),
	/// Two alternative instances of a rule want different parameters.
	DisparateParameters {
		/// The non-terminal which has the issue.
		terminal: String,
		/// The specific [`Ident`] which caused the issue.
		offender: Vec<SpannableNode<Ident>>,
		/// The [`Ident`] we expected to see.
		expected: Vec<SpannableNode<Ident>>,
		/// The span information of this node.
		span: Span
	},
}

impl Error for WagCheckError{}

impl ErrorReport for WagCheckError {
	fn msg(&self) -> (String, String) {
		match self {
		    Self::DuplicateParameters(nt, i) => ("Duplicate Parameter!".to_string(), format!("Nonterminal {nt} uses parameter {i} multiple times. This makes no sense.")),
    		Self::DisparateParameters { terminal, offender, expected, ..} => ("Disparate Parameters!".to_string(), format!("Instance of terminal {terminal} uses parameters {offender:?} while it was defined earlier as {expected:?}. This is unsupported")),
		}
	}

	fn span(self) -> Span {
		match self {
		    Self::DuplicateParameters(_, i) => i.span(),
    		Self::DisparateParameters { span, .. } => span,
		}
	}
}

impl Display for WagCheckError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    	let (head, msg) = self.msg();
    	write!(f, "{head}: {msg}")
    }
}

impl FirstPassState {
	pub(crate) fn add_parameter(&mut self, nt: String, param: SpannableNode<Ident>) -> FirstPassResult<()> {
		if let Some(params) = self.parameter_map.get_mut(&nt) {
			if params.contains(&param) {
				Err(WagCheckError::DuplicateParameters(nt, param))
			} else {
				params.insert(param);
				Ok(())
			}
		} else {
			let mut set = IndexSet::new();
			set.insert(param);
			self.parameter_map.insert(nt, set);
			Ok(())
		}
	}

	// pub(crate) fn get_parameters(&self, s: &str) -> Option<&IndexSet<SpannableNode<Ident>>> {
	// 	self.parameter_map.get(s)
	// }
}

pub(crate) trait GetReqAttributes {
	fn get_req_attributes(&self) -> ReqAttributes;
}

#[cfg(test)]
mod test {

	use crate::parser::Parser;
	use super::{FirstPassState, Rewrite};
	
	use pretty_assertions::assert_eq;

	fn test_inputs(input: &str, expected_input: &str) {
		let mut parser = Parser::new(input);
		let mut output = parser.parse().unwrap();
		output.rewrite(0, &mut FirstPassState::default()).unwrap();
		let mut parser = Parser::new(expected_input);
		let expected = parser.parse().unwrap();
		assert_eq!(expected, output);
	}

	#[test]
	fn test_simple_rewrite_maybe() {
		let input = r"
			A -> X Y?;
		";
		let expected_input = r"
			A -> X A·0·1;
			A·0·1 -> Y | ;
		";
		test_inputs(input, expected_input);
	}

	#[test]
	fn test_simple_rewrite_many() {
		let input = r"
			A -> X Y*;
		";
		let expected_input = r"
			A -> X A·0·1;
			A·0·1 -> Y A·0·1 | ;
		";
		test_inputs(input, expected_input);
	}

	#[test]
	fn test_simple_rewrite_some() {
		let input = r"
			A -> X Y+;
		";
		let expected_input = r"
			A -> X A·0·1;
			A·0·1·p -> Y A·0·1·p | ;
			A·0·1 -> Y A·0·1·p;
		";
		test_inputs(input, expected_input);
	}

	#[test]
	fn test_simple_group() {
		let input = r"
			A -> (B C?)+;
		";
		let expected_input = r"
			A -> A·0·0;
			A·0·0·p -> A·0·0··0 A·0·0·p | ;
			A·0·0 -> A·0·0··0 A·0·0·p;
			A·0·0··0 -> B A·0·0··0·0·1;
			A·0·0··0·0·1 -> C | ;
		";
		test_inputs(input, expected_input);
	}

	#[test]
	fn test_complex_rewrite() {
		let input = r"
			A -> ((X Y)+ Z?)+;
		";
		let expected_input = r"
			A -> A·0·0;
			A·0·0·p -> A·0·0··0 A·0·0·p | ;
			A·0·0 -> A·0·0··0 A·0·0·p;
			A·0·0··0 -> A·0·0··0·0·0 A·0·0··0·0·1;
			A·0·0··0·0·0·p -> A·0·0··0·0·0··1 A·0·0··0·0·0·p | ;
			A·0·0··0·0·0 -> A·0·0··0·0·0··1 A·0·0··0·0·0·p;
			A·0·0··0·0·0··1 -> X Y;
			A·0·0··0·0·1 -> Z | ;
		";
		test_inputs(input, expected_input);
	}

	#[test]
	fn test_rewrite_group_no_ebnf() {
		let input = r"
			A -> (B C);
		";
		let expected_input = r"
			A -> A·0·0;
			A·0·0 -> B C;
		";
		test_inputs(input, expected_input);
	}

	#[test]
	fn test_rewrite_conflict() {
		let input = r"
			A -> B;
			A -> C;
			A -> ;
		";
		let expected_input = r"
			A -> B | C | ;
		";
		test_inputs(input, expected_input);
	}

	#[test]
	fn test_simple_rewrite_with_attrs() {
		let input = r"
			A<*a> -> B<*a, $b>+;
		";
		let expected_input = r"
			A<*a> -> A·0·0<*a, $b>;
			A·0·0·p<&a, &b> -> B<&a, &b> A·0·0·p<&a, &b> | ;
			A·0·0<&a, &b> -> B<&a, &b> A·0·0·p<&a, &b>;
		";
		test_inputs(input, expected_input);
	}

	#[test]
	fn test_simple_group_with_attrs() {
		let input = r"
			A<*x> -> (B<*x> C<$y>?)+;
		";
		let expected_input = r"
			A<*x> -> A·0·0<*x, $y>;
			A·0·0·p<&x, &y> -> A·0·0··0<&x, &y> A·0·0·p<&x, &y> | ;
			A·0·0<&x, &y> -> A·0·0··0<&x, &y> A·0·0·p<&x, &y>;
			A·0·0··0<&x, &y> -> B<&x> A·0·0··0·0·1<&y>;
			A·0·0··0·0·1<&y> -> C<&y> | ;
		";
		test_inputs(input, expected_input);
	}

	#[test]
	fn test_complex_rewrite_with_attrs() {
		let input = r"
			A<*a, *b> -> ((X<*a, *b> Y<*b, $c>)+ Z<$c>?)+;
		";
		let expected_input = r"
			A<*a, *b> -> A·0·0<*a, *b, $c>;
			A·0·0·p<&a, &b, &c> -> A·0·0··0<&a, &b, &c> A·0·0·p<&a, &b, &c> | ;
			A·0·0<&a, &b, &c> -> A·0·0··0<&a, &b, &c> A·0·0·p<&a, &b, &c>;
			A·0·0··0<&a, &b, &c> -> A·0·0··0·0·0<&a, &b, &c> A·0·0··0·0·1<&c>;
			A·0·0··0·0·0·p<&a, &b, &c> -> A·0·0··0·0·0··1<&a, &b, &c> A·0·0··0·0·0·p<&a, &b, &c> | ;
			A·0·0··0·0·0<&a, &b, &c> -> A·0·0··0·0·0··1<&a, &b, &c> A·0·0··0·0·0·p<&a, &b, &c>;
			A·0·0··0·0·0··1<&a, &b, &c> -> X<&a, &b> Y<&b, &c>;
			A·0·0··0·0·1<&c> -> Z<&c> | ;
		";
		test_inputs(input, expected_input);
	}

	#[test]
	fn test_group_no_ebnf_with_attrs() {
		let input = r"
			A<*a> -> (B<*a> C<$b>);
		";
		let expected_input = r"
			A<*a> -> A·0·0<*a, $b>;
			A·0·0<&a, &b> -> B<&a> C<&b>;
		";
		test_inputs(input, expected_input);
	}
}