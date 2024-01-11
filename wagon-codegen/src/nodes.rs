/// This implements [`ToTokensState`] for [`Expression`](`wagon_parser::parser::expression::Expression`).
///
/// For most use-cases, calling this method will output rust code that properly evaluates
/// to whatever final value the expression represents. 
///
/// If any new attributes occur in the
/// expression, the provided `attr_fun` function (see [`ToTokensState`]) should register them with the state object.
pub mod expression;
mod disjunct;
mod conjunct;
mod inverse;
mod comp;
mod sum;
mod term;
mod factor;
mod atom;

use crate::ToTokensState;
use std::rc::Rc;