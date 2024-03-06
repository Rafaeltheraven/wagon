use std::fmt::Display;

use crate::firstpass::{FirstPassState, FirstPassResult, WagCheckError};

use super::{Parse, ParseResult, LexerBridge, Rewrite, SpannableNode, Spannable, Peek};
use super::metadata::Metadata;
use super::rule::Rule;
use indexmap::IndexMap;

use wagon_macros::new_unspanned;

#[derive(PartialEq, Debug, Eq, Hash)]
#[new_unspanned]
/// The full WAG tree.
/// 
/// # Grammar
/// `Wag -> [Metadata]? [Rule]*;`
pub struct Wag {
    /// Metadata associated with this WAG.
	pub metadata: SpannableNode<Metadata>,
    /// The actual grammar.
	pub grammar: Vec<SpannableNode<Rule>>,
}

impl Parse for Wag {
    fn parse(lexer: &mut LexerBridge) -> ParseResult<Self> {
        let metadata = SpannableNode::parse(lexer)?;
        let mut grammar = Vec::new();
        while lexer.peek().is_some() {
        	grammar.push(SpannableNode::parse(lexer)?);
        }
        Ok(Self {metadata, grammar})
    }
}

/// Check all rules in the WAG for parameter errors, run rewrite on the children and combine rules with the same name into a single rule.
impl Rewrite<()> for Wag {

    fn rewrite(&mut self, depth: usize, state: &mut FirstPassState) -> FirstPassResult<()> {
        fn handle_conflict(mut new_rule: SpannableNode<Rule>, map: &mut IndexMap<String, SpannableNode<Rule>>) -> FirstPassResult<()>{ // Combine rules for the same ident into a single rule
            let ident = match &new_rule.node { // Get the identifier for this rule.
                Rule::Analytic(s, ..) | Rule::Generate(s, ..) => s.clone(),
                Rule::Import(..) | Rule::Exclude(..) => todo!(),
            };
            if let Some(orig) = map.get_mut(&ident) { // Check if we encountered this exact same identifier before.
                match (&mut orig.node, &mut new_rule.node) {
                    (Rule::Analytic(_, args1, v1), Rule::Analytic(_, args2, v2)) | (Rule::Generate(_, args1, v1), Rule::Generate(_, args2, v2)) => { // If both are analytic
                        if args1 == args2 { // And they have the same attributes
                            v1.extend(std::mem::take(v2)); // Merge this new rule into the original.
                        } else { // If they have different attributes we have a problem
                            return Err(WagCheckError::DisparateParameters { terminal: ident, offender: args1.to_owned(), expected: args2.to_owned(), span: new_rule.span()});
                        }
                    },
                    _ => {map.insert(ident, new_rule);}
                }
            } else { // If this is a new rule, just add it to the map.
                 map.insert(ident, new_rule);
            };
            Ok(())
        }
        let rules = std::mem::take(&mut self.grammar);
        let mut map: IndexMap<String, SpannableNode<Rule>> = IndexMap::with_capacity(rules.len());
        for mut rule in rules {
            let new_rules = rule.rewrite(depth, state)?.0; // Rewrite the rule and get all the new rules that this has created.
            handle_conflict(rule, &mut map)?; // Make sure this rule has no conflicts.
            for new_rule in new_rules {
                handle_conflict(new_rule, &mut map)?; // And the new rules don't either.
            }
        }
        self.grammar.extend(map.into_values());
        Ok(())
    }

}

impl Display for Wag {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.metadata.fmt(f)?;
        for rule in &self.grammar {
            rule.fmt(f)?;
        }
        Ok(())
    }
}