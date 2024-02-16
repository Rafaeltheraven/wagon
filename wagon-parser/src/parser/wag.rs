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

/// Check all rules in the WAG for parameter errors and run rewrite on the children.
impl Rewrite<()> for Wag {

    fn rewrite(&mut self, depth: usize, state: &mut FirstPassState) -> FirstPassResult<()> {
        fn handle_conflict(mut new_rule: SpannableNode<Rule>, map: &mut IndexMap<String, SpannableNode<Rule>>) -> FirstPassResult<()>{ // Combine rules for the same ident into a single rule
            let ident = match &new_rule.node {
                Rule::Analytic(s, ..) | Rule::Generate(s, ..) => s.clone(),
                Rule::Import(..) | Rule::Exclude(..) => todo!(),
            };
            if let Some(orig) = map.get_mut(&ident) {
                match (&mut orig.node, &mut new_rule.node) {
                    (Rule::Analytic(_, args1, v1), Rule::Analytic(_, args2, v2)) | (Rule::Generate(_, args1, v1), Rule::Generate(_, args2, v2)) => {
                        if args1 == args2 {
                            v1.extend(std::mem::take(v2));
                        } else {
                            return Err(WagCheckError::DisparateParameters { terminal: ident, offender: args1.to_owned(), expected: args2.to_owned(), span: new_rule.span()});
                        }
                    },
                    _ => {map.insert(ident, new_rule);}
                }
            } else {
                 map.insert(ident, new_rule);
            };
            Ok(())
        }
        let rules = std::mem::take(&mut self.grammar);
        let mut map: IndexMap<String, SpannableNode<Rule>> = IndexMap::with_capacity(rules.len());
        for mut rule in rules {
            let new_rules = rule.rewrite(depth, state)?.0;
            handle_conflict(rule, &mut map)?;
            for new_rule in new_rules {
                handle_conflict(new_rule, &mut map)?;
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