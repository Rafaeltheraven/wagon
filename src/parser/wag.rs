use crate::firstpass::{FirstPassState, FirstPassResult, WagCheckError};

use super::ast::{WagTree, WagNode, ToAst, WagIx};
use super::{Parse, ParseResult, PeekLexer, Rewrite};
use super::metadata::Metadata;
use super::rule::Rule;
use indexmap::IndexMap;

#[derive(PartialEq, Debug, Eq, Hash)]
pub(crate) struct Wag {
	pub(crate) metadata: Metadata,
	pub(crate) grammar: Vec<Rule>,
}

impl Parse for Wag {
    fn parse(lexer: &mut PeekLexer) -> ParseResult<Self> {
        let metadata = Metadata::parse(lexer)?;
        let mut grammar = Vec::new();
        while lexer.peek().is_some() {
        	grammar.push(Rule::parse(lexer)?);
        }
        Ok(Self {metadata, grammar})
    }
}

impl ToAst for Wag {

    fn to_ast(self, ast: &mut WagTree) -> WagIx {
        let node = ast.add_node(WagNode::Root(self.metadata));
        for child in self.grammar {
            let child_ix = child.to_ast(ast);
            ast.add_edge(node, child_ix, ());
        }
        node
    }
}

impl Rewrite<()> for Wag {

    fn rewrite(&mut self, depth: usize, state: &mut FirstPassState) -> FirstPassResult<()> {
        fn handle_conflict(mut new_rule: Rule, map: &mut IndexMap<String, Rule>) -> FirstPassResult<()>{ // Combine rules for the same ident into a single rule
            let ident = match &new_rule {
                Rule::Analytic(s, ..) | Rule::Generate(s, ..) => s.clone(),
                Rule::Import(..) | Rule::Exclude(..) => todo!(),
            };
            if let Some(orig) = map.get_mut(&ident) {
                match (orig, &mut new_rule) {
                    (Rule::Analytic(_, args1, v1), Rule::Analytic(_, args2, v2)) | (Rule::Generate(_, args1, v1), Rule::Generate(_, args2, v2)) => {
                        if args1 == args2 {
                            v1.extend(std::mem::take(v2));
                        } else {
                            return Err(WagCheckError::DisparateParameters(ident, args1.to_owned(), args2.to_owned()))
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
        let mut map: IndexMap<String, Rule> = IndexMap::with_capacity(rules.len());
        for mut rule in rules {
            for new_rule in rule.rewrite(depth, state)?.into_iter() {
                handle_conflict(new_rule, &mut map)?;
            }
            handle_conflict(rule, &mut map)?
        }
        self.grammar.extend(map.into_values());
        Ok(())
    }

}