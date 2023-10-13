use super::ast::{WagTree, WagNode, ToAst, WagIx};
use super::{Parse, ParseResult, PeekLexer, Rewrite};
use super::metadata::Metadata;
use super::rule::Rule;
use std::{matches, todo};
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
        while matches!(lexer.peek(), Some(_)) {
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

    fn rewrite(&mut self, depth: usize) {
        fn handle_conflict(mut new_rule: Rule, map: &mut IndexMap<String, Rule>) { // Combine rules for the same ident into a single rule
            let ident = match &new_rule {
                Rule::Analytic(s, ..) | Rule::Generate(s, ..) => s.clone(),
                Rule::Import(..) | Rule::Exclude(..) => todo!(),
            };
            if let Some(orig) = map.get_mut(&ident) {
                match (orig, &mut new_rule) {
                    (Rule::Analytic(_, v1), Rule::Analytic(_, v2)) | (Rule::Generate(_, v1), Rule::Generate(_, v2)) => {
                        v1.extend(std::mem::take(v2));
                    },
                    _ => {map.insert(ident, new_rule);}
                }
            } else {
                 map.insert(ident, new_rule);
            };
        }
        let rules = std::mem::take(&mut self.grammar);
        let mut map: IndexMap<String, Rule> = IndexMap::with_capacity(rules.len());
        for mut rule in rules {
            for new_rule in rule.rewrite(depth).into_iter() {
                handle_conflict(new_rule, &mut map);
            }
            handle_conflict(rule, &mut map)
        }
        self.grammar.extend(map.into_values());
    }

}