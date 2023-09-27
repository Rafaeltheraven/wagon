use super::ast::{WagTree, WagNode, ToAst, WagIx};
use super::{Parse, ParseResult, PeekLexer, Rewrite};
use super::metadata::Metadata;
use super::rule::Rule;
use std::matches;

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
        let mut new_rules = Vec::new();
        let rules = std::mem::take(&mut self.grammar);
        for mut rule in rules {
            new_rules.extend(rule.rewrite(depth));
            new_rules.push(rule);
        }
        self.grammar.extend(new_rules);
    }

}