use crate::parser::wag::Wag;

use super::{CodeGenState};

impl Wag {
    pub(crate) fn gen(self, state: &mut CodeGenState) {
        self.metadata.gen(state);
        for rule in self.grammar {
            rule.gen(state);
        }
    }
}