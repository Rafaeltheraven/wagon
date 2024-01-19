use wagon_value::{Valueable, RecursiveValue};
use wagon_parser::parser::metadata::Metadata;

use crate::{CodeGenArgs, CodeGen, CodeGenResult};


impl CodeGen for Metadata {
    fn gen(mut self, gen_args: &mut CodeGenArgs) -> CodeGenResult<()> {
        if let Some(atom) = self.mappings.remove("prune") {
            gen_args.weight_config.no_prune = !(RecursiveValue::try_from(atom)?.is_truthy()?)
        }
        if let Some(atom) = self.mappings.remove("min_weight") {
            gen_args.weight_config.min_weight = RecursiveValue::try_from(atom)?.is_truthy()?
        }
        if let Some(atom) = self.mappings.remove("first_set") {
            gen_args.weight_config.no_first = !(RecursiveValue::try_from(atom)?.is_truthy()?)
        }
        Ok(())
    }
}