use wagon_value::{Valueable, RecursiveValue};
use wagon_parser::parser::metadata::Metadata;
use wagon_parser::parser::atom::Atom;
use wagon_parser::{SpannableNode, Spannable};

use crate::{CodeGenArgs, CodeGen, CodeGenResult, CodeGenError};

fn atom_to_bool(node: SpannableNode<Atom>) -> CodeGenResult<bool> {
    let span = node.span();
    let atom = node.into_inner();
    RecursiveValue::try_from(atom).map_err(|x| 
        CodeGenError::new_spanned(
            x.into(), 
            span.clone()
        )
    )?.is_truthy().map_err(|x| 
        CodeGenError::new_spanned(
            x.into(), 
            span
        )
    )

}

impl CodeGen for Metadata {
    fn gen(mut self, gen_args: &mut CodeGenArgs) -> CodeGenResult<()> {
        if let Some(node) = self.mappings.remove("prune") {
            gen_args.weight_config.no_prune = !atom_to_bool(node)?;
        }
        if let Some(node) = self.mappings.remove("min_weight") {
            gen_args.weight_config.min_weight = atom_to_bool(node)?;
        }
        if let Some(node) = self.mappings.remove("first_set") {
            gen_args.weight_config.no_first = !atom_to_bool(node)?;
        }
        if let Some(node) = self.mappings.remove("allow_zero") {
            gen_args.weight_config.allow_zero = atom_to_bool(node)?;
        }
        Ok(())
    }
}