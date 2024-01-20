use wagon_value::{Valueable, RecursiveValue};
use wagon_parser::parser::metadata::Metadata;
use wagon_parser::Spannable;

use crate::{CodeGenArgs, CodeGen, CodeGenResult, CodeGenError};


impl CodeGen for Metadata {
    fn gen(mut self, gen_args: &mut CodeGenArgs) -> CodeGenResult<()> {
        if let Some(atom) = self.mappings.remove("prune") {
            let span = atom.span();
            let node = atom.into_inner();
            gen_args.weight_config.no_prune = !(RecursiveValue::try_from(node).map_err(|x| 
                    CodeGenError::new_spanned(
                        x.into(), 
                        span.clone()
                    )
                )?.is_truthy().map_err(|x| 
                    CodeGenError::new_spanned(
                        x.into(), 
                        span
                    )
                )?
            );
        }
        if let Some(atom) = self.mappings.remove("min_weight") {
            let span = atom.span();
            let node = atom.into_inner();
            gen_args.weight_config.min_weight = RecursiveValue::try_from(node).map_err(|x| 
                    CodeGenError::new_spanned(
                        x.into(), 
                        span.clone()
                    )
                )?.is_truthy().map_err(|x| 
                    CodeGenError::new_spanned(
                        x.into(), 
                        span
                    )
                )?;
        }
        if let Some(atom) = self.mappings.remove("first_set") {
            let span = atom.span();
            let node = atom.into_inner();
            gen_args.weight_config.no_first = !(RecursiveValue::try_from(node).map_err(|x| 
                    CodeGenError::new_spanned(
                        x.into(), 
                        span.clone()
                    )
                )?.is_truthy().map_err(|x| 
                    CodeGenError::new_spanned(
                        x.into(), 
                        span
                    )
                ))?;
        }
        Ok(())
    }
}