use wagon_parser::parser::wag::Wag;

use crate::{CodeGenArgs, CodeGen, CodeGenResult};

impl CodeGen for Wag {
    fn gen(self, gen_args: &mut CodeGenArgs) -> CodeGenResult<()> {
        self.metadata.gen(gen_args)?;
        gen_args.fst = Some(true);
        for rule in self.grammar {
            rule.into_inner().gen(gen_args)?;
            gen_args.fst = Some(false);
        }
        Ok(())
    }
}