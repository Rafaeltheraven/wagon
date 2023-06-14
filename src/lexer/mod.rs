mod ident;
mod math;
mod productions;

use logos::Logos;
use productions::Productions;
use math::Math;

#[derive(Debug)]
enum Lexer<'source> {
	Productions(logos::Lexer<'source, Productions>),
	Math(logos::Lexer<'source, Math>),
}

impl<'source> Lexer<'source> {
	fn new(s: &'source str) -> Self {
		Self::Productions(Productions::lexer(s))
	}
}

#[derive(Debug, PartialEq)]
enum Tokens {
	ProductionToken(Productions),
	MathToken(Math)
}

struct LexerBridge<'source> {
	lexer: Lexer<'source>,
	counter: u32
}

impl<'source> LexerBridge<'source> {
	fn new(s: &'source str) -> Self {
		Self { lexer: Lexer::new(s), counter: 0 }
	}
}

impl<'source> Iterator for LexerBridge<'source> {
	type Item = Result<Tokens, ()>;
	fn next(&mut self) -> Option<Self::Item> {
        use Tokens::*;
        match &mut self.lexer {
            Lexer::Productions(prod) => {
                let result = prod.next();
                match result {
                	Some(Ok(Productions::LBr)) | Some(Ok(Productions::LCur)) => self.lexer = Lexer::Math(prod.to_owned().morph()),
                	_ => {}
                };
                result.map(|prod| prod.map(ProductionToken))
            }
            Lexer::Math(math) => {
                let result = math.next();
                match (&result, self.counter) {
                	(Some(Ok(Math::LBr)), _) => self.counter += 1,
                	(Some(Ok(Math::RBr)), 0) | (Some(Ok(Math::RCur)), _) => self.lexer = Lexer::Productions(math.to_owned().morph()),
                	(Some(Ok(Math::RBr)), _) => self.counter -= 1,
                	_ => {}
                };
                result.map(|math| math.map(MathToken))
            }
        }
    }
}

#[cfg(test)]
mod tests {

	use super::{LexerBridge, Tokens, Tokens::*};
	use crate::lexer::productions::Productions;
	use crate::lexer::math::Math;
	use crate::lexer::ident::Ident::*;

	#[test]
	fn test_mode_switching() {
		let s = "[3 + 2.3 - $x] 'a thing' X {y = 4 + (!z['key'] < 3);}";
		let lexer = LexerBridge::new(s);

		let results: Vec<Result<Tokens, ()>> = lexer.collect();
		let expect = vec![
			Ok(ProductionToken(Productions::LBr)),
			Ok(MathToken(Math::LitInt(3))),
			Ok(MathToken(Math::Add)),
			Ok(MathToken(Math::LitFloat(2.3))),
			Ok(MathToken(Math::Sub)),
			Ok(MathToken(Math::Identifier(Synth("x".to_string())))),
			Ok(MathToken(Math::RBr)),
			Ok(ProductionToken(Productions::LitString("a thing".to_string()))),
			Ok(ProductionToken(Productions::Identifier(Unknown("X".to_string())))),
			Ok(ProductionToken(Productions::LCur)),
			Ok(MathToken(Math::Identifier(Unknown("y".to_string())))),
			Ok(MathToken(Math::Assigns)),
			Ok(MathToken(Math::LitInt(4))),
			Ok(MathToken(Math::Add)),
			Ok(MathToken(Math::LPar)),
			Ok(MathToken(Math::Identifier(Inherit("z".to_string())))),
			Ok(MathToken(Math::LBr)),
			Ok(MathToken(Math::LitString("key".to_string()))),
			Ok(MathToken(Math::RBr)),
			Ok(MathToken(Math::Lt)),
			Ok(MathToken(Math::LitInt(3))),
			Ok(MathToken(Math::RPar)),
			Ok(MathToken(Math::Semi)),
			Ok(MathToken(Math::RCur))
		];
		assert_eq!(results, expect);
	}
}