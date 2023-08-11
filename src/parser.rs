mod ast;

use crate::lexer::LexerBridge;
use ast::Wag;


pub struct Parser<'source> {
	lexer: LexerBridge<'source>
}

impl<'source> Parser<'source> {
	pub fn new(data: &'source str) -> Self {
		Self {
			lexer: LexerBridge::new(data)
		}
	}
}