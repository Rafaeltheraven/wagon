
use quote::quote;

use crate::{parser::symbol::Symbol};
use crate::parser::terminal::Terminal;

use super::{CodeGenState, Rc};

impl Symbol {
    pub(crate) fn gen(self, state: &mut CodeGenState, ident: Rc<str>, alt: usize, block: usize, symbol: usize, found_first: bool) -> bool {
        match self {
            Symbol::NonTerminal(i) => {
            	if !found_first {
            		state.first_queue.get_mut(&ident).unwrap()[alt].0.push(i);
            	}
            	found_first
            },
            Symbol::Assignment(_) => todo!(),
            Symbol::Terminal(t) => {
		    	let full_ident: Rc<str> = format!("{}_{}_{}_{}", &ident, alt, block, symbol).into();
		    	if !found_first {
		    		state.first_queue.get_mut(&ident).unwrap()[alt].1 = Some(full_ident.clone())
		    	}
            	match t {
	                Terminal::Regex(r) => {
	                	let tokens = quote!(
	                		#[regex(#r)]
	                		#full_ident,
	                	);
	                	state.lexer.insert(full_ident, tokens);
	                },
	                Terminal::LitString(s) => {
	                	let tokens = quote!(
	                		$[token(#s)]
	                		#full_ident,
	                	);
	                	state.lexer.insert(full_ident, tokens);
	                },
            	};
            	true
            },
            Symbol::Epsilon => {
            	state.first_queue.get_mut(&ident).unwrap()[alt].1 = Some(state.epsilon.clone());
            	true
            },
        }
    }
}