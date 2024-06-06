# WAGon Parser

A library for parsing the WAGon DSL. This crate provides a struct that, given a `String` with the DSL as input, either returns a full AST representing the input file, or a proper error. The error can be handled as needed, the AST can function as the basis for any code generation or analysis that one wants to do. 

Additionally, this crate includes a very simple checker which does the following things:

* Check whether any rule has duplicate attributes in their declaration (i.e. `S<*a, *a> -> ...;`).
* Check whether alternate definitions of the same nonterminal have different attributes in their declarations (i.e. `S<*a> -> ...; S<*b, *c> -> ...;`).
* Merge multiple rules for the same nonterminal into a single rule with alternatives.
* Factor our EBNF operators.

This crate will be the main entrypoint for any language designer who wants to use WAGs.

## Example
```
use wagon_parser::parse_and_check;

let input_grammar = ...;
let wag = parse_and_check(input_grammar);
assert!(wag.is_ok());
```

This crate also defines the AST for a fully parsed WAGon WAG. It's structure mirrors that of the formal DSL.