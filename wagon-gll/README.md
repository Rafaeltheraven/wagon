# WAGon-gll

Helper library for the WAGon parser generator. WAGon will generate a parser generator which does GLL parsing. All common GLL methods as well as the state
are then retrieved from this library, whereas the generator will generate the specific data structures and code as required by the grammar.

This library could technically be used for any generic GLL parsing, as long as you stick to the `GrammarLabel` structure.
