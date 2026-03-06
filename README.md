# Lox interpreter in Standard ML

This is an interpreter for the Lox language from the book _Crafting
Interpreters_.

Run `make` to build the interpreter with MLton or use
`sml -m src/treelox/sources.cm` to load it into the SML/NJ REPL.

## Status

treelox passes the jlox [test suite][test-suite].
bclox passes the clox test suite for chapter 17.

[test-suite]: https://github.com/munificent/craftinginterpreters?tab=readme-ov-file#testing
