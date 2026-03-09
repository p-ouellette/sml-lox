# Lox interpreters in Standard ML

There are two Lox interpreters based on the ones in the book _Crafting
Interpreters_.
treelox is a tree-walk interpreter based on jlox.
bclox is a bytecode virtual machine based on clox.

Run `make` to build the interpreters with MLton or use `sml -m
src/<interpreter>/sources.cm` to load an interpreter into the SML/NJ REPL.

## Status

treelox passes the jlox [test suite][test-suite].
bclox passes the clox test suite for chapter 17.

[test-suite]: https://github.com/munificent/craftinginterpreters?tab=readme-ov-file#testing
