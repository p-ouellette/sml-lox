# Lox interpreter in Standard ML

This is an interpreter for the Lox language from the book _Crafting
Interpreters_.

Run `make` to build the interpreter with MLton or use `sml -m src/lox.cm` to
load it into the SML/NJ REPL.

## Status

Currently passes the [test suite][test-suite] for chapter 11.

## Notes

- function and class values always compare not equal
  - can be fixed by making them ref types like instance

[test-suite]: https://github.com/munificent/craftinginterpreters?tab=readme-ov-file#testing
