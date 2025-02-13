# Lox interpreter in Standard ML

This is an interpreter for the Lox language from the book _Crafting
Interpreters_.

The interpreter can build using MLton or PolyMLB (see `Makefile`).
Use `sml -m lox.cm` to load it into the SML/NJ REPL.

## Status

Currently passes the [test suite][test-suite] for chapter 7.

## TODO

- get rid of eof token?
- should RuntimeException propagate the environment to preserve changes before
  the error?
- grep for XXX comments

[test-suite]: https://github.com/munificent/craftinginterpreters?tab=readme-ov-file#testing
