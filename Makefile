TEST_DIR = craftinginterpreters

all: treelox

treelox: $(shell mlton -stop f src/treelox/sources.mlb)
	mlton -output $@ src/treelox/sources.mlb

.PHONY: test
test: $(TEST_DIR) treelox
	cd $(TEST_DIR) && dart tool/bin/test.dart -i $(abspath treelox) jlox

.PHONY: format
format:
	smlfmt --force src/*/sources.mlb
