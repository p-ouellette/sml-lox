TEST_DIR = craftinginterpreters

all: lox

lox: $(shell mlton -stop f src/lox.mlb)
	mlton -output $@ src/lox.mlb

.PHONY: test
test: $(TEST_DIR) lox
	cd $(TEST_DIR); dart tool/bin/test.dart -i $(CURDIR)/lox jlox

.PHONY: format
format:
	smlfmt --force src/lox.mlb
