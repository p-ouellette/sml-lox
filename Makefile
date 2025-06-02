TEST_DIR = craftinginterpreters

all: treelox

treelox: $(shell mlton -stop f src/treelox/sources.mlb)
	mlton -output $@ src/treelox/sources.mlb

bclox: $(shell mlton -stop f src/bclox/sources.mlb)
	mlton -output $@ src/bclox/sources.mlb

.PHONY: test
test:
	-cd $(TEST_DIR) && dart tool/bin/test.dart -i $(CURDIR)/treelox jlox
	cd $(TEST_DIR) && dart tool/bin/test.dart -i $(CURDIR)/bclox clox

.PHONY: test_treelox
test_treelox: treelox
	cd $(TEST_DIR) && dart tool/bin/test.dart -i $(CURDIR)/treelox jlox

.PHONY: test_bclox
test_bclox: bclox
	cd $(TEST_DIR) && dart tool/bin/test.dart -i $(CURDIR)/bclox clox

.PHONY: format
format:
	smlfmt --force src/*/sources.mlb
