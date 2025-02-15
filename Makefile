TEST_DIR = craftinginterpreters

all: lox

lox: $(shell mlton -stop f lox.mlb)
	mlton -output $@ lox.mlb

test: $(TEST_DIR) lox
	cd $(TEST_DIR); dart tool/bin/test.dart -i $(CURDIR)/lox jlox
