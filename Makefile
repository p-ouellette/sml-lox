BUILD_DIR = build
TEST_DIR = craftinginterpreters
TREELOX = $(BUILD_DIR)/treelox

all: $(TREELOX)

$(TREELOX): $(shell mlton -stop f treelox/sources.mlb) | $(BUILD_DIR)
	mlton -output $@ treelox/sources.mlb

$(BUILD_DIR):
	mkdir $(BUILD_DIR)

.PHONY: test
test: $(TEST_DIR) $(TREELOX)
	cd $(TEST_DIR) && dart tool/bin/test.dart -i $(abspath $(TREELOX)) jlox

.PHONY: format
format:
	smlfmt --force treelox/sources.mlb
