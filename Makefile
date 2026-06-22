# sml-ui build
#
#   make test       build + run tests under MLton (default)
#   make test-poly  build + run tests under Poly/ML
#   make all-tests  run the suite under both compilers
#   make example    build + run the demo (writes assets/ui.png + assets/ui.txt)
#   make clean      remove build artifacts

MLTON      ?= mlton
BIN        := bin
SRCDIR     := src
LIBDIR     := lib/github.com/sjqtentacles
TEST_MLB   := test/sources.mlb
SRCS       := $(wildcard $(SRCDIR)/*.sml $(SRCDIR)/*.sig $(SRCDIR)/*.mlb) \
              $(wildcard $(LIBDIR)/*/*.sml $(LIBDIR)/*/*.sig $(LIBDIR)/*/*.mlb) \
              $(wildcard test/*.sml) $(TEST_MLB)

.PHONY: all test poly test-poly all-tests example clean

all: $(BIN)/test-mlton

$(BIN)/test-mlton: $(SRCS) | $(BIN)
	$(MLTON) -output $@ $(TEST_MLB)

test: $(BIN)/test-mlton
	$(BIN)/test-mlton

poly: $(BIN)/test-poly

$(BIN)/test-poly: $(SRCS) tools/polybuild | $(BIN)
	sh tools/polybuild -o $@ $(TEST_MLB)

test-poly: $(BIN)/test-poly
	$(BIN)/test-poly

all-tests: test test-poly

example: $(BIN)/demo
	mkdir -p assets
	./$(BIN)/demo

$(BIN)/demo: $(SRCS) examples/demo.sml examples/sources.mlb | $(BIN)
	$(MLTON) -output $@ examples/sources.mlb

$(BIN):
	mkdir -p $(BIN)

clean:
	rm -rf $(BIN)
