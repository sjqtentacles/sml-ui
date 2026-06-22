# sml-ui build
#
#   make test       build + run tests under MLton (default)
#   make test-poly  build + run tests under Poly/ML
#   make all-tests  run the suite under both compilers
#   make example    build + run the demo (writes assets/ui.png + assets/ui_modal.png)
#   make gallery    render the per-widget screenshot showcase (assets/widget_*.png)
#   make clean      remove build artifacts

MLTON      ?= mlton
BIN        := bin
SRCDIR     := src
LIBDIR     := lib/github.com/sjqtentacles
TEST_MLB   := test/sources.mlb
SRCS       := $(wildcard $(SRCDIR)/*.sml $(SRCDIR)/*.sig $(SRCDIR)/*.mlb) \
              $(wildcard $(LIBDIR)/*/*.sml $(LIBDIR)/*/*.sig $(LIBDIR)/*/*.mlb) \
              $(wildcard test/*.sml) examples/scenes.sml $(TEST_MLB)

.PHONY: all test poly test-poly all-tests example gallery clean

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

gallery: $(BIN)/gallery
	mkdir -p assets
	./$(BIN)/gallery

$(BIN)/gallery: $(SRCS) examples/scenes.sml examples/gallery.sml examples/gallery.mlb | $(BIN)
	$(MLTON) -output $@ examples/gallery.mlb

$(BIN):
	mkdir -p $(BIN)

clean:
	rm -rf $(BIN)
