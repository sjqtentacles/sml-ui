# sml-ui

[![CI](https://github.com/sjqtentacles/sml-ui/actions/workflows/ci.yml/badge.svg)](https://github.com/sjqtentacles/sml-ui/actions/workflows/ci.yml)

A pure, self-drawn, **immediate-mode widget toolkit** for Standard ML - the
flagship of the pure-SML GUI stack. Each frame the host describes a `widget`
tree; the toolkit lays it out (via [`sml-layout`](https://github.com/sjqtentacles/sml-layout)),
folds a pure input-event model into retained state, emits `event`s, and renders
to a backend-agnostic [`sml-canvas2d`](https://github.com/sjqtentacles/sml-canvas2d)
scene or straight to an [`sml-image`](https://github.com/sjqtentacles/sml-image)
RGBA bitmap.

Everything is **pure and deterministic** - no OS, clock, RNG, threads, or FFI -
so rendered frames are **byte-identical across MLton and Poly/ML**. That is what
makes a GUI toolkit unit-testable headlessly: the test suite renders frames and
compares a **golden checksum** of the pixels against a committed constant on
both compilers.

> Status: under active construction. The core engine, the layout/canvas
> integration, the headless golden-image harness, and the `Label`/`Panel`
> primitives are in place and green on both compilers.

## Widget set (target)

Labels, buttons, checkboxes, radio groups, sliders, text fields,
dropdown/combo boxes, scrollable areas, tabs, menu bars, modal
dialogs/window-chrome, and panels/containers - a self-drawn, OS-independent
(Dear-ImGui-style) desktop look.

## Build & test

```sh
make test        # build + run the suite under MLton
make test-poly   # build + run the suite under Poly/ML (5.9.1)
make all-tests   # both
make example     # render the demo UI to assets/ui.png
```

## License

MIT - see [LICENSE](LICENSE).
