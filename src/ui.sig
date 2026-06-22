(* ui.sig

   sml-ui - a pure, self-drawn, immediate-mode widget toolkit (the FLAGSHIP of
   the pure-SML GUI stack).  Each frame the host describes a `widget` tree; the
   toolkit lays it out (via `sml-layout` for panels), folds a pure input-event
   model into retained state (focus, text buffers, scroll offsets, open menu),
   emits a list of `event`s, and produces either a `Canvas2d.scene` (the
   backend-agnostic draw model) or a rendered `Image.image` (text drawn with the
   vendored `sml-font`, shapes via the `sml-raster` primitives).

   Everything is pure and DETERMINISTIC: there is no OS, clock, or RNG; the host
   feeds mouse/keyboard as `frameinput`.  Rendered images are byte-identical
   across MLton and Poly/ML, which is what makes the toolkit testable headlessly
   with golden images.

   The widget look is self-drawn and OS-independent (Dear-ImGui-style), NOT a
   native system theme - native theming would require the rejected GTK/Qt
   bindings. *)

signature UI =
sig
  type color = Color.rgba

  (* ---- pure input model (no OS, no clock) ---- *)
  datatype mouse = Move of real * real | Down of real * real | Up of real * real
  datatype input = Mouse of mouse | Key of int | Char of string | FocusNext
  type frameinput =
    { mouse_x : real, mouse_y : real, mouse_down : bool, keys : input list }
  val noInput : frameinput

  (* ---- theme ---- *)
  type theme = { bg : color, fg : color, accent : color, pad : real, scale : int }
  val defaultTheme : theme

  (* ---- retained UI state, threaded across frames ---- *)
  type state
  val init      : state
  val focusOf   : state -> string option
  val setFocus  : state -> string option -> state
  val textOf    : state -> string -> string option   (* text-field buffer override *)
  val setText   : state -> string -> string -> state
  val scrollOf  : state -> string -> real             (* 0.0 if unset *)
  val setScroll : state -> string -> real -> state
  val menuOf    : state -> string option              (* open menu title, if any *)

  (* ---- widget tree ---- *)
  datatype widget =
      Label of string
    | Button of { id : string, label : string }
    | Checkbox of { id : string, label : string, checked : bool }
    | Radio of { id : string, label : string, options : string list, selected : int }
    | Slider of { id : string, lo : real, hi : real, value : real }
    | TextField of { id : string, value : string }
    | Dropdown of { id : string, options : string list, selected : int, open_ : bool }
    | Tabs of { id : string, labels : string list, active : int, pages : widget list }
    | MenuBar of { id : string, menus : (string * string list) list }
    | Scroll of { id : string, height : real, child : widget }
    | Modal of { id : string, title : string, open_ : bool, child : widget }
    | Panel of { dir : Layout.dir, gap : real, children : widget list }

  (* An event fired this frame.  `kind` is one of: "click", "toggle"
     (value "true"/"false"), "select" (value = index), "change" (value =
     fmtReal), "input" (value = buffer text), "tab" (value = index), "menu"
     (value = item), "open"/"close" (dropdown/modal). *)
  type event = { id : string, kind : string, value : string }

  type config = { width : int, height : int, font : Font.font, theme : theme }

  (* The pure frame function: prior state + input + widget tree -> new state,
     fired events, and a Canvas2d scene. *)
  val frame : config -> state -> frameinput -> widget
              -> { state : state, events : event list, scene : Canvas2d.scene }

  (* Convenience headless backend: render a frame straight to an Image (text via
     sml-font, shapes via sml-raster, in z-order). *)
  val render : config -> state -> frameinput -> widget
               -> { state : state, image : Image.image, events : event list }
end
