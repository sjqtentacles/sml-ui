(* ui_prim.sml  -  shared internal core for sml-ui (NOT sealed; visible to the
   per-widget leaf modules and to the engine in ui.sml).

   It holds the concrete public-ish types (re-exported opaquely by `structure
   Ui :> UI`), the retained-state record + accessors, deterministic geometry
   helpers, and the canvas drawing primitives every widget shares.

   DETERMINISM: all rounding goes through `iround` = floor(x + 0.5) (NOT
   Real.round, whose round-half-to-even diverges between compilers); reals are
   printed with the forced-decimal `fmtReal`.  Text metrics match the built-in
   canvas2d 5x7 font (advance 6 px, height 7 px per cell, scaled by `scale`),
   verified against the vendored font5x7 BDF. *)

structure Prim =
struct
  structure C = Canvas2d
  structure L = Layout

  type color = Color.rgba
  type rect  = { x : real, y : real, w : real, h : real }
  type size  = { w : real, h : real }

  (* ---- pure input model ---- *)
  datatype mouse = Move of real * real | Down of real * real | Up of real * real
  datatype input = Mouse of mouse | Key of int | Char of string | FocusNext
  type frameinput =
    { mouse_x : real, mouse_y : real, mouse_down : bool, keys : input list }
  val noInput : frameinput =
    { mouse_x = ~1.0, mouse_y = ~1.0, mouse_down = false, keys = [] }

  (* ---- theme ---- *)
  type theme = { bg : color, fg : color, accent : color, pad : real, scale : int }
  fun gray v : color = { r = v, g = v, b = v, a = 1.0 }
  val defaultTheme : theme =
    { bg = gray 0.92, fg = gray 0.13
    , accent = { r = 0.20, g = 0.50, b = 0.86, a = 1.0 }
    , pad = 8.0, scale = 2 }

  (* ---- widget tree (concrete here; replicated into the sealed Ui) ---- *)
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
    | Panel of { dir : L.dir, gap : real, children : widget list }

  type event = { id : string, kind : string, value : string }
  type config = { width : int, height : int, font : Font.font, theme : theme }

  (* ---- retained state, threaded across frames ---- *)
  type state =
    { focus : string option
    , texts : (string * string) list
    , scrolls : (string * real) list
    , menu : string option }
  val init : state = { focus = NONE, texts = [], scrolls = [], menu = NONE }

  fun assoc (xs, k) =
    case List.find (fn (k', _) => k' = k) xs of SOME (_, v) => SOME v | NONE => NONE
  fun upsert (xs, k, v) = (k, v) :: List.filter (fn (k', _) => k' <> k) xs

  fun focusOf (s : state) = #focus s
  fun setFocus (s : state) f =
    { focus = f, texts = #texts s, scrolls = #scrolls s, menu = #menu s }
  fun textOf (s : state) id = assoc (#texts s, id)
  fun setText (s : state) id v =
    { focus = #focus s, texts = upsert (#texts s, id, v)
    , scrolls = #scrolls s, menu = #menu s }
  fun scrollOf (s : state) id = case assoc (#scrolls s, id) of SOME r => r | NONE => 0.0
  fun setScroll (s : state) id r =
    { focus = #focus s, texts = #texts s
    , scrolls = upsert (#scrolls s, id, r), menu = #menu s }
  fun menuOf (s : state) = #menu s
  fun setMenu (s : state) m =
    { focus = #focus s, texts = #texts s, scrolls = #scrolls s, menu = m }

  (* What every leaf-widget module's `view` returns: the draw commands it
     contributed, the events it fired this frame, and the (possibly updated)
     retained state. *)
  type viewout = { cmds : Canvas2d.cmd list, events : event list, state : state }

  (* ---- deterministic numeric helpers ---- *)
  fun iround (x : real) : int = Real.floor (x + 0.5)
  fun clampr (lo, hi) x = if x < lo then lo else if x > hi then hi else x
  fun fmtReal r =
    let val s = Real.fmt (StringCvt.FIX (SOME 2)) r
    in if String.isPrefix "~" s then "-" ^ String.extract (s, 1, NONE) else s end

  (* ---- text metrics (match the built-in canvas2d 5x7 font) ---- *)
  val glyphAdv = 6
  val glyphH   = 7
  fun textW (sc : int) s = real (String.size s * glyphAdv * sc)
  fun textH (sc : int) = real (glyphH * sc)
  fun scaleOf (cfg : config) = #scale (#theme cfg)
  fun rowH (cfg : config) = textH (scaleOf cfg) + 12.0

  (* ---- color shades ---- *)
  fun darken k ({ r, g, b, a } : color) : color =
    { r = r * k, g = g * k, b = b * k, a = a }
  fun lighten k ({ r, g, b, a } : color) : color =
    { r = r + (1.0 - r) * k, g = g + (1.0 - g) * k, b = b + (1.0 - b) * k, a = a }

  (* ---- drawing primitives -> Canvas2d.cmd list ---- *)
  fun fillRect (rc : rect, col) = C.FillRect (rc, col)
  fun strokeRect ({ x, y, w, h } : rect, col, lw) =
    C.Stroke { path = [ C.MoveTo (x, y), C.LineTo (x + w, y)
                      , C.LineTo (x + w, y + h), C.LineTo (x, y + h), C.Close ]
             , color = col, width = lw }
  fun text (x, y, s, col, sc) =
    C.Text { x = x, y = y, text = s, color = col, scale = sc }

  (* Draw `s` vertically centered inside `rc`, left edge at rc.x + lpad. *)
  fun textIn (cfg, rc : rect, lpad, s, col) =
    let val sc = scaleOf cfg
        val ty = #y rc + (#h rc - textH sc) / 2.0
    in text (#x rc + lpad, ty, s, col, sc) end

  (* ---- hit testing ---- *)
  fun ptIn ({ x, y, w, h } : rect) (px, py) =
    px >= x andalso px <= x + w andalso py >= y andalso py <= y + h
  fun downInside (input : frameinput) rc =
    List.exists (fn Mouse (Down p) => ptIn rc p | _ => false) (#keys input)
  fun upInside (input : frameinput) rc =
    List.exists (fn Mouse (Up p) => ptIn rc p | _ => false) (#keys input)
  (* A "click" is a release inside the rect this frame. *)
  fun clicked input rc = upInside input rc
  (* Pressed visual: button held with the pointer currently over the rect. *)
  fun pressedVis (input : frameinput) rc =
    #mouse_down input andalso ptIn rc (#mouse_x input, #mouse_y input)

  (* Typed characters this frame, concatenated in order. *)
  fun typedChars (input : frameinput) =
    String.concat (List.mapPartial (fn Char c => SOME c | _ => NONE) (#keys input))
  fun hasFocusNext (input : frameinput) =
    List.exists (fn FocusNext => true | _ => false) (#keys input)

  (* ---- sml-layout integration: distribute children rects in `avail` ---- *)
  val ez : L.edges = { top = 0.0, right = 0.0, bottom = 0.0, left = 0.0 }
  fun leafBox (i, { w, h } : size) =
    L.Box { dir = L.Row, justify = L.JStart, align = L.Start
          , grow = 0.0, basis = NONE, padding = ez, margin = ez, gap = 0.0
          , min = { w = w, h = h }, tag = SOME i, children = [] }
  (* Lay out child boxes of the given intrinsic sizes along `dir` inside `avail`,
     packed at the start with `gap` between them; returns one rect per child in
     order (absolute coords, offset to avail's origin). *)
  fun mapi f xs =
    let fun go (_, []) = [] | go (i, x :: r) = f (i, x) :: go (i + 1, r)
    in go (0, xs) end
  fun panelRects (dir, gap, sizes : size list, avail : rect) : rect list =
    let
      val kids = mapi (fn (i, s) => leafBox (i, s)) sizes
      val root = L.Box { dir = dir, justify = L.JStart, align = L.Start
                       , grow = 0.0, basis = NONE, padding = ez, margin = ez
                       , gap = gap, min = { w = #w avail, h = #h avail }
                       , tag = NONE, children = kids }
      val solved = L.solve { width = #w avail, height = #h avail } root
      fun rectOf i =
        case List.find (fn (SOME j, _) => j = i | _ => false) solved of
            SOME (_, r) => { x = #x r + #x avail, y = #y r + #y avail
                           , w = #w r, h = #h r }
          | NONE => { x = #x avail, y = #y avail, w = 0.0, h = 0.0 }
    in List.tabulate (length sizes, rectOf) end
end
