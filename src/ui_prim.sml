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

  (* ---- theme ----

     DETERMINISM: every channel is quantized to an exact n/255 byte value via
     `chan` (rounding done HERE with floor(x + 0.5), never Real.round).  Raw
     channel values like 0.70 give 0.70*255 = 178.5, a round-half tie whose
     resolution inside the vendored `Color.pack` differs between MLton's and
     Poly/ML's float multiply - so every color the toolkit emits is snapped to a
     byte fraction first, keeping packed pixels byte-identical across compilers. *)
  fun iround0 (x : real) : int = Real.floor (x + 0.5)
  fun chan (v : real) : real = real (iround0 (v * 255.0)) / 255.0
  type theme = { bg : color, fg : color, accent : color, pad : real, scale : int }
  fun rgba (r, g, b, a) : color = { r = chan r, g = chan g, b = chan b, a = a }
  fun gray v : color = let val c = chan v in { r = c, g = c, b = c, a = 1.0 } end
  val defaultTheme : theme =
    { bg = gray 0.92, fg = gray 0.13
    , accent = rgba (0.20, 0.50, 0.86, 1.0)
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
  fun clampr (lo : real, hi : real) (x : real) =
    if x < lo then lo else if x > hi then hi else x
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
    { r = chan (r * k), g = chan (g * k), b = chan (b * k), a = a }
  fun lighten k ({ r, g, b, a } : color) : color =
    { r = chan (r + (1.0 - r) * k), g = chan (g + (1.0 - g) * k)
    , b = chan (b + (1.0 - b) * k), a = a }

  (* ---- drawing primitives -> Canvas2d.cmd list ----

     DETERMINISM: every coordinate handed to canvas2d is SNAPPED to an integer
     with `sn` = floor(x + 0.5).  Half-pixel coordinates would otherwise reach a
     round-half-to-even tie inside the rasterizer that diverges between MLton and
     Poly/ML; snapping here guarantees the backend only ever sees whole numbers,
     so every golden frame is byte-identical across compilers. *)
  fun sn (x : real) : real = real (iround x)
  fun fillRect ({ x, y, w, h } : rect, col) =
    C.FillRect ({ x = sn x, y = sn y, w = sn w, h = sn h }, col)
  fun strokeRect ({ x, y, w, h } : rect, col, lw) =
    let val x0 = sn x and y0 = sn y and x1 = sn (x + w) and y1 = sn (y + h)
    in C.Stroke { path = [ C.MoveTo (x0, y0), C.LineTo (x1, y0)
                         , C.LineTo (x1, y1), C.LineTo (x0, y1), C.Close ]
                , color = col, width = lw } end
  fun text (x, y, s, col, sc) =
    C.Text { x = sn x, y = sn y, text = s, color = col, scale = sc }

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
