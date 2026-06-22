(* ui.sml  -  the sml-ui engine.

   `structure Ui :> UI` re-exports the concrete types from `Prim` (opaquely) and
   adds the two pure frame functions.  The engine is one mutually-recursive pass
   because CONTAINERS (`Panel`, and later `Tabs`/`Scroll`/`Modal`) lay out and
   draw arbitrary child widgets; LEAF widgets live in their own `w_*.sml`
   modules and never recurse.  Container geometry is delegated to `sml-layout`
   via `Prim.panelRects`; everything is rendered through `sml-canvas2d`. *)

structure Ui :> UI =
struct
  open Prim

  fun rootRect (cfg : config) : rect =
    { x = 0.0, y = 0.0, w = real (#width cfg), h = real (#height cfg) }

  (* Intrinsic size of a widget (containers measure their children). *)
  fun measure (cfg : config) (w : widget) : size =
    case w of
      Label s => WLabel.measure cfg s
    | Button r => WButton.measure cfg r
    | Checkbox r => WCheckbox.measure cfg r
    | Slider r => WSlider.measure cfg r
    | TextField r => WTextField.measure cfg r
    | Panel { dir, gap, children } =>
        let
          val sizes = map (measure cfg) children
          val n = length children
          val gaps = if n > 0 then gap * real (n - 1) else 0.0
          fun main s = if dir = L.Row then #w s else #h s
          fun cross s = if dir = L.Row then #h s else #w s
          val sumMain = foldl (fn (s, a) => a + main s) 0.0 sizes + gaps
          val maxCross = foldl (fn (s, a) => Real.max (a, cross s)) 0.0 sizes
        in
          if dir = L.Row then { w = sumMain, h = maxCross }
          else { w = maxCross, h = sumMain }
        end
    | _ => { w = 120.0, h = rowH cfg }

  (* Draw + interact with a widget inside `rc`, threading retained state. *)
  fun view (cfg : config, st : state, input : frameinput, rc : rect)
           (w : widget) : viewout =
    case w of
      Label s => WLabel.view (cfg, st, input, rc) s
    | Button r => WButton.view (cfg, st, input, rc) r
    | Checkbox r => WCheckbox.view (cfg, st, input, rc) r
    | Slider r => WSlider.view (cfg, st, input, rc) r
    | TextField r => WTextField.view (cfg, st, input, rc) r
    | Panel { dir, gap, children } =>
        let
          val sizes = map (measure cfg) children
          val rects = panelRects (dir, gap, sizes, rc)
          fun go ([], _, st, cmds, evs) =
                { cmds = cmds, events = evs, state = st }
            | go (c :: cs, r :: rs, st, cmds, evs) =
                let val { cmds = cc, events = ee, state = st' } =
                          view (cfg, st, input, r) c
                in go (cs, rs, st', cmds @ cc, evs @ ee) end
            | go (_, [], st, cmds, evs) =
                { cmds = cmds, events = evs, state = st }
        in go (children, rects, st, [], []) end
    | _ => { cmds = [], events = [], state = st }

  (* Pre-order list of focusable widget ids (currently text fields), used to
     resolve FocusNext deterministically. *)
  fun focusables (w : widget) : string list =
    case w of
      TextField { id, ... } => [id]
    | Tabs { pages, ... } => List.concat (map focusables pages)
    | Scroll { child, ... } => focusables child
    | Modal { child, ... } => focusables child
    | Panel { children, ... } => List.concat (map focusables children)
    | _ => []

  (* Advance focus to the id after the current focus (cycling); if nothing is
     focused, focus the first; if the focused id is last/unknown, wrap. *)
  fun advanceFocus (st : state) (ids : string list) : state =
    case ids of
      [] => st
    | first :: _ =>
        let
          fun nextAfter (cur, []) = first
            | nextAfter (cur, x :: rest) =
                if x = cur then (case rest of y :: _ => y | [] => first)
                else nextAfter (cur, rest)
          val target = case focusOf st of SOME f => nextAfter (f, ids) | NONE => first
        in setFocus st (SOME target) end

  fun frame (cfg : config) (st : state) (input : frameinput) (w : widget) =
    let
      val root = rootRect cfg
      val { cmds, events, state } = view (cfg, st, input, root) w
      val state = if hasFocusNext input then advanceFocus state (focusables w)
                  else state
      val scene : C.scene = fillRect (root, #bg (#theme cfg)) :: cmds
    in { state = state, events = events, scene = scene } end

  fun render (cfg : config) (st : state) (input : frameinput) (w : widget) =
    let
      val { state, events, scene } = frame cfg st input w
      val image =
        C.toImage { width = #width cfg, height = #height cfg
                  , background = #bg (#theme cfg) } scene
    in { state = state, image = image, events = events } end
end
