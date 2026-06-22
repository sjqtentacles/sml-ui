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

  fun frame (cfg : config) (st : state) (input : frameinput) (w : widget) =
    let
      val root = rootRect cfg
      val { cmds, events, state } = view (cfg, st, input, root) w
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
