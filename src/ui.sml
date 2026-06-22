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

  (* Internal render result: leaf `viewout` plus an `overlays` list of commands
     that must be drawn AFTER the whole tree (z-top) - open dropdown popups,
     menus, and modals. *)
  type vout = { cmds : C.cmd list, overlays : C.cmd list
              , events : event list, state : state }
  fun leaf ({ cmds, events, state } : viewout) : vout =
    { cmds = cmds, overlays = [], events = events, state = state }

  fun maxW f xs = foldl (fn (x, a) => Real.max (a, f x)) 0.0 xs

  (* Intrinsic size of a widget (containers measure their children). *)
  fun measure (cfg : config) (w : widget) : size =
    let val sc = scaleOf cfg val pad = #pad (#theme cfg) val rh = rowH cfg in
    case w of
      Label s => WLabel.measure cfg s
    | Button r => WButton.measure cfg r
    | Checkbox r => WCheckbox.measure cfg r
    | Slider r => WSlider.measure cfg r
    | TextField r => WTextField.measure cfg r
    | Radio r => WRadio.measure cfg r
    | Dropdown { options, ... } =>
        { w = maxW (textW sc) options + 2.0 * pad + textH sc + 12.0, h = rh }
    | Tabs { labels, active, pages, ... } =>
        let
          val stripW = foldl (fn (l, a) => a + textW sc l + 2.0 * pad) 0.0 labels
          val pg = if active >= 0 andalso active < length pages
                   then measure cfg (List.nth (pages, active))
                   else { w = 0.0, h = 0.0 }
        in { w = Real.max (stripW, #w pg), h = rh + #h pg } end
    | Scroll { height, child, ... } =>
        { w = #w (measure cfg child), h = height }
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
    | _ => { w = 120.0, h = rh }
    end

  (* Left-to-right tab strip rects for a Tabs widget. *)
  fun tabRects (cfg, rc : rect, labels) : rect list =
    let val sc = scaleOf cfg val pad = #pad (#theme cfg) val sh = rowH cfg
        fun go (_, []) = []
          | go (x, l :: ls) =
              let val tw = textW sc l + 2.0 * pad
              in { x = x, y = #y rc, w = tw, h = sh } :: go (x + tw, ls) end
    in go (#x rc, labels) end

  (* Draw + interact with a widget inside `rc`, threading retained state. *)
  fun view (cfg : config, st : state, input : frameinput, rc : rect)
           (w : widget) : vout =
    let val sc = scaleOf cfg val pad = #pad (#theme cfg) val rh = rowH cfg in
    case w of
      Label s => leaf (WLabel.view (cfg, st, input, rc) s)
    | Button r => leaf (WButton.view (cfg, st, input, rc) r)
    | Checkbox r => leaf (WCheckbox.view (cfg, st, input, rc) r)
    | Slider r => leaf (WSlider.view (cfg, st, input, rc) r)
    | TextField r => leaf (WTextField.view (cfg, st, input, rc) r)
    | Radio r => leaf (WRadio.view (cfg, st, input, rc) r)
    | Dropdown { id, options, selected, open_ } =>
        let
          val selText = if selected >= 0 andalso selected < length options
                        then List.nth (options, selected) else ""
          val box =
            [ fillRect (rc, gray 0.99)
            , strokeRect (rc, gray 0.45, 1.0)
            , textIn (cfg, rc, pad, selText, #fg (#theme cfg))
            , textIn (cfg, rc, #w rc - pad - textW sc "V", "V", #fg (#theme cfg)) ]
          fun optRect j =
            { x = #x rc, y = #y rc + rh * real (j + 1), w = #w rc, h = rh }
          val idxs = List.tabulate (length options, fn j => j)
          fun optCmds j =
            let val r = optRect j
                val face = if j = selected then #accent (#theme cfg) else gray 0.97
                val fg = if j = selected then gray 0.99 else #fg (#theme cfg)
            in [ fillRect (r, face), strokeRect (r, gray 0.55, 1.0)
               , textIn (cfg, r, pad, List.nth (options, j), fg) ] end
          val pop = if open_ then List.concat (map optCmds idxs) else []
          val optClick = if open_
                         then List.find (fn j => clicked input (optRect j)) idxs
                         else NONE
          val events =
            case optClick of
              SOME j => [ { id = id, kind = "select", value = Int.toString j } ]
            | NONE => if clicked input rc
                      then [ { id = id, kind = if open_ then "close" else "open"
                             , value = "" } ]
                      else []
        in { cmds = box, overlays = pop, events = events, state = st } end
    | Tabs { id, labels, active, pages } =>
        let
          val rects = tabRects (cfg, rc, labels)
          val idxs = List.tabulate (length labels, fn i => i)
          fun tabCmds i =
            let val r = List.nth (rects, i)
                val face = if i = active then gray 0.99 else gray 0.82
            in [ fillRect (r, face), strokeRect (r, gray 0.45, 1.0)
               , textIn (cfg, r, pad, List.nth (labels, i), #fg (#theme cfg)) ] end
          val strip = List.concat (map tabCmds idxs)
          val tabClick = List.find (fn i => clicked input (List.nth (rects, i))) idxs
          val tabEv = case tabClick of
                        SOME i => [ { id = id, kind = "tab", value = Int.toString i } ]
                      | NONE => []
          val pageRect = { x = #x rc, y = #y rc + rh, w = #w rc, h = #h rc - rh }
          val pv = if active >= 0 andalso active < length pages
                   then view (cfg, st, input, pageRect) (List.nth (pages, active))
                   else { cmds = [], overlays = [], events = [], state = st }
        in { cmds = strip @ #cmds pv, overlays = #overlays pv
           , events = tabEv @ #events pv, state = #state pv } end
    | Scroll { id, height, child } =>
        let
          val vp = { x = #x rc, y = #y rc, w = #w rc, h = height }
          val cs = measure cfg child
          val maxOff = Real.max (0.0, #h cs - height)
          val off = clampr (0.0, maxOff) (scrollOf st id)
          val childRect = { x = #x rc, y = #y rc - off, w = #w rc, h = #h cs }
          val cv = view (cfg, st, input, childRect) child
          (* scrollbar on the right edge (drawn outside the clip, always visible) *)
          val barW = 6.0
          val barX = #x rc + #w rc - barW
          val track = { x = barX, y = #y rc, w = barW, h = height }
          val thumbH = if #h cs <= 0.0 then height
                       else Real.max (12.0, height * height / #h cs)
          val thumbY = if maxOff <= 0.0 then #y rc
                       else #y rc + (height - thumbH) * (off / maxOff)
          val thumb = { x = barX, y = thumbY, w = barW, h = thumbH }
          val clipped = [ C.Save, C.Clip vp ] @ #cmds cv @ [ C.Restore ]
          val chrome = [ fillRect (track, gray 0.85), fillRect (thumb, gray 0.55)
                       , strokeRect (vp, gray 0.45, 1.0) ]
        in { cmds = clipped @ chrome, overlays = #overlays cv
           , events = #events cv, state = #state cv } end
    | Panel { dir, gap, children } =>
        let
          val sizes = map (measure cfg) children
          val rects = panelRects (dir, gap, sizes, rc)
          fun go ([], _, st, cmds, ovs, evs) =
                { cmds = cmds, overlays = ovs, events = evs, state = st }
            | go (c :: cs, r :: rs, st, cmds, ovs, evs) =
                let val { cmds = cc, overlays = oo, events = ee, state = st' } =
                          view (cfg, st, input, r) c
                in go (cs, rs, st', cmds @ cc, ovs @ oo, evs @ ee) end
            | go (_, [], st, cmds, ovs, evs) =
                { cmds = cmds, overlays = ovs, events = evs, state = st }
        in go (children, rects, st, [], [], []) end
    | _ => { cmds = [], overlays = [], events = [], state = st }
    end

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
      val { cmds, overlays, events, state } = view (cfg, st, input, root) w
      val state = if hasFocusNext input then advanceFocus state (focusables w)
                  else state
      val scene : C.scene = fillRect (root, #bg (#theme cfg)) :: (cmds @ overlays)
    in { state = state, events = events, scene = scene } end

  fun render (cfg : config) (st : state) (input : frameinput) (w : widget) =
    let
      val { state, events, scene } = frame cfg st input w
      val image =
        C.toImage { width = #width cfg, height = #height cfg
                  , background = #bg (#theme cfg) } scene
    in { state = state, image = image, events = events } end
end
