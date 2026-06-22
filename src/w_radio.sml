(* w_radio.sml  -  the Radio leaf widget: a vertical radio group with a header
   caption and one row per option.

   Reference contract (see w_label.sml):

     measure : Prim.config -> ARGS -> Prim.size
     view    : Prim.config * Prim.state * Prim.frameinput * Prim.rect -> ARGS
               -> Prim.viewout

   CONTROLLED: it renders the supplied `selected` index and only REPORTS the
   newly clicked option via a "select" event; it never stores it in state. *)

structure WRadio =
struct
  type args = { id : string, label : string, options : string list, selected : int }

  fun measure (cfg : Prim.config) ({ label, options, ... } : args) : Prim.size =
    let
      val sc = Prim.scaleOf cfg
      val pad = #pad (#theme cfg)
      val side = Prim.textH sc
      val headerW = pad + Prim.textW sc label + pad
      fun optW opt = pad + side + 6.0 + Prim.textW sc opt + pad
      val w = List.foldl (fn (opt, m) => Real.max (m, optW opt)) headerW options
      val h = Prim.rowH cfg * real (1 + length options)
    in
      { w = w, h = h }
    end

  fun view (cfg : Prim.config, st : Prim.state, input : Prim.frameinput, rc : Prim.rect)
           ({ id, label, options, selected } : args) : Prim.viewout =
    let
      val theme = #theme cfg
      val sc = Prim.scaleOf cfg
      val pad = #pad theme
      val rh = Prim.rowH cfg
      val side = Prim.textH sc
      val n = length options
      val headerRect : Prim.rect = { x = #x rc, y = #y rc, w = #w rc, h = rh }
      val headerCmd = Prim.textIn (cfg, headerRect, pad, label, #fg theme)
      fun rowRect j : Prim.rect =
        { x = #x rc, y = #y rc + rh * real (j + 1), w = #w rc, h = rh }
      fun rowCmds j =
        let
          val rr = rowRect j
          val opt = List.nth (options, j)
          val boxY = #y rr + (rh - side) / 2.0
          val boxRect : Prim.rect = { x = #x rc + pad, y = boxY, w = side, h = side }
          val baseCmds =
            [ Prim.fillRect (boxRect, Prim.gray 0.99)
            , Prim.strokeRect (boxRect, Prim.gray 0.45, 1.0) ]
          val dotCmds =
            if j = selected then
              let
                val inset = side / 4.0
                val dot : Prim.rect =
                  { x = #x boxRect + inset, y = #y boxRect + inset
                  , w = side - 2.0 * inset, h = side - 2.0 * inset }
              in [ Prim.fillRect (dot, #accent theme) ] end
            else []
          val textCmd = Prim.textIn (cfg, rr, pad + side + 6.0, opt, #fg theme)
        in
          baseCmds @ dotCmds @ [ textCmd ]
        end
      val optCmds = List.concat (List.tabulate (n, rowCmds))
      fun hit j = if Prim.clicked input (rowRect j) then SOME j else NONE
      val events =
        case List.mapPartial hit (List.tabulate (n, fn j => j)) of
            j :: _ => [ { id = id, kind = "select", value = Int.toString j } ]
          | [] => []
    in
      { cmds = headerCmd :: optCmds, events = events, state = st }
    end
end
