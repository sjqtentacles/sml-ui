(* w_checkbox.sml  -  the Checkbox leaf widget: a labeled square toggle.

   Reference contract (see w_label.sml):

     measure : Prim.config -> ARGS -> Prim.size
     view    : Prim.config * Prim.state * Prim.frameinput * Prim.rect -> ARGS
               -> Prim.viewout

   CONTROLLED: it renders the supplied `checked` and only REPORTS the new value
   via a "toggle" event; it never stores the checkbox value in retained state. *)

structure WCheckbox =
struct
  type args = { id : string, label : string, checked : bool }

  fun measure (cfg : Prim.config) ({ label, ... } : args) : Prim.size =
    let
      val sc = Prim.scaleOf cfg
      val side = Prim.textH sc
      val pad = #pad (#theme cfg)
    in
      { w = side + 6.0 + Prim.textW sc label + 2.0 * pad, h = Prim.rowH cfg }
    end

  fun view (cfg : Prim.config, st : Prim.state, input : Prim.frameinput, rc : Prim.rect)
           ({ id, label, checked } : args) : Prim.viewout =
    let
      val theme = #theme cfg
      val sc = Prim.scaleOf cfg
      val side = Prim.textH sc
      val pad = #pad theme
      val boxY = #y rc + (#h rc - side) / 2.0
      val boxRect : Prim.rect = { x = #x rc + pad, y = boxY, w = side, h = side }
      val baseCmds =
        [ Prim.fillRect (boxRect, Prim.gray 0.99)
        , Prim.strokeRect (boxRect, Prim.gray 0.45, 1.0) ]
      val checkCmds =
        if checked then
          let
            val inset = Prim.textH sc / 4.0
            val mark : Prim.rect =
              { x = #x boxRect + inset, y = #y boxRect + inset
              , w = side - 2.0 * inset, h = side - 2.0 * inset }
          in [ Prim.fillRect (mark, #accent theme) ] end
        else []
      val labelCmd =
        Prim.textIn (cfg, rc, pad + side + 6.0, label, #fg theme)
      val events =
        if Prim.clicked input rc then
          [ { id = id, kind = "toggle", value = Bool.toString (not checked) } ]
        else []
    in
      { cmds = baseCmds @ checkCmds @ [ labelCmd ], events = events, state = st }
    end
end
