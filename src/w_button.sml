(* w_button.sml  -  the Button leaf widget: a clickable push button.

   Follows the per-widget leaf-module contract (see w_label.sml):

     measure : Prim.config -> ARGS -> Prim.size
     view    : Prim.config * Prim.state * Prim.frameinput * Prim.rect -> ARGS
               -> Prim.viewout

   Draws a filled face (hover/pressed shaded), a 1px border, and the centered
   label, then fires a "click" event on release inside.  A leaf never recurses
   into other widgets; state is threaded through unchanged. *)

structure WButton =
struct
  fun measure (cfg : Prim.config) ({ id = _, label } : { id : string, label : string })
              : Prim.size =
    { w = Prim.textW (Prim.scaleOf cfg) label + 4.0 * #pad (#theme cfg)
    , h = Prim.rowH cfg }

  fun view (cfg : Prim.config, st : Prim.state, input : Prim.frameinput, rc : Prim.rect)
           ({ id, label } : { id : string, label : string }) : Prim.viewout =
    let
      val face =
        if Prim.pressedVis input rc then Prim.gray 0.66
        else if Prim.ptIn rc (#mouse_x input, #mouse_y input) then Prim.gray 0.88
        else Prim.gray 0.80
      val lpad = (#w rc - Prim.textW (Prim.scaleOf cfg) label) / 2.0
      val events =
        if Prim.clicked input rc then [ { id = id, kind = "click", value = "" } ]
        else []
    in
      { cmds = [ Prim.fillRect (rc, face)
               , Prim.strokeRect (rc, Prim.gray 0.45, 1.0)
               , Prim.textIn (cfg, rc, lpad, label, #fg (#theme cfg)) ]
      , events = events
      , state = st }
    end
end
