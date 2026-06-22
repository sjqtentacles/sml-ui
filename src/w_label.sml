(* w_label.sml  -  the Label leaf widget: static, non-interactive text.

   Reference implementation for the per-widget leaf-module contract:

     measure : Prim.config -> ARGS -> Prim.size
     view    : Prim.config * Prim.state * Prim.frameinput * Prim.rect -> ARGS
               -> Prim.viewout

   A leaf never recurses into other widgets; it only draws itself within the
   rect the engine assigns and reports any events/state it produced. *)

structure WLabel =
struct
  fun measure (cfg : Prim.config) (s : string) : Prim.size =
    { w = Prim.textW (Prim.scaleOf cfg) s + 2.0 * #pad (#theme cfg)
    , h = Prim.rowH cfg }

  fun view (cfg : Prim.config, st : Prim.state, _ : Prim.frameinput, rc : Prim.rect)
           (s : string) : Prim.viewout =
    { cmds = [ Prim.textIn (cfg, rc, #pad (#theme cfg), s, #fg (#theme cfg)) ]
    , events = []
    , state = st }
end
