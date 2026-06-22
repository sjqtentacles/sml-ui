(* w_textfield.sml  -  the TextField leaf widget: an editable single-line text
   field with a retained buffer and click-to-focus.

   Reference implementation for the per-widget leaf-module contract:

     measure : Prim.config -> ARGS -> Prim.size
     view    : Prim.config * Prim.state * Prim.frameinput * Prim.rect -> ARGS
               -> Prim.viewout

   A leaf never recurses into other widgets; it only draws itself within the
   rect the engine assigns and reports any events/state it produced.  Focus
   cycling (FocusNext) is the engine's job, not ours. *)

structure WTextField =
struct
  fun measure (cfg : Prim.config) ({ id = _, value = _ } : { id : string, value : string })
      : Prim.size =
    { w = 180.0, h = Prim.rowH cfg }

  fun view (cfg : Prim.config, st : Prim.state, input : Prim.frameinput, rc : Prim.rect)
           ({ id, value } : { id : string, value : string }) : Prim.viewout =
    let
      val theme = #theme cfg
      val sc = Prim.scaleOf cfg
      fun eff s = case Prim.textOf s id of SOME t => t | NONE => value

      (* 1. focus on click + initialize buffer *)
      val st2 =
        if Prim.clicked input rc then
          let val st1 = Prim.setFocus st (SOME id)
          in case Prim.textOf st1 id of SOME _ => st1 | NONE => Prim.setText st1 id value end
        else st

      (* 2. focus flag from st2 *)
      val focused2 = Prim.focusOf st2 = SOME id

      (* 3. append typed characters when focused *)
      val (st3, events) =
        if focused2 then
          let val typed = Prim.typedChars input
          in if typed <> "" then
               let val newText = eff st2 ^ typed
               in (Prim.setText st2 id newText,
                   [ { id = id, kind = "input", value = newText } ]) end
             else (st2, [])
          end
        else (st2, [])

      (* render with FINAL text + focus from st3 *)
      val txt = eff st3
      val focused = Prim.focusOf st3 = SOME id
      val face = Prim.fillRect (rc, Prim.gray 0.99)
      val border =
        if focused then Prim.strokeRect (rc, #accent theme, 2.0)
        else Prim.strokeRect (rc, Prim.gray 0.45, 1.0)
      val body = Prim.textIn (cfg, rc, #pad theme, txt, #fg theme)
      val caretCmds =
        if focused then
          let val caretX = #x rc + #pad theme + Prim.textW sc txt
              val caret = { x = caretX
                          , y = #y rc + (#h rc - Prim.textH sc) / 2.0
                          , w = 2.0, h = Prim.textH sc }
          in [ Prim.fillRect (caret, #fg theme) ] end
        else []
    in
      { cmds = [ face, border, body ] @ caretCmds
      , events = events
      , state = st3 }
    end
end
