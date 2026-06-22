(* w_slider.sml  -  the Slider leaf widget: a horizontal, CONTROLLED slider.

   Follows the per-widget leaf-module contract:

     measure : Prim.config -> ARGS -> Prim.size
     view    : Prim.config * Prim.state * Prim.frameinput * Prim.rect -> ARGS
               -> Prim.viewout

   It renders the INCOMING `value` and, when pressed/dragged/clicked inside its
   rect, fires a "change" event with the value implied by the pointer's x. The
   thumb is not moved this frame and retained state is left unchanged. *)

structure WSlider =
struct
  fun measure (cfg : Prim.config)
              (_ : { id : string, lo : real, hi : real, value : real }) : Prim.size =
    { w = 160.0, h = Prim.rowH cfg }

  fun view (cfg : Prim.config, st : Prim.state, input : Prim.frameinput, rc : Prim.rect)
           ({ id, lo, hi, value } : { id : string, lo : real, hi : real, value : real })
           : Prim.viewout =
    let
      val sc = Prim.scaleOf cfg
      val thumbR = Prim.textH sc / 2.0
      val trackX = #x rc + thumbR
      val trackW0 = #w rc - 2.0 * thumbR
      val trackW = if trackW0 <= 0.0 then 1.0 else trackW0
      val trackY = #y rc + #h rc / 2.0
      fun frac v =
        if Real.abs (hi - lo) < 1e~9 then 0.0
        else Prim.clampr (0.0, 1.0) ((v - lo) / (hi - lo))
      fun posX v = trackX + frac v * trackW
      val px = posX value
      val track = { x = trackX, y = trackY - 2.0, w = trackW, h = 4.0 }
      val fill = { x = trackX, y = trackY - 2.0, w = px - trackX, h = 4.0 }
      val thumb = { x = px - thumbR, y = trackY - thumbR
                  , w = 2.0 * thumbR, h = 2.0 * thumbR }
      val cmds =
        [ Prim.fillRect (track, Prim.gray 0.70)
        , Prim.fillRect (fill, #accent (#theme cfg))
        , Prim.fillRect (thumb, Prim.gray 0.95)
        , Prim.strokeRect (thumb, Prim.gray 0.40, 1.0) ]
      val active = Prim.pressedVis input rc orelse Prim.clicked input rc
      val events =
        if active then
          let
            val newFrac = Prim.clampr (0.0, 1.0) ((#mouse_x input - trackX) / trackW)
            val newVal = lo + newFrac * (hi - lo)
          in [ { id = id, kind = "change", value = Prim.fmtReal newVal } ] end
        else []
    in { cmds = cmds, events = events, state = st } end
end
