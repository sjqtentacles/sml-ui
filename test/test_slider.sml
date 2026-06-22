(* test_slider.sml  -  Slider golden frame + exact value mapping from mouse-x. *)

structure SliderTests =
struct
  open Support

  val w = Ui.Panel { dir = Layout.Column, gap = 6.0
                   , children = [ Ui.Slider { id = "s", lo = 0.0, hi = 100.0, value = 25.0 } ] }

  (* track geometry: thumbR = textH(scale 2)/2 = 7 ; trackX = 7 ; trackW = 160-14 = 146 *)
  fun run () =
    let
      val csMid = shot (200, 80) Ui.init noInput w
      val { events = evHalf, ... } = frameOf (200, 80) Ui.init (click (80, 13)) w  (* (80-7)/146 = 0.5 *)
      val { events = evLo, ... }   = frameOf (200, 80) Ui.init (click (7, 13)) w   (* frac 0 *)
      val { events = evHi, ... }   = frameOf (200, 80) Ui.init (click (153, 13)) w (* frac 1 *)
    in
      section "slider";
      checkString "slider golden" ("7940C3BA", csMid);
      checkString "mouse mid -> 50.00"  ("s:change=50.00",  eventsStr evHalf);
      checkString "mouse left -> 0.00"  ("s:change=0.00",   eventsStr evLo);
      checkString "mouse right -> 100.00" ("s:change=100.00", eventsStr evHi)
    end
end
