(* test_checkbox.sml  -  Checkbox golden frames + toggle reporting. *)

structure CheckboxTests =
struct
  open Support

  fun box checked =
    Ui.Panel { dir = Layout.Column, gap = 6.0
             , children = [ Ui.Checkbox { id = "c1", label = "ON", checked = checked } ] }

  fun run () =
    let
      val csOff = shot (200, 80) Ui.init noInput (box false)
      val csOn  = shot (200, 80) Ui.init noInput (box true)
      val { events = evToggleOn, ... } = frameOf (200, 80) Ui.init (click (16, 12)) (box false)
      val { events = evToggleOff, ... } = frameOf (200, 80) Ui.init (click (16, 12)) (box true)
      val { events = evOut, ... } = frameOf (200, 80) Ui.init (click (150, 70)) (box false)
    in
      section "checkbox";
      checkString "checkbox off golden" ("C5641525", csOff);
      checkString "checkbox on golden"  ("FE82BE81", csOn);
      checkBool   "on differs from off" (true, csOff <> csOn);
      checkString "click toggles on"  ("c1:toggle=true",  eventsStr evToggleOn);
      checkString "click toggles off" ("c1:toggle=false", eventsStr evToggleOff);
      checkInt    "click outside ignored" (0, length evOut)
    end
end
