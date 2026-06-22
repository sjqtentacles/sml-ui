(* test_button.sml  -  Button golden frames + click dispatch. *)

structure ButtonTests =
struct
  open Support

  val w = Ui.Panel { dir = Layout.Column, gap = 6.0
                   , children = [ Ui.Button { id = "ok", label = "OK" } ] }

  fun run () =
    let
      val csNormal = shot (200, 80) Ui.init noInput w
      val csPressed = shot (200, 80) Ui.init (press (20, 12)) w
      val { events = evIn, ... } = frameOf (200, 80) Ui.init (click (20, 12)) w
      val { events = evOut, ... } = frameOf (200, 80) Ui.init (click (150, 70)) w
    in
      section "button";
      checkString "button normal golden"  ("3FFE8A55", csNormal);
      checkString "button pressed golden"  ("0DED95D5", csPressed);
      checkBool   "pressed differs from normal" (true, csNormal <> csPressed);
      checkString "click inside fires click" ("ok:click=", eventsStr evIn);
      checkInt    "click outside fires nothing" (0, length evOut)
    end
end
