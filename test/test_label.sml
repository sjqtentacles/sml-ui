(* test_label.sml  -  golden-checksum + layout tests for the Label widget and
   the Panel container (the layout-engine smoke). *)

structure LabelTests =
struct
  open Support

  fun run () =
    let
      val w = Ui.Label "HELLO"
      val csLabel = shot (120, 40) Ui.init noInput w

      (* a Panel laying out three labels in a column via sml-layout *)
      val panel = Ui.Panel
        { dir = Layout.Column, gap = 4.0
        , children = [Ui.Label "ALPHA", Ui.Label "BETA", Ui.Label "GAMMA"] }
      val csPanel = shot (160, 120) Ui.init noInput panel

      (* a label produces no events and never mutates state *)
      val { events, state, ... } = Ui.render (config (120, 40)) Ui.init noInput w
    in
      section "label";
      checkString "label HELLO golden"  ("805AFD15", csLabel);
      checkString "panel of labels golden" ("EF55210D", csPanel);
      checkInt    "label fires no events" (0, length events);
      checkBool   "label leaves focus unset" (true, Ui.focusOf state = NONE)
    end
end
