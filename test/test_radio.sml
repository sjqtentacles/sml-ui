(* test_radio.sml  -  Radio group golden frames + exact selection dispatch. *)

structure RadioTests =
struct
  open Support

  fun grp sel =
    Ui.Panel { dir = Layout.Column, gap = 6.0
             , children = [ Ui.Radio { id = "col", label = "COLOR"
                                      , options = ["RED", "GREEN", "BLUE"]
                                      , selected = sel } ] }

  (* rows (rh=26): header 0..26, opt0 26..52, opt1 52..78, opt2 78..104 *)
  fun run () =
    let
      val cs0 = shot (200, 140) Ui.init noInput (grp 0)
      val cs1 = shot (200, 140) Ui.init noInput (grp 1)
      val { events = ev1, ... } = frameOf (200, 140) Ui.init (click (20, 65)) (grp 0)
      val { events = ev0, ... } = frameOf (200, 140) Ui.init (click (20, 40)) (grp 0)
      val { events = evHdr, ... } = frameOf (200, 140) Ui.init (click (20, 12)) (grp 0)
    in
      section "radio";
      checkString "radio sel0 golden" ("8E3461D5", cs0);
      checkString "radio sel1 golden" ("2747A595", cs1);
      checkBool   "sel1 differs from sel0" (true, cs0 <> cs1);
      checkString "click option1 selects 1" ("col:select=1", eventsStr ev1);
      checkString "click option0 selects 0" ("col:select=0", eventsStr ev0);
      checkInt    "click header selects nothing" (0, length evHdr)
    end
end
