(* test_tabs.sml  -  Tabs strip switching + only-the-active-page-renders. *)

structure TabsTests =
struct
  open Support

  fun tabs active =
    Ui.Panel { dir = Layout.Column, gap = 6.0
             , children = [ Ui.Tabs { id = "t", labels = ["ONE", "TWO"], active = active
                                     , pages = [ Ui.Label "PAGEA", Ui.Label "PAGEB" ] } ] }

  (* tab0 x 0..52, tab1 x 52..104 (strip height 26) *)
  fun run () =
    let
      val cs0 = shot (220, 120) Ui.init noInput (tabs 0)
      val cs1 = shot (220, 120) Ui.init noInput (tabs 1)
      val { events = ev1, ... } = frameOf (220, 120) Ui.init (click (70, 12)) (tabs 0)
      val { events = ev0, ... } = frameOf (220, 120) Ui.init (click (20, 12)) (tabs 0)
    in
      section "tabs";
      checkString "tabs active0 golden" ("2D8F0241", cs0);
      checkString "tabs active1 golden" ("18FF4741", cs1);
      checkBool   "active page changes render" (true, cs0 <> cs1);
      checkString "click tab1 fires tab=1" ("t:tab=1", eventsStr ev1);
      checkString "click tab0 fires tab=0" ("t:tab=0", eventsStr ev0)
    end
end
