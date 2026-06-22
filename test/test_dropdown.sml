(* test_dropdown.sml  -  Dropdown closed/open golden frames, open/close, and
   option selection from the popup overlay. *)

structure DropdownTests =
struct
  open Support

  fun dd open_ =
    Ui.Panel { dir = Layout.Column, gap = 6.0
             , children = [ Ui.Dropdown { id = "dd", options = ["A", "B", "C"]
                                        , selected = 0, open_ = open_ } ] }

  (* box row 0..26; popup opt0 26..52, opt1 52..78, opt2 78..104 *)
  fun run () =
    let
      val csClosed = shot (200, 140) Ui.init noInput (dd false)
      val csOpen   = shot (200, 140) Ui.init noInput (dd true)
      val { events = evOpen, ... }  = frameOf (200, 140) Ui.init (click (20, 12)) (dd false)
      val { events = evClose, ... } = frameOf (200, 140) Ui.init (click (20, 12)) (dd true)
      val { events = evSel, ... }   = frameOf (200, 140) Ui.init (click (20, 65)) (dd true)
    in
      section "dropdown";
      checkString "dropdown closed golden" ("32C9A041", csClosed);
      checkString "dropdown open golden"   ("8685CD38", csOpen);
      checkBool   "open differs from closed" (true, csClosed <> csOpen);
      checkString "click closed box opens"  ("dd:open=",  eventsStr evOpen);
      checkString "click open box closes"   ("dd:close=", eventsStr evClose);
      checkString "click popup option selects" ("dd:select=1", eventsStr evSel)
    end
end
