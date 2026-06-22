(* test_scroll.sml  -  Scroll clips its child to the viewport and offsets the
   content by the retained scroll position (golden at two offsets). *)

structure ScrollTests =
struct
  open Support

  val tall = Ui.Panel { dir = Layout.Column, gap = 4.0
                      , children = [ Ui.Label "ITEM ONE", Ui.Label "ITEM TWO"
                                   , Ui.Label "ITEM THREE", Ui.Label "ITEM FOUR"
                                   , Ui.Label "ITEM FIVE" ] }
  val w = Ui.Scroll { id = "sc", height = 60.0, child = tall }

  fun run () =
    let
      val csTop = shot (200, 80) Ui.init noInput w
      val st30 = Ui.setScroll Ui.init "sc" 30.0
      val csMid = shot (200, 80) st30 noInput w
    in
      section "scroll";
      checkString "scroll offset 0 golden"  ("1D757B6D", csTop);
      checkString "scroll offset 30 golden" ("5BF6E42D", csMid);
      checkBool   "scrolling changes the frame" (true, csTop <> csMid)
    end
end
