(* test_textfield.sml  -  TextField focus, typing into the retained buffer, and
   deterministic FocusNext cycling between two fields. *)

structure TextFieldTests =
struct
  open Support

  val one = Ui.Panel { dir = Layout.Column, gap = 6.0
                     , children = [ Ui.TextField { id = "tf", value = "HI" } ] }
  val two = Ui.Panel { dir = Layout.Column, gap = 6.0
                     , children = [ Ui.TextField { id = "tf1", value = "" }
                                  , Ui.TextField { id = "tf2", value = "" } ] }

  fun run () =
    let
      val csIdle = shot (220, 60) Ui.init noInput one
      (* focus by click, then type 'A' into the buffer *)
      val { state = st1, ... } = frameOf (220, 60) Ui.init (click (20, 12)) one
      val csFocused = shot (220, 60) st1 noInput one
      val { state = st2, events = evType, ... } = frameOf (220, 60) st1 (chars ["A"]) one

      (* FocusNext moves from tf1 to tf2 *)
      val stFocus1 = Ui.setFocus Ui.init (SOME "tf1")
      val { state = stNext, ... } = frameOf (240, 90) stFocus1 focusNext two
    in
      section "textfield";
      checkString "textfield idle golden"   ("FB1DD92D", csIdle);
      checkBool   "click sets focus" (true, Ui.focusOf st1 = SOME "tf");
      checkBool   "focused frame differs"   (true, csIdle <> csFocused);
      checkString "typing appends to buffer" ("tf:input=HIA", eventsStr evType);
      checkString "buffer retained in state" ("HIA",
        case Ui.textOf st2 "tf" of SOME s => s | NONE => "<none>");
      checkBool   "FocusNext tf1 -> tf2" (true, Ui.focusOf stNext = SOME "tf2")
    end
end
