(* test_modal.sml  -  Modal renders centered window chrome over a dimmed
   backdrop, fires a close event from its title-bar X, and TRAPS clicks so
   widgets beneath an open modal do not react. *)

structure ModalTests =
struct
  open Support

  val body = Ui.Panel { dir = Layout.Column, gap = 6.0
                      , children = [ Ui.Label "BODY", Ui.Button { id = "okb", label = "OK" } ] }
  fun modal open_ = Ui.Modal { id = "m", title = "HELLO", open_ = open_, child = body }

  (* a base button behind a (possibly open) modal, to prove click-trapping *)
  fun behind open_ =
    Ui.Panel { dir = Layout.Column, gap = 6.0
             , children = [ Ui.Button { id = "bg", label = "BG" }
                          , Ui.Modal { id = "m2", title = "T", open_ = open_
                                     , child = Ui.Label "X" } ] }

  (* window centers at winX=40, winY=40, winW=160, winH=100;
     close box x 174..200, y 40..66 *)
  fun run () =
    let
      val csClosed = shot (240, 180) Ui.init noInput (modal false)
      val csOpen   = shot (240, 180) Ui.init noInput (modal true)
      val { events = evClose, ... } = frameOf (240, 180) Ui.init (click (185, 52)) (modal true)
      val { events = evTrapped, ... } = frameOf (240, 180) Ui.init (click (10, 12)) (behind true)
      val { events = evLive, ... }    = frameOf (240, 180) Ui.init (click (10, 12)) (behind false)
    in
      section "modal";
      checkString "modal closed golden" ("9FB949D1", csClosed);
      checkString "modal open golden"   ("E0F133B3", csOpen);
      checkBool   "open differs from closed" (true, csClosed <> csOpen);
      checkString "close button fires close" ("m:close=", eventsStr evClose);
      checkInt    "open modal traps base clicks" (0, length evTrapped);
      checkString "closed modal lets base click through" ("bg:click=", eventsStr evLive)
    end
end
