(* test_menubar.sml  -  MenuBar opens a menu on a title click and fires a
   {kind="menu"} event on item selection; open menu is an overlay. *)

structure MenuBarTests =
struct
  open Support

  val mb = Ui.MenuBar { id = "mb"
                      , menus = [ ("FILE", ["OPEN", "SAVE", "QUIT"])
                                , ("EDIT", ["COPY", "PASTE"]) ] }
  val w = Ui.Panel { dir = Layout.Column, gap = 6.0, children = [ mb ] }

  (* titles: FILE x 0..64, EDIT x 64..128 (bar height 26).
     FILE's items popup: OPEN 26..52, SAVE 52..78, QUIT 78..104 *)
  fun run () =
    let
      val csClosed = shot (240, 140) Ui.init noInput w
      (* click FILE to open the menu (engine records it in state) *)
      val { state = st1, ... } = frameOf (240, 140) Ui.init (click (20, 12)) w
      val csOpen = shot (240, 140) st1 noInput w
      (* with FILE open, click SAVE item *)
      val { events = evSave, state = st2, ... } =
        frameOf (240, 140) st1 (click (20, 65)) w
      (* clicking FILE again closes it *)
      val { state = st3, ... } = frameOf (240, 140) st1 (click (20, 12)) w
    in
      section "menubar";
      checkString "menubar closed golden" ("8513B0A1", csClosed);
      checkString "menubar open golden"   ("AF4AEE90", csOpen);
      checkBool   "open differs from closed" (true, csClosed <> csOpen);
      checkBool   "title click opens menu" (true, Ui.menuOf st1 = SOME "FILE");
      checkString "item click fires menu event" ("mb:menu=SAVE", eventsStr evSave);
      checkBool   "item click closes menu" (true, Ui.menuOf st2 = NONE);
      checkBool   "re-click title closes menu" (true, Ui.menuOf st3 = NONE)
    end
end
