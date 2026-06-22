(* test_gallery.sml  -  golden pixel-checksum tests for the README screenshot
   showcase rendered by `make gallery`.  Each scene comes from the shared
   `Scenes` structure (examples/scenes.sml) that the gallery renderer also uses,
   so these committed FNV-1a digests are exactly the bytes in assets/widget_*.png
   - and asserting them on BOTH MLton and Poly/ML catches any cross-compiler
   drift in a committed screenshot. *)

structure GalleryTests =
struct
  open Support

  fun run () =
    let
      val (bw, bh) = Scenes.buttonsDim
      val csButtons = shot (bw, bh) Ui.init noInput Scenes.buttons
      val (iw, ih) = Scenes.inputsDim
      val csInputs = shot (iw, ih) Ui.init noInput Scenes.inputs
      val (dw, dh) = Scenes.dropdownDim
      val csDropdown = shot (dw, dh) Ui.init noInput Scenes.dropdown
      val (tw, th) = Scenes.tabsDim
      val csTabs = shot (tw, th) Ui.init noInput Scenes.tabs
      (* menu scene: open FILE via a click frame, then render the popup overlay *)
      val (mw, mh) = Scenes.menuDim
      val { state = stMenu, ... } = frameOf (mw, mh) Ui.init Scenes.menuOpenClick Scenes.menu
      val csMenu = shot (mw, mh) stMenu noInput Scenes.menu
      (* scroll scene: retained offset *)
      val (sw, sh) = Scenes.scrollDim
      val stScroll = Ui.setScroll Ui.init Scenes.scrollId Scenes.scrollOffset
      val csScroll = shot (sw, sh) stScroll noInput Scenes.scroll
    in
      section "gallery";
      checkBool   "menu scene actually opens FILE" (true, Ui.menuOf stMenu = SOME "FILE");
      checkString "widget_buttons golden"  ("E384D2F5", csButtons);
      checkString "widget_inputs golden"   ("BF0D75C1", csInputs);
      checkString "widget_dropdown golden" ("9A7FEFD8", csDropdown);
      checkString "widget_tabs golden"     ("C4BBB92F", csTabs);
      checkString "widget_menu golden"     ("CBE97CA3", csMenu);
      checkString "widget_scroll golden"   ("82D6EABF", csScroll)
    end
end
