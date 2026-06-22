(* scenes.sml  -  the SHARED widget-tree definitions behind the README
   screenshot showcase.  This single source of truth is compiled into BOTH the
   `make gallery` renderer (examples/gallery.sml, which writes the PNGs) AND the
   golden test suite (test/test_gallery.sml, which asserts a committed FNV-1a
   checksum per scene on MLton *and* Poly/ML).  Sharing the trees here means the
   bytes a reader sees in `assets/widget_*.png` are exactly the bytes the
   determinism tests lock down. *)

structure Scenes =
struct
  structure L = Layout

  fun row children = Ui.Panel { dir = L.Row, gap = 10.0, children = children }
  fun col gap children = Ui.Panel { dir = L.Column, gap = gap, children = children }

  (* ---- 1. buttons + checkboxes + radio group ---- *)
  val buttonsDim = (260, 260)
  val buttons =
    col 9.0
      [ Ui.Label "ACTIONS"
      , row [ Ui.Button { id = "ok", label = "OK" }
            , Ui.Button { id = "cancel", label = "CANCEL" } ]
      , Ui.Checkbox { id = "wrap", label = "WRAP TEXT", checked = true }
      , Ui.Checkbox { id = "grid", label = "SHOW GRID", checked = false }
      , Ui.Radio { id = "mode", label = "MODE"
                 , options = ["LIGHT", "DARK", "AUTO"], selected = 1 } ]

  (* ---- 2. slider + text field ---- *)
  val inputsDim = (300, 150)
  val inputs =
    col 10.0
      [ Ui.Label "PREFERENCES"
      , row [ Ui.Label "ZOOM"
            , Ui.Slider { id = "zoom", lo = 0.0, hi = 100.0, value = 70.0 } ]
      , row [ Ui.Label "NAME", Ui.TextField { id = "name", value = "ADA" } ] ]

  (* ---- 3. an open dropdown (popup overlay) ---- *)
  val dropdownDim = (220, 184)
  val dropdown =
    col 6.0
      [ Ui.Label "QUALITY"
      , Ui.Dropdown { id = "quality", options = ["LOW", "MEDIUM", "HIGH"]
                    , selected = 2, open_ = true } ]

  (* ---- 4. a tab strip switched to the second tab ---- *)
  val tabsDim = (280, 168)
  val tabPageA =
    col 6.0 [ Ui.Label "GENERAL"
            , Ui.Checkbox { id = "g1", label = "OPTION A", checked = true } ]
  val tabPageB =
    col 6.0 [ Ui.Label "AUDIO"
            , row [ Ui.Label "VOL"
                  , Ui.Slider { id = "vol", lo = 0.0, hi = 100.0, value = 55.0 } ] ]
  val tabs =
    col 6.0
      [ Ui.Tabs { id = "tabs", labels = ["GENERAL", "AUDIO"], active = 1
                , pages = [ tabPageA, tabPageB ] } ]

  (* ---- 5. a menu bar with the FILE menu open ----
     The open menu lives in retained `state`, so the scene is rendered in two
     passes: a click on the FILE title (x 0..64, bar height 26) records it, then
     the second pass with no input draws the popup overlay. *)
  val menuDim = (300, 190)
  val menu =
    col 6.0
      [ Ui.MenuBar { id = "menu"
                   , menus = [ ("FILE", ["NEW", "OPEN", "SAVE", "QUIT"])
                             , ("EDIT", ["UNDO", "REDO"])
                             , ("HELP", ["ABOUT"]) ] }
      , Ui.Label "MENU BAR DEMO" ]
  val menuOpenClick : Ui.frameinput =
    { mouse_x = 20.0, mouse_y = 12.0, mouse_down = false
    , keys = [ Ui.Mouse (Ui.Down (20.0, 12.0)), Ui.Mouse (Ui.Up (20.0, 12.0)) ] }

  (* ---- 6. a scroll container, scrolled partway down ---- *)
  val scrollDim = (210, 112)
  val scrollId = "sc"
  val scrollOffset = 24.0
  val scroll =
    Ui.Scroll { id = scrollId, height = 84.0
              , child = col 4.0 [ Ui.Label "ITEM ONE", Ui.Label "ITEM TWO"
                                , Ui.Label "ITEM THREE", Ui.Label "ITEM FOUR"
                                , Ui.Label "ITEM FIVE", Ui.Label "ITEM SIX" ] }
end
