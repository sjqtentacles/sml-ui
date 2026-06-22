(* demo.sml  -  render a full "settings window" UI to assets/ui.png plus a modal
   dialog to assets/ui_modal.png.  Pure and deterministic: the exact bytes (and
   the FNV-1a digests printed below) are byte-identical across MLton and
   Poly/ML.  Run with `make example`. *)

structure Demo =
struct
  structure C = Canvas2d

  fun readFile p =
    let val i = TextIO.openIn p val s = TextIO.inputAll i
    in TextIO.closeIn i; s end

  val font = Font.parseBdf (readFile "data/font5x7.bdf")
  val cfg = { width = 540, height = 470, font = font, theme = Ui.defaultTheme }

  fun row children = Ui.Panel { dir = Layout.Row, gap = 10.0, children = children }
  fun col children = Ui.Panel { dir = Layout.Column, gap = 10.0, children = children }

  val general = Ui.Panel
    { dir = Layout.Column, gap = 9.0
    , children =
        [ Ui.Label "DISPLAY"
        , Ui.Checkbox { id = "wrap", label = "WRAP TEXT", checked = true }
        , Ui.Checkbox { id = "grid", label = "SHOW GRID", checked = false }
        , Ui.Radio { id = "theme", label = "MODE"
                   , options = ["LIGHT", "DARK", "AUTO"], selected = 1 }
        , row [ Ui.Label "ZOOM"
              , Ui.Slider { id = "zoom", lo = 0.0, hi = 100.0, value = 70.0 } ]
        , row [ Ui.Label "NAME", Ui.TextField { id = "name", value = "ADA" } ]
        , row [ Ui.Label "QUALITY"
              , Ui.Dropdown { id = "quality", options = ["LOW", "MEDIUM", "HIGH"]
                            , selected = 2, open_ = true } ]
        , row [ Ui.Button { id = "ok", label = "OK" }
              , Ui.Button { id = "cancel", label = "CANCEL" } ] ] }

  val audio = Ui.Panel
    { dir = Layout.Column, gap = 9.0
    , children =
        [ Ui.Label "AUDIO"
        , row [ Ui.Label "MASTER"
              , Ui.Slider { id = "master", lo = 0.0, hi = 100.0, value = 40.0 } ]
        , row [ Ui.Label "MUSIC"
              , Ui.Slider { id = "music", lo = 0.0, hi = 100.0, value = 80.0 } ]
        , Ui.Checkbox { id = "mute", label = "MUTE ALL", checked = false } ] }

  val tree = Ui.Panel
    { dir = Layout.Column, gap = 8.0
    , children =
        [ Ui.MenuBar { id = "menu"
                     , menus = [ ("FILE", ["NEW", "OPEN", "SAVE", "QUIT"])
                               , ("EDIT", ["UNDO", "REDO"])
                               , ("HELP", ["ABOUT"]) ] }
        , Ui.Tabs { id = "tabs", labels = ["GENERAL", "AUDIO"], active = 0
                  , pages = [ general, audio ] } ] }

  val modalTree = Ui.Modal
    { id = "about", title = "ABOUT SML-UI", open_ = true
    , child = Ui.Panel { dir = Layout.Column, gap = 8.0
                       , children = [ Ui.Label "SML-UI 1.0"
                                    , Ui.Label "A PURE STANDARD ML GUI TOOLKIT"
                                    , Ui.Button { id = "close", label = "CLOSE" } ] } }

  (* FNV-1a digest of the rendered pixels (matches the test harness). *)
  fun digest ({ width, height, data } : Image.image) =
    let
      val prime = 0wx01000193 : Word32.word
      fun step (b, h) = Word32.* (Word32.xorb (h, Word32.fromInt (Word8.toInt b)), prime)
      fun mix (n, h) = step (Word8.fromInt (Int.rem (n, 256)),
                             step (Word8.fromInt (Int.rem (Int.quot (n, 256), 256)), h))
      val h1 = mix (width, mix (height, 0wx811c9dc5 : Word32.word))
    in StringCvt.padLeft #"0" 8 (Word32.toString (Word8Vector.foldl step h1 data)) end

  fun writePng (path, img) =
    let val os = BinIO.openOut path
    in BinIO.output (os, Image.encodePng img); BinIO.closeOut os end

  fun main () =
    let
      val { image = main_img, ... } = Ui.render cfg Ui.init Ui.noInput tree
      val { image = modal_img, ... } = Ui.render cfg Ui.init Ui.noInput modalTree
    in
      writePng ("assets/ui.png", main_img);
      writePng ("assets/ui_modal.png", modal_img);
      print ("ui.png        " ^ Int.toString (#width main_img) ^ "x"
             ^ Int.toString (#height main_img) ^ "  fnv1a=" ^ digest main_img ^ "\n");
      print ("ui_modal.png  " ^ Int.toString (#width modal_img) ^ "x"
             ^ Int.toString (#height modal_img) ^ "  fnv1a=" ^ digest modal_img ^ "\n")
    end
end

val () = Demo.main ()
