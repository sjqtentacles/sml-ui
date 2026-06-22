(* gallery.sml  -  render the per-widget / per-state screenshot showcase to
   assets/widget_*.png.  Every scene comes from the shared `Scenes` structure so
   the committed PNGs are exactly what the golden tests in test/test_gallery.sml
   pin down; the printed FNV-1a digests are the constants those tests assert.
   Pure and deterministic: byte-identical across MLton and Poly/ML.  Run with
   `make gallery`. *)

structure Gallery =
struct
  fun readFile p =
    let val i = TextIO.openIn p val s = TextIO.inputAll i
    in TextIO.closeIn i; s end

  val font = Font.parseBdf (readFile "data/font5x7.bdf")
  fun cfg (w, h) = { width = w, height = h, font = font, theme = Ui.defaultTheme }

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

  (* Render a static scene (init state, no input). *)
  fun renderStatic (w, h) st input widget =
    let val { image, ... } = Ui.render (cfg (w, h)) st input widget in image end

  fun emit (name, (w, h), img) =
    ( writePng ("assets/" ^ name ^ ".png", img)
    ; print (StringCvt.padRight #" " 18 (name ^ ".png")
             ^ Int.toString w ^ "x" ^ Int.toString h
             ^ "  fnv1a=" ^ digest img ^ "\n") )

  fun main () =
    let
      open Scenes
      val (bw, bh) = buttonsDim
      val imgButtons = renderStatic (bw, bh) Ui.init Ui.noInput buttons
      val (iw, ih) = inputsDim
      val imgInputs = renderStatic (iw, ih) Ui.init Ui.noInput inputs
      val (dw, dh) = dropdownDim
      val imgDropdown = renderStatic (dw, dh) Ui.init Ui.noInput dropdown
      val (tw, th) = tabsDim
      val imgTabs = renderStatic (tw, th) Ui.init Ui.noInput tabs
      val (mw, mh) = menuDim
      val { state = stMenu, ... } = Ui.render (cfg (mw, mh)) Ui.init menuOpenClick menu
      val imgMenu = renderStatic (mw, mh) stMenu Ui.noInput menu
      val (sw, sh) = scrollDim
      val stScroll = Ui.setScroll Ui.init scrollId scrollOffset
      val imgScroll = renderStatic (sw, sh) stScroll Ui.noInput scroll
    in
      emit ("widget_buttons",  (bw, bh), imgButtons);
      emit ("widget_inputs",   (iw, ih), imgInputs);
      emit ("widget_dropdown", (dw, dh), imgDropdown);
      emit ("widget_tabs",     (tw, th), imgTabs);
      emit ("widget_menu",     (mw, mh), imgMenu);
      emit ("widget_scroll",   (sw, sh), imgScroll)
    end
end

val () = Gallery.main ()
