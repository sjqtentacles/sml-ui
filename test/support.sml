(* support.sml - shared helpers for the sml-ui test suite.

   The core correctness strategy is GOLDEN CHECKSUMS over rendered frames: a
   widget tree is rendered to an `Image.image`, and a deterministic 32-bit
   FNV-1a hash of (width, height, RGBA bytes) is compared against a committed
   constant.  Because every vendored backend is byte-identical across MLton and
   Poly/ML, the same checksum is produced by both compilers - which is what
   makes the toolkit unit-testable headlessly and catches cross-compiler drift. *)

structure Support =
struct
  open Harness

  fun readFile path =
    let val ins = TextIO.openIn path
        val s = TextIO.inputAll ins
    in TextIO.closeIn ins; s end

  (* Parsed once; the committed 5x7 BDF the canvas built-in font mirrors. *)
  val font = Font.parseBdf (readFile "data/font5x7.bdf")

  (* FNV-1a over the image dimensions and pixel bytes -> canonical 8-hex digest.
     Deterministic and byte-identical across compilers. *)
  fun digest ({ width, height, data } : Image.image) =
    let
      val prime = 0wx01000193 : Word32.word
      fun step (b, h) =
        Word32.* (Word32.xorb (h, Word32.fromInt (Word8.toInt b)), prime)
      val h0 = 0wx811c9dc5 : Word32.word
      fun mix (n, h) = step (Word8.fromInt (Int.rem (n, 256)),
                             step (Word8.fromInt (Int.rem (Int.quot (n, 256), 256)), h))
      val h1 = mix (width, mix (height, h0))
      val h = Word8Vector.foldl step h1 data
      val s = Word32.toString h
    in StringCvt.padLeft #"0" 8 s end

  (* A standard 320x180 config + neutral theme used by most golden frames. *)
  val theme = Ui.defaultTheme
  val noInput = Ui.noInput
  fun config (w, h) = { width = w, height = h, font = font, theme = theme }

  (* Render a frame and return its checksum. *)
  fun shot (w, h) st input widget =
    let val { image, ... } = Ui.render (config (w, h)) st input widget
    in digest image end

  fun checkShot name (expected, (w, h), st, input, widget) =
    checkString name (expected, shot (w, h) st input widget)

  (* helpers to build inputs *)
  fun click (xi, yi) =
    let val (x, y) = (real xi, real yi)
    in { mouse_x = x, mouse_y = y, mouse_down = false,
         keys = [Ui.Mouse (Ui.Down (x, y)), Ui.Mouse (Ui.Up (x, y))] } : Ui.frameinput end
  fun press (xi, yi) =
    let val (x, y) = (real xi, real yi)
    in { mouse_x = x, mouse_y = y, mouse_down = true,
         keys = [Ui.Mouse (Ui.Down (x, y))] } : Ui.frameinput end
  fun hover (xi, yi) =
    let val (x, y) = (real xi, real yi)
    in { mouse_x = x, mouse_y = y, mouse_down = false,
         keys = [Ui.Mouse (Ui.Move (x, y))] } : Ui.frameinput end
  fun chars cs =
    { mouse_x = ~1.0, mouse_y = ~1.0, mouse_down = false,
      keys = map Ui.Char cs } : Ui.frameinput
  val focusNext =
    { mouse_x = ~1.0, mouse_y = ~1.0, mouse_down = false,
      keys = [Ui.FocusNext] } : Ui.frameinput

  (* full render result (state + events + checksum) for stateful flows *)
  fun frameOf (w, h) st input widget =
    let val { state, image, events } = Ui.render (config (w, h)) st input widget
    in { state = state, events = events, digest = digest image } end

  fun eventStr ({ id, kind, value } : Ui.event) = id ^ ":" ^ kind ^ "=" ^ value
  fun eventsStr es = String.concatWith ";" (map eventStr es)
end
