(* canvas2d.sml

   Implementation of `signature CANVAS2D`.

   A scene is folded into either an `Image.image` (via sml-raster primitives) or
   an SVG string (built directly, reusing `Svg.fmtReal` for every coordinate).
   Transforms are 2x3 affines; in both backends the current transform is BAKED
   into coordinates rather than emitted as a separate transform attribute, which
   keeps the two backends consistent and deterministic. *)

structure Canvas2d :> CANVAS2D =
struct

  type color = Color.rgba
  type rect  = { x : real, y : real, w : real, h : real }

  (* ---- affine transforms: x' = a*x + c*y + e, y' = b*x + d*y + f ---- *)
  type xform = { a : real, b : real, c : real, d : real, e : real, f : real }

  val identity : xform = { a = 1.0, b = 0.0, c = 0.0, d = 1.0, e = 0.0, f = 0.0 }
  fun translate (tx, ty) : xform = { a = 1.0, b = 0.0, c = 0.0, d = 1.0, e = tx, f = ty }
  fun scale (sx, sy) : xform = { a = sx, b = 0.0, c = 0.0, d = sy, e = 0.0, f = 0.0 }
  fun rotate th : xform =
    let val co = Math.cos th and si = Math.sin th
    in { a = co, b = si, c = ~si, d = co, e = 0.0, f = 0.0 } end

  (* compose (m1, m2) = matrix product m1 * m2 (apply m2, then m1). *)
  fun compose (m1 : xform, m2 : xform) : xform =
    { a = #a m1 * #a m2 + #c m1 * #b m2
    , b = #b m1 * #a m2 + #d m1 * #b m2
    , c = #a m1 * #c m2 + #c m1 * #d m2
    , d = #b m1 * #c m2 + #d m1 * #d m2
    , e = #a m1 * #e m2 + #c m1 * #f m2 + #e m1
    , f = #b m1 * #e m2 + #d m1 * #f m2 + #f m1 }

  fun apply (m : xform) (x, y) =
    (#a m * x + #c m * y + #e m, #b m * x + #d m * y + #f m)

  fun axisAligned (m : xform) = Real.== (#b m, 0.0) andalso Real.== (#c m, 0.0)

  (* ---- path + command datatypes ---- *)
  datatype pathseg =
      MoveTo  of real * real
    | LineTo  of real * real
    | QuadTo  of (real * real) * (real * real)
    | CubicTo of (real * real) * (real * real) * (real * real)
    | Close

  datatype cmd =
      Save | Restore
    | SetTransform of xform
    | Transform of xform
    | Fill of { path : pathseg list, color : color }
    | Stroke of { path : pathseg list, color : color, width : real }
    | FillRect of rect * color
    | Text of { x : real, y : real, text : string, color : color, scale : int }
    | Clip of rect

  type scene = cmd list

  val fmtReal = Svg.fmtReal

  (* ---- helpers ---- *)
  (* Round-half-up via floor: unlike Real.round (whose round-to-even tie rule
     can differ across compilers on exact .5 coordinates from bezier
     flattening), floor (x + 0.5) is computed identically on MLton and Poly/ML,
     keeping rasterized pixels byte-identical. *)
  fun iround r = Real.floor (r + 0.5)

  fun toRgba8 (col : color) : Image.rgba8 =
    let
      val w = Color.pack col   (* 0xRRGGBBAA *)
      fun byteAt sh = Word8.fromInt (Word32.toInt (Word32.andb (Word32.>> (w, sh), 0wxFF)))
    in
      { r = byteAt 0w24, g = byteAt 0w16, b = byteAt 0w8, a = byteAt 0w0 }
    end

  fun minR (a, b) = if a < b then a else b
  fun maxR (a, b) = if a > b then a else b
  fun maxI (a, b) = if a > b then a else b
  fun minI (a, b) = if a < b then a else b

  (* ---- bezier flattening (fixed subdivision -> deterministic) ---- *)
  val flatN = 24

  fun quadPts ((x0, y0), (cx, cy), (ex, ey)) =
    List.tabulate (flatN, fn i =>
      let val t = real (i + 1) / real flatN
          val u = 1.0 - t
      in ( u * u * x0 + 2.0 * u * t * cx + t * t * ex
         , u * u * y0 + 2.0 * u * t * cy + t * t * ey ) end)

  fun cubicPts ((x0, y0), (c1x, c1y), (c2x, c2y), (ex, ey)) =
    List.tabulate (flatN, fn i =>
      let val t = real (i + 1) / real flatN
          val u = 1.0 - t
      in ( u*u*u*x0 + 3.0*u*u*t*c1x + 3.0*u*t*t*c2x + t*t*t*ex
         , u*u*u*y0 + 3.0*u*u*t*c1y + 3.0*u*t*t*c2y + t*t*t*ey ) end)

  (* Flatten a path to a list of device-space (transformed) points. *)
  fun flatten (xf : xform) segs =
    let
      fun tp p = apply xf p
      fun go _ acc [] = List.rev acc
        | go cur acc (s :: rest) =
            (case s of
                 MoveTo p => go p (tp p :: acc) rest
               | LineTo p => go p (tp p :: acc) rest
               | QuadTo (c, e) =>
                   go e (List.revAppend (List.map tp (quadPts (cur, c, e)), acc)) rest
               | CubicTo (c1, c2, e) =>
                   go e (List.revAppend (List.map tp (cubicPts (cur, c1, c2, e)), acc)) rest
               | Close => go cur acc rest)
    in
      go (0.0, 0.0) [] segs
    end

  (* ---- clip rect: inclusive int bounds ---- *)
  type bounds = { x0 : int, y0 : int, x1 : int, y1 : int }

  fun intersectB (p : bounds, q : bounds) : bounds =
    { x0 = maxI (#x0 p, #x0 q), y0 = maxI (#y0 p, #y0 q)
    , x1 = minI (#x1 p, #x1 q), y1 = minI (#y1 p, #y1 q) }

  (* device int bounds of a transformed (axis-aligned) rect *)
  fun rectBounds (xf : xform) ({ x, y, w, h } : rect) : bounds =
    let
      val pts = List.map (apply xf) [ (x, y), (x + w, y), (x + w, y + h), (x, y + h) ]
      val xs = List.map #1 pts and ys = List.map #2 pts
      val minx = List.foldl minR (hd xs) (tl xs)
      val maxx = List.foldl maxR (hd xs) (tl xs)
      val miny = List.foldl minR (hd ys) (tl ys)
      val maxy = List.foldl maxR (hd ys) (tl ys)
    in
      { x0 = iround minx, y0 = iround miny, x1 = iround maxx - 1, y1 = iround maxy - 1 }
    end

  (* ---- built-in 5x7 bitmap font ---- *)
  fun glyphData c =
    case c of
        #" " => SOME ("....." ^ "....." ^ "....." ^ "....." ^ "....." ^ "....." ^ ".....")
      | #"A" => SOME (".###." ^ "#...#" ^ "#...#" ^ "#####" ^ "#...#" ^ "#...#" ^ "#...#")
      | #"B" => SOME ("####." ^ "#...#" ^ "#...#" ^ "####." ^ "#...#" ^ "#...#" ^ "####.")
      | #"C" => SOME (".###." ^ "#...#" ^ "#...." ^ "#...." ^ "#...." ^ "#...#" ^ ".###.")
      | #"D" => SOME ("###.." ^ "#..#." ^ "#...#" ^ "#...#" ^ "#...#" ^ "#..#." ^ "###..")
      | #"E" => SOME ("#####" ^ "#...." ^ "#...." ^ "####." ^ "#...." ^ "#...." ^ "#####")
      | #"F" => SOME ("#####" ^ "#...." ^ "#...." ^ "####." ^ "#...." ^ "#...." ^ "#....")
      | #"G" => SOME (".###." ^ "#...#" ^ "#...." ^ "#.###" ^ "#...#" ^ "#...#" ^ ".###.")
      | #"H" => SOME ("#...#" ^ "#...#" ^ "#...#" ^ "#####" ^ "#...#" ^ "#...#" ^ "#...#")
      | #"I" => SOME (".###." ^ "..#.." ^ "..#.." ^ "..#.." ^ "..#.." ^ "..#.." ^ ".###.")
      | #"J" => SOME ("..###" ^ "...#." ^ "...#." ^ "...#." ^ "#..#." ^ "#..#." ^ ".##..")
      | #"K" => SOME ("#...#" ^ "#..#." ^ "#.#.." ^ "##..." ^ "#.#.." ^ "#..#." ^ "#...#")
      | #"L" => SOME ("#...." ^ "#...." ^ "#...." ^ "#...." ^ "#...." ^ "#...." ^ "#####")
      | #"M" => SOME ("#...#" ^ "##.##" ^ "#.#.#" ^ "#.#.#" ^ "#...#" ^ "#...#" ^ "#...#")
      | #"N" => SOME ("#...#" ^ "#...#" ^ "##..#" ^ "#.#.#" ^ "#..##" ^ "#...#" ^ "#...#")
      | #"O" => SOME (".###." ^ "#...#" ^ "#...#" ^ "#...#" ^ "#...#" ^ "#...#" ^ ".###.")
      | #"P" => SOME ("####." ^ "#...#" ^ "#...#" ^ "####." ^ "#...." ^ "#...." ^ "#....")
      | #"Q" => SOME (".###." ^ "#...#" ^ "#...#" ^ "#...#" ^ "#.#.#" ^ "#..#." ^ ".##.#")
      | #"R" => SOME ("####." ^ "#...#" ^ "#...#" ^ "####." ^ "#.#.." ^ "#..#." ^ "#...#")
      | #"S" => SOME (".###." ^ "#...#" ^ "#...." ^ ".###." ^ "....#" ^ "#...#" ^ ".###.")
      | #"T" => SOME ("#####" ^ "..#.." ^ "..#.." ^ "..#.." ^ "..#.." ^ "..#.." ^ "..#..")
      | #"U" => SOME ("#...#" ^ "#...#" ^ "#...#" ^ "#...#" ^ "#...#" ^ "#...#" ^ ".###.")
      | #"V" => SOME ("#...#" ^ "#...#" ^ "#...#" ^ "#...#" ^ "#...#" ^ ".#.#." ^ "..#..")
      | #"W" => SOME ("#...#" ^ "#...#" ^ "#...#" ^ "#.#.#" ^ "#.#.#" ^ "##.##" ^ "#...#")
      | #"X" => SOME ("#...#" ^ "#...#" ^ ".#.#." ^ "..#.." ^ ".#.#." ^ "#...#" ^ "#...#")
      | #"Y" => SOME ("#...#" ^ "#...#" ^ ".#.#." ^ "..#.." ^ "..#.." ^ "..#.." ^ "..#..")
      | #"Z" => SOME ("#####" ^ "....#" ^ "...#." ^ "..#.." ^ ".#..." ^ "#...." ^ "#####")
      | #"0" => SOME (".###." ^ "#...#" ^ "#..##" ^ "#.#.#" ^ "##..#" ^ "#...#" ^ ".###.")
      | #"1" => SOME ("..#.." ^ ".##.." ^ "..#.." ^ "..#.." ^ "..#.." ^ "..#.." ^ ".###.")
      | #"2" => SOME (".###." ^ "#...#" ^ "....#" ^ "...#." ^ "..#.." ^ ".#..." ^ "#####")
      | #"3" => SOME ("#####" ^ "...#." ^ "..#.." ^ "...#." ^ "....#" ^ "#...#" ^ ".###.")
      | #"4" => SOME ("...#." ^ "..##." ^ ".#.#." ^ "#..#." ^ "#####" ^ "...#." ^ "...#.")
      | #"5" => SOME ("#####" ^ "#...." ^ "####." ^ "....#" ^ "....#" ^ "#...#" ^ ".###.")
      | #"6" => SOME (".###." ^ "#...#" ^ "#...." ^ "####." ^ "#...#" ^ "#...#" ^ ".###.")
      | #"7" => SOME ("#####" ^ "....#" ^ "...#." ^ "..#.." ^ ".#..." ^ ".#..." ^ ".#...")
      | #"8" => SOME (".###." ^ "#...#" ^ "#...#" ^ ".###." ^ "#...#" ^ "#...#" ^ ".###.")
      | #"9" => SOME (".###." ^ "#...#" ^ "#...#" ^ ".####" ^ "....#" ^ "#...#" ^ ".###.")
      | #"-" => SOME ("....." ^ "....." ^ "....." ^ "#####" ^ "....." ^ "....." ^ ".....")
      | #"." => SOME ("....." ^ "....." ^ "....." ^ "....." ^ "....." ^ ".##.." ^ ".##..")
      | #":" => SOME ("....." ^ ".##.." ^ ".##.." ^ "....." ^ ".##.." ^ ".##.." ^ ".....")
      | _ => NONE

  fun foldChar c =
    if Char.isLower c then Char.toUpper c else c

  (* ---- raster backend ---- *)
  fun fillRectClipped img (clip : bounds) (left, top, right, bot) col8 =
    (* left..right, top..bot inclusive, clipped to `clip` *)
    let
      val l = maxI (left, #x0 clip)  and t = maxI (top, #y0 clip)
      val r = minI (right, #x1 clip) and b = minI (bot, #y1 clip)
    in
      if r < l orelse b < t then img
      else Raster.fillRect img { x = l, y = t, w = r - l + 1, h = b - t + 1 } col8
    end

  fun drawGlyph img clip (gx, gy) scale col8 rows =
    let
      fun row (ri, img) =
        let
          fun col (ci, img) =
            if String.sub (rows, ri * 5 + ci) = #"#" then
              fillRectClipped img clip
                (gx + ci * scale, gy + ri * scale,
                 gx + ci * scale + scale - 1, gy + ri * scale + scale - 1) col8
            else img
          fun loopC (ci, img) = if ci >= 5 then img else loopC (ci + 1, col (ci, img))
        in loopC (0, img) end
      fun loopR (ri, img) = if ri >= 7 then img else loopR (ri + 1, row (ri, img))
    in loopR (0, img) end

  fun textRaster img clip (xf : xform) { x, y, text, color, scale } =
    let
      val (ox0, oy0) = apply xf (x, y)
      val ox = iround ox0 and oy = iround oy0
      val col8 = toRgba8 color
      val sc = maxI (1, scale)
      fun loop img (cx, cy) [] = img
        | loop img (cx, cy) (c :: cs) =
            if c = #"\n" then loop img (ox, cy + 8 * sc) cs
            else
              (case glyphData (foldChar c) of
                   SOME rows => loop (drawGlyph img clip (cx, cy) sc col8 rows)
                                     (cx + 6 * sc, cy) cs
                 | NONE => loop img (cx + 6 * sc, cy) cs)
    in loop img (ox, oy) (String.explode text) end

  fun fillPathRaster img (xf : xform) path col8 =
    let
      val pts = List.map (fn (x, y) => (iround x, iround y)) (flatten xf path)
    in
      case pts of
          [a, b, c] => Raster.fillTriangle img (a, b, c) col8
        | _ => Raster.fillPolygon img pts col8
    end

  fun strokePathRaster img (xf : xform) path col8 width =
    let
      val pts = List.map (fn (x, y) => (iround x, iround y)) (flatten xf path)
      val w = maxI (1, iround width)
      val hw = (w - 1) div 2
      fun seg img ((x0, y0), (x1, y1)) =
        if w <= 1 then Raster.line img { x0 = x0, y0 = y0, x1 = x1, y1 = y1 } col8
        else
          let
            fun dxLoop (dx, img) =
              if dx > hw then img
              else
                let
                  fun dyLoop (dy, img) =
                    if dy > hw then img
                    else dyLoop (dy + 1,
                      Raster.line img
                        { x0 = x0 + dx, y0 = y0 + dy, x1 = x1 + dx, y1 = y1 + dy } col8)
                in dxLoop (dx + 1, dyLoop (~hw, img)) end
          in dxLoop (~hw, img) end
      fun walk img (a :: b :: rest) = walk (seg img (a, b)) (b :: rest)
        | walk img _ = img
    in walk img pts end

  fun fillRectRaster img (xf : xform) clip (r : rect) col8 =
    if axisAligned xf then
      let val { x0, y0, x1, y1 } = rectBounds xf r
      in fillRectClipped img clip (x0, y0, x1, y1) col8 end
    else
      let
        val { x, y, w, h } = r
        val pts = List.map (fn (px, py) => let val (a, b) = apply xf (px, py)
                                           in (iround a, iround b) end)
                    [ (x, y), (x + w, y), (x + w, y + h), (x, y + h) ]
      in Raster.fillPolygon img pts col8 end

  fun toImage { width, height, background } scene =
    let
      val bg8 = toRgba8 background
      val full : bounds = { x0 = 0, y0 = 0, x1 = width - 1, y1 = height - 1 }
      fun step (cmd, (img, xf, clip, stk)) =
        case cmd of
            Save => (img, xf, clip, (xf, clip) :: stk)
          | Restore =>
              (case stk of
                   (x, c) :: r => (img, x, c, r)
                 | [] => (img, xf, clip, stk))
          | SetTransform t => (img, t, clip, stk)
          | Transform t => (img, compose (xf, t), clip, stk)
          | Clip r => (img, xf, intersectB (clip, rectBounds xf r), stk)
          | FillRect (r, col) => (fillRectRaster img xf clip r (toRgba8 col), xf, clip, stk)
          | Fill { path, color } => (fillPathRaster img xf path (toRgba8 color), xf, clip, stk)
          | Stroke { path, color, width } =>
              (strokePathRaster img xf path (toRgba8 color) width, xf, clip, stk)
          | Text t => (textRaster img clip xf t, xf, clip, stk)
      val (img, _, _, _) =
        List.foldl step (Raster.blank (width, height) bg8, identity, full, []) scene
    in img end

  (* ---- SVG backend ---- *)
  fun pathD close pts =
    case pts of
        [] => ""
      | (x0, y0) :: rest =>
          "M " ^ fmtReal x0 ^ " " ^ fmtReal y0
          ^ String.concat (List.map (fn (x, y) => " L " ^ fmtReal x ^ " " ^ fmtReal y) rest)
          ^ (if close then " Z" else "")

  fun rectSvg (xf : xform) ({ x, y, w, h } : rect) col =
    if axisAligned xf then
      let
        val (x0, y0) = apply xf (x, y)
        val (x1, y1) = apply xf (x + w, y + h)
        val lx = minR (x0, x1) and ly = minR (y0, y1)
        val ww = Real.abs (x1 - x0) and hh = Real.abs (y1 - y0)
      in
        "<rect x=\"" ^ fmtReal lx ^ "\" y=\"" ^ fmtReal ly
        ^ "\" width=\"" ^ fmtReal ww ^ "\" height=\"" ^ fmtReal hh
        ^ "\" fill=\"" ^ Color.toHex col ^ "\"/>"
      end
    else
      let
        val pts = List.map (apply xf) [ (x, y), (x + w, y), (x + w, y + h), (x, y + h) ]
      in
        "<path d=\"" ^ pathD true pts ^ "\" fill=\"" ^ Color.toHex col ^ "\"/>"
      end

  fun fillSvg (xf : xform) path col =
    "<path d=\"" ^ pathD true (flatten xf path) ^ "\" fill=\"" ^ Color.toHex col ^ "\"/>"

  fun strokeSvg (xf : xform) path col width =
    "<path d=\"" ^ pathD false (flatten xf path)
    ^ "\" fill=\"none\" stroke=\"" ^ Color.toHex col
    ^ "\" stroke-width=\"" ^ fmtReal width ^ "\"/>"

  fun textSvg (xf : xform) { x, y, text, color, scale } =
    let
      val (ox, oy) = apply xf (x, y)
      fun esc s =
        String.translate
          (fn #"&" => "&amp;" | #"<" => "&lt;" | #">" => "&gt;" | c => String.str c) s
    in
      "<text x=\"" ^ fmtReal ox ^ "\" y=\"" ^ fmtReal oy
      ^ "\" font-family=\"monospace\" font-size=\"" ^ Int.toString (7 * scale)
      ^ "\" fill=\"" ^ Color.toHex color ^ "\">" ^ esc text ^ "</text>"
    end

  fun toSvg { width, height } scene =
    let
      fun step (cmd, (xf, stk, acc)) =
        case cmd of
            Save => (xf, xf :: stk, acc)
          | Restore => (case stk of x :: r => (x, r, acc) | [] => (xf, stk, acc))
          | SetTransform t => (t, stk, acc)
          | Transform t => (compose (xf, t), stk, acc)
          | Clip _ => (xf, stk, acc)
          | FillRect (r, col) => (xf, stk, rectSvg xf r col :: acc)
          | Fill { path, color } => (xf, stk, fillSvg xf path color :: acc)
          | Stroke { path, color, width } => (xf, stk, strokeSvg xf path color width :: acc)
          | Text t => (xf, stk, textSvg xf t :: acc)
      val (_, _, acc) = List.foldl step (identity, [], []) scene
      val lines = List.rev acc
      val w = Int.toString width and h = Int.toString height
      val header =
        "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"" ^ w
        ^ "\" height=\"" ^ h ^ "\" viewBox=\"0 0 " ^ w ^ " " ^ h ^ "\">"
      val body = String.concat (List.map (fn l => "\n  " ^ l) lines)
    in
      header ^ body ^ "\n</svg>"
    end
end
