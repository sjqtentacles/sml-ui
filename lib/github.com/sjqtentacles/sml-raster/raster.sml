(* raster.sml

   Implementation of RASTER: a small, pure 2D rasterizer over the sml-image
   RGBA8 representation.

   Strategy: each public op materializes the input image's bytes into a fresh
   mutable Word8Array, mutates it while drawing (clipping to bounds), then
   freezes it back into a new immutable `image`.  This keeps the public API
   purely functional while drawing a shape is O(pixels) rather than allocating
   a whole new vector per pixel. *)

structure Raster :> RASTER =
struct
  type image = Image.image
  type rgba8 = Image.rgba8

  structure W8  = Word8
  structure W8V = Word8Vector
  structure W8A = Word8Array

  (* --- a small mutable canvas: array of 4*w*h bytes, RGBA row-major --- *)
  type canvas = { width : int, height : int, arr : W8A.array }

  fun toCanvas ({ width, height, data } : image) : canvas =
    { width = width, height = height,
      arr = W8A.tabulate (W8V.length data, fn i => W8V.sub (data, i)) }

  fun freeze ({ width, height, arr } : canvas) : image =
    { width = width, height = height, data = W8A.vector arr }

  (* Opaque write of one pixel, clipped to canvas bounds. *)
  fun put ({ width, height, arr } : canvas) (x, y) ({ r, g, b, a } : rgba8) =
    if x < 0 orelse y < 0 orelse x >= width orelse y >= height then ()
    else
      let val off = 4 * (y * width + x)
      in W8A.update (arr, off,     r);
         W8A.update (arr, off + 1, g);
         W8A.update (arr, off + 2, b);
         W8A.update (arr, off + 3, a)
      end

  (* Alpha-over blend of one pixel against the existing pixel, clipped.
     out = (src*a + dst*(255-a) + 127) div 255, rounded to nearest. *)
  fun blend ({ width, height, arr } : canvas) (x, y) ({ r, g, b, a } : rgba8) =
    if x < 0 orelse y < 0 orelse x >= width orelse y >= height then ()
    else
      let
        val off = 4 * (y * width + x)
        val sa = W8.toInt a
        val ia = 255 - sa
        fun comp (sv, dv) =
          let val s = W8.toInt sv and d = W8.toInt dv
          in W8.fromInt ((s * sa + d * ia + 127) div 255) end
        val dr = W8A.sub (arr, off)
        val dg = W8A.sub (arr, off + 1)
        val db = W8A.sub (arr, off + 2)
        val da = W8A.sub (arr, off + 3)
      in
        W8A.update (arr, off,     comp (r, dr));
        W8A.update (arr, off + 1, comp (g, dg));
        W8A.update (arr, off + 2, comp (b, db));
        W8A.update (arr, off + 3, comp (a, da))
      end

  (* --- public single-pixel ops --- *)

  fun blank (w, h) color = Image.fill (w, h) color

  fun setPixel img p color =
    let val c = toCanvas img in put c p color; freeze c end

  fun blendPixel img p color =
    let val c = toCanvas img in blend c p color; freeze c end

  (* --- lines (Bresenham) --- *)

  fun drawLine c (x0, y0) (x1, y1) color =
    let
      val dx = abs (x1 - x0)
      val dy = abs (y1 - y0)
      val sx = if x0 < x1 then 1 else ~1
      val sy = if y0 < y1 then 1 else ~1
      fun loop (x, y, err) =
        (put c (x, y) color;
         if x = x1 andalso y = y1 then ()
         else
           let
             val e2 = 2 * err
             val (x', err1) = if e2 > ~dy then (x + sx, err - dy) else (x, err)
             val (y', err2) = if e2 <  dx then (y + sy, err1 + dx) else (y, err1)
           in loop (x', y', err2) end)
    in loop (x0, y0, dx - dy) end

  fun line img { x0, y0, x1, y1 } color =
    let val c = toCanvas img
    in drawLine c (x0, y0) (x1, y1) color; freeze c end

  (* --- rectangles --- *)

  fun fillRect img { x, y, w, h } color =
    let
      val c = toCanvas img
      fun rows j =
        if j >= h then ()
        else
          let
            fun cols i = if i >= w then () else (put c (x + i, y + j) color; cols (i + 1))
          in cols 0; rows (j + 1) end
    in if w <= 0 orelse h <= 0 then freeze c
       else (rows 0; freeze c)
    end

  fun rect img { x, y, w, h } color =
    let val c = toCanvas img
    in if w <= 0 orelse h <= 0 then freeze c
       else
         (drawLine c (x, y) (x + w - 1, y) color;             (* top *)
          drawLine c (x, y + h - 1) (x + w - 1, y + h - 1) color; (* bottom *)
          drawLine c (x, y) (x, y + h - 1) color;             (* left *)
          drawLine c (x + w - 1, y) (x + w - 1, y + h - 1) color; (* right *)
          freeze c)
    end

  (* --- circles (midpoint) --- *)

  fun circle img { cx, cy, r } color =
    let
      val c = toCanvas img
      fun plot (x, y) =
        (put c (cx + x, cy + y) color; put c (cx - x, cy + y) color;
         put c (cx + x, cy - y) color; put c (cx - x, cy - y) color;
         put c (cx + y, cy + x) color; put c (cx - y, cy + x) color;
         put c (cx + y, cy - x) color; put c (cx - y, cy - x) color)
      fun loop (x, y, d) =
        if x > y then ()
        else
          (plot (x, y);
           if d < 0 then loop (x + 1, y, d + 2 * x + 3)
           else loop (x + 1, y - 1, d + 2 * (x - y) + 5))
    in if r < 0 then freeze c
       else (loop (0, r, 1 - r); freeze c)
    end

  fun fillCircle img { cx, cy, r } color =
    let
      val c = toCanvas img
      fun hspan (y, half) =
        let
          fun cols i = if i > half then () else (put c (cx + i, y) color; put c (cx - i, y) color; cols (i + 1))
        in cols 0 end
      fun loop (x, y, d) =
        if x > y then ()
        else
          (* horizontal spans for the two symmetric octant pairs *)
          (hspan (cy + y, x); hspan (cy - y, x);
           hspan (cy + x, y); hspan (cy - x, y);
           if d < 0 then loop (x + 1, y, d + 2 * x + 3)
           else loop (x + 1, y - 1, d + 2 * (x - y) + 5))
    in if r < 0 then freeze c
       else (loop (0, r, 1 - r); freeze c)
    end

  (* --- ellipses (midpoint) --- *)

  (* Walk the first-quadrant boundary of an ellipse with radii (rx, ry),
     invoking [f (x, y)] for each boundary point with x, y >= 0.  Standard
     two-region midpoint ellipse: coordinates and the dx/dy slope trackers are
     integers; only the decision variables d1/d2 carry the +0.25 fractional
     term, and they evolve purely by integer increments, so the visited pixel
     set is identical across compilers. *)
  fun ellipsePoints (rx, ry) f =
    if rx < 0 orelse ry < 0 then ()
    else
      let
        val rx2 = rx * rx
        val ry2 = ry * ry
        fun region1 (x, y, dx, dy, d1) =
          if dx >= dy then (x, y, dx, dy)
          else
            (f (x, y);
             if d1 < 0.0 then
               let val x' = x + 1
                   val dx' = dx + 2 * ry2
               in region1 (x', y, dx', dy,
                           d1 + Real.fromInt dx' + Real.fromInt ry2) end
             else
               let val x' = x + 1
                   val y' = y - 1
                   val dx' = dx + 2 * ry2
                   val dy' = dy - 2 * rx2
               in region1 (x', y', dx', dy',
                           d1 + Real.fromInt dx' - Real.fromInt dy' + Real.fromInt ry2) end)
        fun region2 (x, y, dx, dy, d2) =
          if y < 0 then ()
          else
            (f (x, y);
             if d2 > 0.0 then
               let val y' = y - 1
                   val dy' = dy - 2 * rx2
               in region2 (x, y', dx, dy',
                           d2 + Real.fromInt rx2 - Real.fromInt dy') end
             else
               let val y' = y - 1
                   val x' = x + 1
                   val dx' = dx + 2 * ry2
                   val dy' = dy - 2 * rx2
               in region2 (x', y', dx', dy',
                           d2 + Real.fromInt dx' - Real.fromInt dy' + Real.fromInt rx2) end)
        val dy0 = 2 * rx2 * ry
        val d1 = Real.fromInt ry2 - Real.fromInt (rx2 * ry) + 0.25 * Real.fromInt rx2
        val (x1, y1, dx1, dy1) = region1 (0, ry, 0, dy0, d1)
        val d2 = Real.fromInt ry2 * (Real.fromInt x1 + 0.5) * (Real.fromInt x1 + 0.5)
                 + Real.fromInt rx2 * Real.fromInt ((y1 - 1) * (y1 - 1))
                 - Real.fromInt (rx2 * ry2)
      in
        region2 (x1, y1, dx1, dy1, d2)
      end

  fun ellipse img { cx, cy, rx, ry } color =
    let
      val c = toCanvas img
      fun plot (x, y) =
        (put c (cx + x, cy + y) color; put c (cx - x, cy + y) color;
         put c (cx + x, cy - y) color; put c (cx - x, cy - y) color)
    in ellipsePoints (rx, ry) plot; freeze c end

  fun fillEllipse img { cx, cy, rx, ry } color =
    let
      val c = toCanvas img
    in
      if rx < 0 orelse ry < 0 then freeze c
      else
        let
          (* Rightmost boundary x for each scanline offset y in 0..ry. *)
          val maxX = Array.array (ry + 1, ~1)
          fun record (x, y) =
            if y >= 0 andalso y <= ry andalso x > Array.sub (maxX, y)
            then Array.update (maxX, y, x) else ()
          val () = ellipsePoints (rx, ry) record
          fun spanRow (y, half) =
            let fun cols i = if i > half then ()
                             else (put c (cx + i, y) color; put c (cx - i, y) color; cols (i + 1))
            in cols 0 end
          fun rows y =
            if y > ry then ()
            else
              let val half = Array.sub (maxX, y)
              in
                (if half >= 0 then
                   (spanRow (cy + y, half);
                    if y <> 0 then spanRow (cy - y, half) else ())
                 else ());
                rows (y + 1)
              end
        in rows 0; freeze c end
    end

  (* --- circular arc --- *)

  (* Reuses the midpoint-circle point set so that a full-turn arc draws the
     exact same pixels as [circle]; each candidate point is kept only when its
     angle falls within the requested counter-clockwise sweep. *)
  fun arc img { cx, cy, r, startAngle, endAngle } color =
    let
      val c = toCanvas img
      val twoPi = 2.0 * Math.pi
      val span = endAngle - startAngle
      fun norm x = x - twoPi * Real.realFloor (x / twoPi)
      fun inArc (px, py) =
        let val a = Math.atan2 (Real.fromInt (py - cy), Real.fromInt (px - cx))
        in norm (a - startAngle) <= span end
      fun plotIf (px, py) = if inArc (px, py) then put c (px, py) color else ()
      fun plot (x, y) =
        (plotIf (cx + x, cy + y); plotIf (cx - x, cy + y);
         plotIf (cx + x, cy - y); plotIf (cx - x, cy - y);
         plotIf (cx + y, cy + x); plotIf (cx - y, cy + x);
         plotIf (cx + y, cy - x); plotIf (cx - y, cy - x))
      fun loop (x, y, d) =
        if x > y then ()
        else
          (plot (x, y);
           if d < 0 then loop (x + 1, y, d + 2 * x + 3)
           else loop (x + 1, y - 1, d + 2 * (x - y) + 5))
    in if r < 0 then freeze c
       else (loop (0, r, 1 - r); freeze c)
    end

  (* --- polylines --- *)

  fun polyline img pts color =
    let
      val c = toCanvas img
      fun loop (p :: q :: rest) = (drawLine c p q color; loop (q :: rest))
        | loop [p] = put c p color  (* single point *)
        | loop [] = ()
    in loop pts; freeze c end

  (* --- scanline fill for polygons (even-odd) --- *)

  fun insertAsc (x, []) = [x]
    | insertAsc (x, y :: ys) = if x <= y then x :: y :: ys else y :: insertAsc (x, ys)
  fun sortAsc xs = foldl insertAsc [] xs

  fun fillPolygonInto c pts color =
    case pts of
      [] => ()
    | [_] => ()
    | _ =>
      let
        val ys = map (fn (_, y) => y) pts
        val ymin = foldl Int.min (hd ys) (tl ys)
        val ymax = foldl Int.max (hd ys) (tl ys)
        (* edges as adjacent vertex pairs, wrapping last->first *)
        val verts = Vector.fromList pts
        val n = Vector.length verts
        fun edge i = (Vector.sub (verts, i), Vector.sub (verts, (i + 1) mod n))
        fun scan y =
          if y > ymax then ()
          else
            let
              (* collect x crossings of scanline y + 0.5 (use half-open rule) *)
              fun gather (i, acc) =
                if i >= n then acc
                else
                  let
                    val ((x0, y0), (x1, y1)) = edge i
                  in
                    if (y0 <= y andalso y1 > y) orelse (y1 <= y andalso y0 > y)
                    then
                      let
                        (* x intersection at scanline y (integer, rounded) *)
                        val t = (y - y0)
                        val xi = x0 + (x1 - x0) * t div (y1 - y0)
                      in gather (i + 1, xi :: acc) end
                    else gather (i + 1, acc)
                  end
              val xs = sortAsc (gather (0, []))
              fun spans (a :: b :: rest) =
                    let fun cols i = if i > b then () else (put c (i, y) color; cols (i + 1))
                    in cols a; spans rest end
                | spans _ = ()
            in spans xs; scan (y + 1) end
      in scan ymin end

  fun fillPolygon img pts color =
    let val c = toCanvas img in fillPolygonInto c pts color; freeze c end

  (* --- triangles --- *)

  fun triangle img (a, b, cc) color =
    let
      val c = toCanvas img
    in
      drawLine c a b color;
      drawLine c b cc color;
      drawLine c cc a color;
      freeze c
    end

  fun fillTriangle img (a, b, cc) color =
    let val c = toCanvas img
    in fillPolygonInto c [a, b, cc] color; freeze c end

  (* --- blit --- *)

  fun blit img { dst = (dx, dy), src } =
    let
      val c = toCanvas img
      val { width = sw, height = sh, ... } = src
      fun rows j =
        if j >= sh then ()
        else
          let
            fun cols i =
              if i >= sw then ()
              else
                (put c (dx + i, dy + j) (Image.getPixel src (i, j));
                 cols (i + 1))
          in cols 0; rows (j + 1) end
    in rows 0; freeze c end
end
