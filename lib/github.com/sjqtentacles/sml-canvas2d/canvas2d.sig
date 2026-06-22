(* canvas2d.sig

   A backend-agnostic, immediate 2D drawing-command model. You build an
   immutable display list (`scene` = `cmd list`) of paths, rectangles, text, and
   affine transforms, then render the SAME scene through two deterministic
   backends:

     - `toImage` rasterizes onto an `sml-image` RGBA8 `Image.image` (via the
       `sml-raster` primitives) - the golden-image carrier used to test the
       whole GUI stack headlessly;
     - `toSvg` serializes to an SVG document string, every coordinate formatted
       with `fmtReal` (decimal point always, leading "-" never "~"), reusing the
       canonical `sml-svg` convention.

   Both backends are pure and byte-identical across MLton and Poly/ML.

   Colors are `Color.rgba` (real channels in [0,1]); the raster backend converts
   them to `Image.rgba8` via `Color.pack` (clamped, round-to-nearest).

   NOTE (scope): unlike the original sketch, `toImage` takes NO external font -
   text is drawn with a small BUILT-IN 5x7 bitmap font so canvas2d stays
   dependency-faithful (it vendors only sml-image/sml-raster/sml-color/sml-svg,
   no sml-font and no runtime BDF file). The built-in font covers A-Z (lower
   case folded to upper), 0-9, space, '-', '.', and ':'; unknown glyphs advance
   blank. Text is positioned through the current transform's translation; glyph
   cells are axis-aligned. Alpha is honored exactly in `toSvg`; the raster
   backend writes shape colors opaquely (a documented v1 simplification), and
   `Clip` is applied by `FillRect`/`Text` (path clipping is image-bounds only). *)

signature CANVAS2D =
sig
  type color = Color.rgba
  type rect  = { x : real, y : real, w : real, h : real }

  (* ---- affine transforms (2x3: x' = a*x + c*y + e, y' = b*x + d*y + f) ---- *)
  type xform
  val identity  : xform
  val translate : real * real -> xform
  val scale     : real * real -> xform
  val rotate    : real -> xform                 (* radians, CW in screen space *)
  val compose   : xform * xform -> xform        (* compose (m1, m2) = m1 then map by m1 of m2's image; matrix m1*m2 *)
  val apply     : xform -> real * real -> real * real

  (* ---- path segments ---- *)
  datatype pathseg =
      MoveTo  of real * real
    | LineTo  of real * real
    | QuadTo  of (real * real) * (real * real)                       (* control, end *)
    | CubicTo of (real * real) * (real * real) * (real * real)       (* c1, c2, end *)
    | Close

  (* ---- drawing commands ---- *)
  datatype cmd =
      Save | Restore
    | SetTransform of xform
    | Transform of xform                          (* multiply onto current xform *)
    | Fill of { path : pathseg list, color : color }
    | Stroke of { path : pathseg list, color : color, width : real }
    | FillRect of rect * color
    | Text of { x : real, y : real, text : string, color : color, scale : int }
    | Clip of rect

  type scene = cmd list

  (* Rasterize a scene onto a `background`-filled image of the given size. *)
  val toImage : { width : int, height : int, background : color }
                -> scene -> Image.image

  (* Serialize a scene to an SVG document string (no trailing newline). *)
  val toSvg : { width : int, height : int } -> scene -> string

  val fmtReal : real -> string
end
