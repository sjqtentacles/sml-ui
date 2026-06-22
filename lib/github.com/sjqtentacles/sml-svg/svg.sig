(* svg.sig

   A small, pure SVG document builder and serializer. You describe a drawing as
   a tree of `el` values, then `toString` renders a complete, pretty-printed
   `<svg>` document. Rendering is deterministic and byte-identical under both
   MLton and Poly/ML:

     - element nesting is laid out with the vendored `sml-pretty` printer
       (one element per line, group children indented two columns);
     - every coordinate is a `real` formatted by `fmtReal`, which always emits a
       decimal point and uses a leading "-" (never SML's "~") so the same bytes
       come out of either compiler;
     - attribute values and text content are XML-escaped.

   Shapes (`Rect`/`Circle`/`Line`/`Text`) carry a free-form `attrs` list of
   `(name, value)` pairs rendered verbatim (escaped) after the geometry, so
   styling such as `("fill", "red")` or `("stroke-width", "2")` needs no
   dedicated constructor. `Path` is raw path data; `Group` wraps a `<g>`. *)

signature SVG =
sig
  (* An XML attribute as a (name, value) pair. The value is escaped on render. *)
  type attr = string * string

  datatype el =
      Rect   of { x : real, y : real, width : real, height : real
                , attrs : attr list }
    | Circle of { cx : real, cy : real, r : real, attrs : attr list }
    | Line   of { x1 : real, y1 : real, x2 : real, y2 : real
                , attrs : attr list }
    | Path   of string
    | Group  of el list
    | Text   of { x : real, y : real, text : string, attrs : attr list }

  (* Render a complete `<svg>` document (with xmlns/width/height/viewBox) for a
     canvas of the given pixel size and element list. The result is
     pretty-printed and has no trailing newline. *)
  val toString : { width : int, height : int, els : el list } -> string

  (* Deterministic real formatting: fixed-precision, trailing zeros trimmed but
     always keeping a decimal point, with a leading "-" for negatives. E.g.
     `10.0 -> "10.0"`, `3.14 -> "3.14"`, `~2.5 -> "-2.5"`, `0.0 -> "0.0"`. *)
  val fmtReal : real -> string
end
