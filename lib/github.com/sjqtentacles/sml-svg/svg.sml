(* svg.sml

   Implementation of `signature SVG`. Layout is delegated to the vendored
   `sml-pretty` printer: each element renders as one atomic `Pretty.text` line,
   and `hardline`/`nest` provide the one-element-per-line, indented-group
   structure. Because each tag is a single `text`, attributes never wrap; only
   the structural newlines between elements break. *)

structure Svg :> SVG =
struct

  structure P = Pretty

  infixr 6 ^^
  fun a ^^ b = P.concat (a, b)

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

  (* ---- numeric formatting (deterministic across compilers) ---- *)

  fun fmtReal r =
    let
      (* Fixed precision avoids scientific notation and the GEN-format
         differences between MLton and Poly/ML. Work on the magnitude so the
         sign is handled explicitly (and never as SML's "~"). *)
      val s = Real.fmt (StringCvt.FIX (SOME 6)) (Real.abs r)
      val (intPart, fracPart) =
        case String.fields (fn c => c = #".") s of
            a :: b :: _ => (a, b)
          | [a] => (a, "")
          | [] => ("0", "")
      (* Drop trailing zeros from the fractional digits, but keep at least one
         so the result always contains a decimal point. *)
      val fracTrimmed =
        let
          fun dropZeros [] = [#"0"]
            | dropZeros (c :: cs) = if c = #"0" then dropZeros cs else c :: cs
        in
          String.implode (List.rev (dropZeros (List.rev (String.explode fracPart))))
        end
      val mag = intPart ^ "." ^ fracTrimmed
      val isZero =
        List.all (fn c => c = #"0") (String.explode intPart) andalso fracTrimmed = "0"
      val sign = if r < 0.0 andalso not isZero then "-" else ""
    in
      sign ^ mag
    end

  (* ---- XML escaping ---- *)

  fun escapeText s =
    String.translate
      (fn #"&" => "&amp;"
        | #"<" => "&lt;"
        | #">" => "&gt;"
        | c => String.str c)
      s

  fun escapeAttr s =
    String.translate
      (fn #"&" => "&amp;"
        | #"<" => "&lt;"
        | #">" => "&gt;"
        | #"\"" => "&quot;"
        | c => String.str c)
      s

  (* ---- attribute rendering ---- *)

  fun numAttr (name, v) = " " ^ name ^ "=\"" ^ fmtReal v ^ "\""
  fun strAttr (name, v) = " " ^ name ^ "=\"" ^ escapeAttr v ^ "\""
  fun attrList attrs = String.concat (List.map strAttr attrs)

  (* ---- per-element document ---- *)

  fun elDoc el =
    case el of
        Rect { x, y, width, height, attrs } =>
          P.text ("<rect" ^ numAttr ("x", x) ^ numAttr ("y", y)
                  ^ numAttr ("width", width) ^ numAttr ("height", height)
                  ^ attrList attrs ^ "/>")
      | Circle { cx, cy, r, attrs } =>
          P.text ("<circle" ^ numAttr ("cx", cx) ^ numAttr ("cy", cy)
                  ^ numAttr ("r", r) ^ attrList attrs ^ "/>")
      | Line { x1, y1, x2, y2, attrs } =>
          P.text ("<line" ^ numAttr ("x1", x1) ^ numAttr ("y1", y1)
                  ^ numAttr ("x2", x2) ^ numAttr ("y2", y2)
                  ^ attrList attrs ^ "/>")
      | Path d =>
          P.text ("<path d=\"" ^ escapeAttr d ^ "\"/>")
      | Text { x, y, text, attrs } =>
          P.text ("<text" ^ numAttr ("x", x) ^ numAttr ("y", y)
                  ^ attrList attrs ^ ">" ^ escapeText text ^ "</text>")
      | Group [] => P.text "<g></g>"
      | Group els =>
          P.text "<g>"
          ^^ P.nest 2 (P.hardline ^^ stack (List.map elDoc els))
          ^^ P.hardline ^^ P.text "</g>"

  (* Join element docs one-per-line with hardlines (always break). *)
  and stack [] = P.empty
    | stack [d] = d
    | stack (d :: ds) = d ^^ P.hardline ^^ stack ds

  (* ---- document ---- *)

  fun toString { width, height, els } =
    let
      val w = Int.toString width
      val h = Int.toString height
      val header =
        "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"" ^ w
        ^ "\" height=\"" ^ h ^ "\" viewBox=\"0 0 " ^ w ^ " " ^ h ^ "\">"
      val body =
        case els of
            [] => P.empty
          | _ => P.nest 2 (P.hardline ^^ stack (List.map elDoc els))
      val doc = P.text header ^^ body ^^ P.hardline ^^ P.text "</svg>"
    in
      P.pretty 80 doc
    end

end
