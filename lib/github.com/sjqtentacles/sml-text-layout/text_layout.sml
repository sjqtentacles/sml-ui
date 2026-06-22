(* text_layout.sml - implementation of TEXT_LAYOUT.

   All arithmetic is in integer pixels over a bitmap font, so output is exact
   and byte-identical across MLton and Poly/ML.  A grapheme cluster's pixel
   width is its summed Unicode display-width (cells) times the font's monospace
   cell advance. *)

structure TextLayout :> TEXT_LAYOUT =
struct
  type font = Font.font
  type glyphpos = { ch : string, x : int, y : int, w : int }
  type line = { y : int, width : int, glyphs : glyphpos list }
  type layout = { width : int, height : int, lines : line list }

  fun cellWidth font = Font.advance font #" "

  (* Display width of a grapheme cluster in cells (sum of Unicode.width over its
     codepoints); malformed bytes contribute nothing. *)
  fun cells g =
    let val cps = Unicode.decodeUtf8 g handle _ => []
    in List.foldl (fn (cp, a) => a + Unicode.width cp) 0 cps end

  fun graphemeWidth font g = cells g * cellWidth font

  (* Width of one (newline-free) text line in pixels. *)
  fun textWidth font s =
    List.foldl (fn (g, a) => a + graphemeWidth font g) 0 (Unicode.graphemes s)

  fun measure font s =
    if s = "" then (0, 0)
    else
      let
        val ls = String.fields (fn c => c = #"\n") s
        val w = List.foldl (fn (ln, a) => Int.max (a, textWidth font ln)) 0 ls
        val h = Font.height font * length ls
      in (w, h) end

  (* ---- wrapping ---- *)

  (* A token is a maximal run of spaces (Sp) or of non-space clusters (Wd); each
     cluster is paired with its pixel width. *)
  datatype tok = Sp of (string * int) list | Wd of (string * int) list

  fun isSpace g = (g = " ")

  fun tokenize font s =
    let
      val gs = map (fn g => (g, graphemeWidth font g)) (Unicode.graphemes s)
      (* group consecutive clusters of the same space-ness *)
      fun group [] = []
        | group ((g, w) :: rest) =
            let
              val sp = isSpace g
              fun take acc [] = (rev acc, [])
                | take acc ((g', w') :: r) =
                    if isSpace g' = sp then take ((g', w') :: acc) r
                    else (rev acc, (g', w') :: r)
              val (run, rest') = take [(g, w)] rest
            in
              (if sp then Sp run else Wd run) :: group rest'
            end
    in group gs end

  fun runWidth clusters = List.foldl (fn ((_, w), a) => a + w) 0 clusters

  (* Lay out one paragraph (no embedded newlines) into a list of lines whose y
     is provisionally 0 (assigned globally afterwards). *)
  fun layoutPara (maxWidth : int) (toks : tok list) : line list =
    let
      (* Append `clusters` to the current line starting at curX, with no
         breaking (caller guarantees they fit). *)
      fun emit (cur, curX) clusters =
        List.foldl
          (fn ((ch, w), (gs, x)) => (gs @ [{ ch = ch, x = x, y = 0, w = w }], x + w))
          (cur, curX) clusters

      fun finish (lines, cur, curX) =
        { y = 0, width = curX, glyphs = cur } :: lines

      (* Force-break a word across lines, cluster by cluster. *)
      fun forceBreak (lines, cur, curX) [] = (lines, cur, curX)
        | forceBreak (lines, cur, curX) ((ch, w) :: rest) =
            if cur <> [] andalso curX + w > maxWidth
            then forceBreak (finish (lines, cur, curX), [], 0) ((ch, w) :: rest)
            else forceBreak (lines, cur @ [{ ch = ch, x = curX, y = 0, w = w }], curX + w) rest

      (* Walk tokens.  `sep` is the pending separator spaces (emitted only if the
         next word stays on this line; dropped at a wrap boundary). *)
      fun go (lines, cur, curX, _) [] = rev (finish (lines, cur, curX))
        | go (lines, cur, curX, _) (Sp run :: rest) =
            (* Leading spaces on an empty line are dropped. *)
            if cur = [] then go (lines, cur, curX, []) rest
            else go (lines, cur, curX, run) rest
        | go (lines, cur, curX, sep) (Wd word :: rest) =
            let val ww = runWidth word in
              if cur = [] then
                (if ww <= maxWidth then
                   let val (cur', curX') = emit ([], 0) word
                   in go (lines, cur', curX', []) rest end
                 else
                   let val (lines', cur', curX') = forceBreak (lines, [], 0) word
                   in go (lines', cur', curX', []) rest end)
              else
                let val sepW = runWidth sep in
                  if curX + sepW + ww <= maxWidth then
                    let
                      val (cur1, x1) = emit (cur, curX) sep
                      val (cur2, x2) = emit (cur1, x1) word
                    in go (lines, cur2, x2, []) rest end
                  else
                    (* wrap: drop sep, start a new line with this word *)
                    let val lines1 = finish (lines, cur, curX) in
                      if ww <= maxWidth then
                        let val (cur', curX') = emit ([], 0) word
                        in go (lines1, cur', curX', []) rest end
                      else
                        let val (lines', cur', curX') = forceBreak (lines1, [], 0) word
                        in go (lines', cur', curX', []) rest end
                    end
                end
            end
    in
      go ([], [], 0, []) toks
    end

  fun wrap { font, maxWidth, lineHeight } s =
    if s = "" then { width = 0, height = 0, lines = [] }
    else
      let
        val lh = case lineHeight of SOME n => n | NONE => Font.height font
        val paras = String.fields (fn c => c = #"\n") s
        val rawLines = List.concat (map (fn p => layoutPara maxWidth (tokenize font p)) paras)
        (* assign y per line and to each glyph *)
        fun place i ({ width, glyphs, ... } : line) =
          let
            val y = i * lh
            val gs = map (fn { ch, x, w, ... } : glyphpos => { ch = ch, x = x, y = y, w = w }) glyphs
          in { y = y, width = width, glyphs = gs } end
        fun indexed (_, []) = []
          | indexed (i, ln :: rest) = place i ln :: indexed (i + 1, rest)
        val lines = indexed (0, rawLines)
        val w = List.foldl (fn ({ width, ... } : line, a) => Int.max (a, width)) 0 lines
        val h = length lines * lh
      in { width = w, height = h, lines = lines } end
end
