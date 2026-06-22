(* text_layout.sig

   Pure-Standard-ML text layout: Unicode-aware measurement and greedy line
   breaking / word wrapping on top of `sml-font` (bitmap glyph advances) and
   `sml-unicode` (extended grapheme-cluster segmentation + East-Asian display
   width).

   The unit of layout is the GRAPHEME CLUSTER (UAX #29), so a base character
   plus its combining marks stay together and occupy a single cell.  A cluster's
   display width in CELLS is the sum of `Unicode.width` over its codepoints
   (combining marks add 0, East-Asian wide characters count as 2); its width in
   PIXELS is that cell count times the font's monospace cell advance.  Because
   the model is integer cell/pixel arithmetic over a bitmap font, every result
   is exact and byte-identical under MLton and Poly/ML.

   Wrapping is greedy: text is first split on hard newlines ('\n'); each
   resulting paragraph is then packed word-by-word into `maxWidth` pixels,
   breaking at inter-word spaces.  A run of spaces between two words is a single
   break opportunity (its spaces are emitted as glyphs only when the words stay
   on the same line, and dropped at a wrap boundary).  A single word wider than
   `maxWidth` is force-broken between grapheme clusters.

   Scope (deterministic by construction): bitmap monospace measurement only - no
   TTF hinting, no complex shaping, no bidi reordering, no hyphenation. *)

signature TEXT_LAYOUT =
sig
  type font = Font.font

  (* A laid-out grapheme cluster: its UTF-8 text `ch`, top-left pixel position
     (`x`, `y`) within the layout, and pixel width `w`. *)
  type glyphpos = { ch : string, x : int, y : int, w : int }

  (* One laid-out line: its baseline-independent top `y`, total pixel `width`
     (x-extent of its glyphs, including interior spaces), and its glyphs left to
     right. *)
  type line = { y : int, width : int, glyphs : glyphpos list }

  (* A finished layout: the bounding `width` (widest line) and `height`
     (number of lines times the line height), and the lines top to bottom. *)
  type layout = { width : int, height : int, lines : line list }

  (* Pixel cell advance of the font (the monospace cell width all measurement is
     based on): `Font.advance` of a space.  Exposed for callers that need to map
     pixels back to columns. *)
  val cellWidth : font -> int

  (* Unicode-aware pixel width of a single grapheme cluster (UTF-8): its summed
     `Unicode.width` cells times `cellWidth`.  Unknown/empty input is 0. *)
  val graphemeWidth : font -> string -> int

  (* Measure a (possibly multi-line) string at scale 1: `width` is the widest
     line's pixel width and `height` is `Font.height` times the number of lines
     (lines are split on '\n').  The empty string measures (0, 0). *)
  val measure : font -> string -> int * int

  (* Greedy word-wrap into `maxWidth` pixels.  `lineHeight` defaults to
     `Font.height font` when NONE.  Newlines force hard breaks; over-long words
     are force-broken between grapheme clusters.  The empty string yields a
     layout with no lines (width 0, height 0). *)
  val wrap : { font : font, maxWidth : int, lineHeight : int option }
             -> string -> layout
end
