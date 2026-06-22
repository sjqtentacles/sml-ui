(* pretty.sig

   A Wadler/Leijen "prettier printer": pure combinators that build an abstract
   `doc`, plus a `pretty` renderer that lays the document out within a maximum
   line width. `group` chooses a flat (single-line) layout when it fits and a
   broken (multi-line) layout otherwise. Rendering is deterministic. *)

signature PRETTY =
sig
  type doc

  (* The empty document; identity for `concat`. *)
  val empty : doc

  (* A document containing literal text (assumed to contain no newlines). *)
  val text : string -> doc

  (* A line break that becomes a single space when flattened (inside a
     `group` that fits). *)
  val line : doc

  (* A line break that becomes nothing when flattened. *)
  val softline : doc

  (* A line break that is always rendered as a newline, even inside a `group`
     that would otherwise fit on one line (it forces the group to break). *)
  val hardline : doc

  (* `nest n d` increases the indentation of line breaks inside `d` by `n`
     columns. Indentation takes effect on the next broken line. *)
  val nest : int -> doc -> doc

  (* `group d` renders `d` flat (all `line`/`softline` collapsed) if the flat
     layout fits in the remaining width; otherwise renders `d` as-is. *)
  val group : doc -> doc

  (* Concatenate two documents. `<>` is the infix form. *)
  val concat : doc * doc -> doc
  val <>  : doc * doc -> doc

  (* Concatenate two documents separated by a space. *)
  val <+> : doc * doc -> doc

  (* Concatenate two documents separated by a `softline`. *)
  val </> : doc * doc -> doc

  (* `vsep ds` joins documents with `line` (vertical when broken). *)
  val vsep : doc list -> doc

  (* `hsep ds` joins documents with a single space. *)
  val hsep : doc list -> doc

  (* `pretty w d` renders `d` as a string, trying to keep lines within `w`
     columns. *)
  val pretty : int -> doc -> string
end
