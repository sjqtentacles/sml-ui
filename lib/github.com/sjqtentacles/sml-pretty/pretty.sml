(* pretty.sml

   Implementation of the Wadler/Leijen prettier-printer (signature PRETTY).

   The renderer keeps a work list of `(indent, mode, doc)` triples and a current
   column `k`. A `group` is laid out flat when its flat rendering fits in the
   remaining width up to the next forced break (`fits`), and broken otherwise.
   This is the classic linear-time best/fits algorithm and is deterministic. *)

structure Pretty :> PRETTY =
struct

  datatype doc =
      Nil
    | Text of string
    | Line                 (* space when flat, newline when broken *)
    | SoftLine             (* nothing when flat, newline when broken *)
    | HardLine             (* always a newline; forces enclosing group to break *)
    | Cat of doc * doc
    | Nest of int * doc
    | Group of doc

  val empty = Nil
  fun text s = Text s
  val line = Line
  val softline = SoftLine
  val hardline = HardLine
  fun nest n d = Nest (n, d)
  fun group d = Group d
  fun concat (a, b) = Cat (a, b)

  infixr 6 <>
  infixr 6 <+>
  infixr 6 </>
  fun a <> b = Cat (a, b)
  fun a <+> b = Cat (a, Cat (Text " ", b))
  fun a </> b = Cat (a, Cat (SoftLine, b))

  fun hsep [] = Nil
    | hsep [d] = d
    | hsep (d :: ds) = Cat (d, Cat (Text " ", hsep ds))

  fun vsep [] = Nil
    | vsep [d] = d
    | vsep (d :: ds) = Cat (d, Cat (Line, vsep ds))

  datatype mode = Flat | Break

  fun spaces n = CharVector.tabulate (Int.max (n, 0), fn _ => #" ")

  (* Does the content at the front of the work list fit in `w` columns, i.e.
     up to (and not past) the next line break it produces? In `Flat` mode a
     `HardLine` cannot fit, which forces its enclosing group to break. *)
  fun fits w worklist =
    w >= 0 andalso
    (case worklist of
         [] => true
       | (i, m, d) :: rest =>
           (case d of
                Nil => fits w rest
              | Text s => fits (w - String.size s) rest
              | Line =>
                  (case m of Flat => fits (w - 1) rest | Break => true)
              | SoftLine =>
                  (case m of Flat => fits w rest | Break => true)
              | HardLine =>
                  (case m of Flat => false | Break => true)
              | Cat (x, y) => fits w ((i, m, x) :: (i, m, y) :: rest)
              | Nest (j, x) => fits w ((i + j, m, x) :: rest)
              | Group x => fits w ((i, Flat, x) :: rest)))

  fun pretty width doc =
    let
      fun go (_, []) = []
        | go (k, (i, m, d) :: rest) =
            (case d of
                 Nil => go (k, rest)
               | Text s => s :: go (k + String.size s, rest)
               | Cat (x, y) => go (k, (i, m, x) :: (i, m, y) :: rest)
               | Nest (j, x) => go (k, (i + j, m, x) :: rest)
               | Line =>
                   (case m of
                        Flat => " " :: go (k + 1, rest)
                      | Break => ("\n" ^ spaces i) :: go (i, rest))
               | SoftLine =>
                   (case m of
                        Flat => go (k, rest)
                      | Break => ("\n" ^ spaces i) :: go (i, rest))
               | HardLine => ("\n" ^ spaces i) :: go (i, rest)
               | Group x =>
                   if fits (width - k) ((i, Flat, x) :: rest)
                   then go (k, (i, Flat, x) :: rest)
                   else go (k, (i, Break, x) :: rest))
    in
      String.concat (go (0, [(0, Break, doc)]))
    end

end
