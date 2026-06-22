(* layout.sig

   A pure constraint / flexbox-style box-layout solver: a function from a box
   tree to the resolved, absolute rectangle of every box.

   This is the geometric backbone of the pure-SML GUI stack - both the native
   widget toolkit (`sml-ui`) and the web front-end share it - so it is kept
   dependency-free.

   Model. A `box` lays its `children` out along its main axis (`dir`): `Row`
   places them left-to-right, `Column` top-to-bottom. Each box carries:

     - `grow`   : its flex-grow weight, used by its PARENT to share out leftover
                  main-axis space among siblings (CSS `flex-grow`);
     - `basis`  : its preferred main-axis size before growing (CSS `flex-basis`);
                  `NONE` falls back to the box's `min` size on the main axis;
     - `padding`: inner inset - children are laid out inside it;
     - `margin` : outer space reserved around the box within its allocation;
     - `gap`    : space inserted between consecutive children on the main axis;
     - `min`    : minimum width/height (resolved rects never shrink below it);
     - `justify`: distribution of leftover main-axis space (only when no child
                  grows): `JStart`/`JCenter`/`JEnd`/`SpaceBetween`/`SpaceAround`;
     - `align`  : cross-axis placement of children: `Start`/`Center`/`End`, or
                  `Stretch` to fill the cross axis;
     - `tag`    : an opaque caller value, echoed back with the resolved rect so
                  callers can map geometry to their own widgets.

   `solve` places the root's margin box at the viewport, then returns every box
   paired with its absolute resolved rect in PRE-ORDER (parent before children).
   All reals are formatted for output via `fmtReal` (decimal point always,
   leading "-" never "~") so geometry is byte-identical across MLton and Poly/ML. *)

signature LAYOUT =
sig
  datatype dir = Row | Column
  datatype align = Start | Center | End | Stretch
  datatype justify = JStart | JCenter | JEnd | SpaceBetween | SpaceAround

  type edges = { top : real, right : real, bottom : real, left : real }
  type size  = { w : real, h : real }
  type rect  = { x : real, y : real, w : real, h : real }

  datatype 'a box = Box of
    { dir : dir, justify : justify, align : align
    , grow : real, basis : real option
    , padding : edges, margin : edges, gap : real
    , min : size, tag : 'a option
    , children : 'a box list }

  (* Solve within a fixed viewport; returns each box's tag paired with its
     absolute resolved rect, in pre-order (root first). *)
  val solve : { width : real, height : real } -> 'a box -> ('a option * rect) list

  (* Deterministic real formatting: always a decimal point, leading "-" for
     negatives (never SML's "~"). E.g. 10.0 -> "10.0", ~2.5 -> "-2.5". *)
  val fmtReal : real -> string
end
