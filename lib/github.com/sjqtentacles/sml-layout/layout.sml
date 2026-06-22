(* layout.sml

   Implementation of `signature LAYOUT`.

   Each box is allocated an OUTER rectangle (its margin box). Subtracting the
   box's own margin gives its border rect (the rect reported for its tag, never
   smaller than `min`); subtracting padding gives the content rect its children
   are laid out within. Children are distributed along the main axis with a
   flex-grow pass (leftover space shared by `grow` weight), then positioned with
   `justify` (only when nothing grows) and aligned on the cross axis. The walk
   is recursive, so absolute coordinates compose naturally. *)

structure Layout :> LAYOUT =
struct

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

  (* ---- deterministic real formatting (matches Svg.fmtReal) ---- *)
  fun fmtReal r =
    let
      val s = Real.fmt (StringCvt.FIX (SOME 6)) (Real.abs r)
      val (intPart, fracPart) =
        case String.fields (fn c => c = #".") s of
            a :: b :: _ => (a, b)
          | [a] => (a, "")
          | [] => ("0", "")
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

  fun maxr (a, b) = if a > b then a else b

  (* field accessors on a box *)
  fun unbox (Box b) = b

  (* Lay a box out within its outer (margin-box) rect; return pre-order list. *)
  fun layout (outer : rect) (Box b) : ('a option * rect) list =
    let
      val { top, right, bottom, left } = #margin b
      val bx = #x outer + left
      val by = #y outer + top
      val bw = maxr (#w (#min b), #w outer - left - right)
      val bh = maxr (#h (#min b), #h outer - top - bottom)
      val borderRect = { x = bx, y = by, w = bw, h = bh }

      val { top = pt, right = pr, bottom = pb, left = pl } = #padding b
      val content =
        { x = bx + pl, y = by + pt
        , w = maxr (0.0, bw - pl - pr)
        , h = maxr (0.0, bh - pt - pb) }
    in
      (#tag b, borderRect) :: layoutChildren content (Box b)
    end

  and layoutChildren (content : rect) (Box b) =
    let
      val kids = #children b
      val n = List.length kids
    in
      if n = 0 then []
      else
        let
          val isRow = (#dir b = Row)
          val mainSize  = if isRow then #w content else #h content
          val crossSize = if isRow then #h content else #w content

          fun mMargin c =
            let val { top, right, bottom, left } = #margin (unbox c)
            in if isRow then left + right else top + bottom end
          fun mMarginLead c =
            let val { top, left, ... } = #margin (unbox c)
            in if isRow then left else top end
          fun cMargin c =
            let val { top, right, bottom, left } = #margin (unbox c)
            in if isRow then top + bottom else left + right end
          fun baseOf c =
            (case #basis (unbox c) of
                 SOME v => v
               | NONE => if isRow then #w (#min (unbox c)) else #h (#min (unbox c)))
          fun growOf c = #grow (unbox c)
          fun naturalCross c =
            if isRow then #h (#min (unbox c)) else #w (#min (unbox c))

          val totalGap = #gap b * real (n - 1)
          val sumBaseOuter =
            List.foldl (fn (c, acc) => acc + baseOf c + mMargin c) 0.0 kids
          val free = mainSize - sumBaseOuter - totalGap
          val totalGrow = List.foldl (fn (c, acc) => acc + growOf c) 0.0 kids

          fun mainOf c =
            baseOf c
            + (if totalGrow > 0.0 andalso free > 0.0
               then free * growOf c / totalGrow else 0.0)

          val consumed =
            List.foldl (fn (c, acc) => acc + mainOf c + mMargin c) 0.0 kids
            + totalGap
          val leftover = maxr (0.0, mainSize - consumed)

          val (lead, between) =
            if totalGrow > 0.0 then (0.0, #gap b)
            else
              (case #justify b of
                   JStart => (0.0, #gap b)
                 | JCenter => (leftover / 2.0, #gap b)
                 | JEnd => (leftover, #gap b)
                 | SpaceBetween =>
                     (0.0, #gap b + (if n > 1 then leftover / real (n - 1) else 0.0))
                 | SpaceAround =>
                     let val u = leftover / real n
                     in (u / 2.0, #gap b + u) end)

          fun place _ [] = []
            | place cursor (c :: cs) =
                let
                  val ms = mainOf c
                  val outerMain = ms + mMargin c
                  val (outerCross, crossOff) =
                    (case #align b of
                         Stretch => (crossSize, 0.0)
                       | Start => (naturalCross c + cMargin c, 0.0)
                       | Center =>
                           let val oc = naturalCross c + cMargin c
                           in (oc, (crossSize - oc) / 2.0) end
                       | End =>
                           let val oc = naturalCross c + cMargin c
                           in (oc, crossSize - oc) end)
                  val outerRect =
                    if isRow then
                      { x = #x content + cursor, y = #y content + crossOff
                      , w = outerMain, h = outerCross }
                    else
                      { x = #x content + crossOff, y = #y content + cursor
                      , w = outerCross, h = outerMain }
                  val sub = layout outerRect c
                in
                  sub @ place (cursor + outerMain + between) cs
                end
        in
          place lead kids
        end
    end

  fun solve { width, height } root =
    layout { x = 0.0, y = 0.0, w = width, h = height } root
end
