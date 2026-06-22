(* test_panel.sml  -  a Panel laying out a mix of widgets via sml-layout,
   rendered byte-identically across compilers. *)

structure PanelTests =
struct
  open Support

  val w = Ui.Panel
    { dir = Layout.Column, gap = 6.0
    , children =
        [ Ui.Label "SETTINGS"
        , Ui.Button { id = "ok", label = "OK" }
        , Ui.Checkbox { id = "c", label = "WRAP", checked = true }
        , Ui.Panel { dir = Layout.Row, gap = 8.0
                   , children = [ Ui.Label "X", Ui.Label "Y", Ui.Label "Z" ] } ] }

  fun run () =
    let val cs = shot (240, 160) Ui.init noInput w
    in
      section "panel";
      checkString "mixed panel golden" ("4F06FE81", cs)
    end
end
