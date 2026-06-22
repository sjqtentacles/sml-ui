(* entry.sml  -  reset the harness, run every widget suite, then report. *)
fun main () =
  ( Harness.reset ()
  ; LabelTests.run ()
  ; ButtonTests.run ()
  ; CheckboxTests.run ()
  ; SliderTests.run ()
  ; TextFieldTests.run ()
  ; PanelTests.run ()
  ; if Harness.run ()
    then OS.Process.exit OS.Process.success
    else OS.Process.exit OS.Process.failure )
