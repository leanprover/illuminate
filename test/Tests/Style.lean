import Tests.Helpers

open Illuminate

-- ══════════════════════════════════════════════════════════════════
-- Style defaults (5)
-- ══════════════════════════════════════════════════════════════════

def testStyle_fillDefault : IO Unit := do
  let f : Fill := {}
  assertTrue (f.color == Color.black) "default fill is black"

def testStyle_strokeDefault : IO Unit := do
  let s : Stroke := {}
  assertTrue (s.color == Color.black) "default stroke color is black"
  assertTrue (s.width == 1.0) "default stroke width is 1.0"
  assertTrue (s.lineCap == .butt) "default line cap is butt"

def testStyle_textDefault : IO Unit := do
  let t : TextStyle := {}
  assertTrue (t.fontSize == 16) "default font size is 16"
  assertTrue (t.bold == false) "default bold is false"

def testStyle_colorTransparent : IO Unit := do
  assertTrue (Color.transparent.a == 0.0) "transparent alpha is 0"

def testStyle_colorEq : IO Unit := do
  assertTrue (Color.black == Color.black) "black eq black"
  assertTrue (Color.black != Color.white) "black neq white"
  assertTrue (Color.red != Color.blue) "red neq blue"

-- ══════════════════════════════════════════════════════════════════
-- Test list
-- ══════════════════════════════════════════════════════════════════

def styleTests : List (String × IO Unit) :=
  [ -- Style defaults (5)
    ("Style/fillDefault", testStyle_fillDefault),
    ("Style/strokeDefault", testStyle_strokeDefault),
    ("Style/textDefault", testStyle_textDefault),
    ("Style/colorTransparent", testStyle_colorTransparent),
    ("Style/colorEq", testStyle_colorEq)
  ]
