/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/

import Tests.Helpers

open Illuminate

-- ══════════════════════════════════════════════════════════════════
-- Style defaults (5)
-- ══════════════════════════════════════════════════════════════════

def testStyle_fillDefault : IO Unit := do
  let f : Fill := {}
  assertTrue (f.color == none) "default fill color is none"

def testStyle_strokeDefault : IO Unit := do
  let s : Stroke := {}
  assertTrue (s.color == none) "default stroke color is none"
  assertTrue (s.width == none) "default stroke width is none"
  assertTrue (s.lineCap == none) "default line cap is none"

def testStyle_textDefault : IO Unit := do
  let t : TextStyle := {}
  assertTrue (t.fontSize == none) "default font size is none"
  assertTrue (t.bold == none) "default bold is none"

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
