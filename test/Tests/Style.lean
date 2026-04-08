/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/
module
import Tests.Helpers
public section

open Illuminate

/-!
# Style defaults (5)
-/

def testStyle_fillDefault : IO Unit := do
  let f : Fill := default
  match f with
  | .solid fs => assertTrue (fs.color == Color.lightGray) "default fill color is lightGray"
  | _ => throw <| IO.userError "expected solid fill"

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

/-!
# Gradient types (5)
-/

def testStyle_gradientStop : IO Unit := do
  let s : GradientStop := { offset := 0.5, color := Color.red }
  assertApproxEq s.offset 0.5 "stop offset"
  assertTrue (s.color == Color.red) "stop color"

def testStyle_linearGradient : IO Unit := do
  let g := Gradient.linear 0 10 0 (-10) #[
    { offset := 0, color := Color.white },
    { offset := 1, color := Color.blue }
  ]
  match g with
  | .linear x1 y1 x2 _ _ _ =>
    assertApproxEq x1 0 "linear x1"
    assertApproxEq y1 10 "linear y1"
    assertApproxEq x2 0 "linear x2"
  | _ => throw <| IO.userError "expected linear gradient"

def testStyle_radialGradient : IO Unit := do
  let g := Gradient.radialSymmetric 50 #[
    { offset := 0, color := Color.white },
    { offset := 1, color := Color.black }
  ]
  match g with
  | .radial cx _ r _ _ _ _ _ =>
    assertApproxEq cx 0 "radial cx"
    assertApproxEq r 50 "radial r"
  | _ => throw <| IO.userError "expected radial gradient"

def testStyle_gradientFillCoe : IO Unit := do
  let g := Gradient.vertical 100 #[
    { offset := 0, color := Color.red },
    { offset := 1, color := Color.blue }
  ]
  let f : ResolvedFill := g
  match f with
  | .gradient _ => pure ()
  | _ => throw <| IO.userError "expected gradient fill"

def testStyle_gradientVertical : IO Unit := do
  let g := Gradient.vertical 80 #[]
  match g with
  | .linear x1 y1 x2 y2 _ _ =>
    assertApproxEq x1 0 "vertical x1"
    assertApproxEq y1 40 "vertical y1"
    assertApproxEq x2 0 "vertical x2"
    assertApproxEq y2 (-40) "vertical y2"
  | _ => throw <| IO.userError "expected linear gradient"

/-!
# Test list
-/

def styleTests : List (String × IO Unit) :=
  [ -- Style defaults (5)
    ("Style/fillDefault", testStyle_fillDefault),
    ("Style/strokeDefault", testStyle_strokeDefault),
    ("Style/textDefault", testStyle_textDefault),
    ("Style/colorTransparent", testStyle_colorTransparent),
    ("Style/colorEq", testStyle_colorEq),
    -- Gradient types (5)
    ("Style/gradientStop", testStyle_gradientStop),
    ("Style/linearGradient", testStyle_linearGradient),
    ("Style/radialGradient", testStyle_radialGradient),
    ("Style/gradientFillCoe", testStyle_gradientFillCoe),
    ("Style/gradientVertical", testStyle_gradientVertical)
  ]
