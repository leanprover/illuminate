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
# joinLines
-/

def testJoinLines_bothEmpty : IO Unit := do
  let result := joinLines (α := Nat) [] []
  assertTrue (result == []) "joinLines [] [] = []"

def testJoinLines_leftEmpty : IO Unit := do
  let result := joinLines (α := Nat) [] [[1, 2]]
  assertTrue (result == [[1, 2]]) "joinLines [] rs = rs"

def testJoinLines_rightEmpty : IO Unit := do
  let result := joinLines (α := Nat) [[1, 2]] []
  assertTrue (result == [[1, 2]]) "joinLines ls [] = ls"

def testJoinLines_merge : IO Unit := do
  let result := joinLines [[1], [2, 3]] [[4], [5]]
  assertTrue (result == [[1], [2, 3, 4], [5]]) "last line merges with first"

/-!
# StyledText combinators
-/

private def base : FontStyle := { fontSize := 10 }

def testStyledText_bareString : IO Unit := do
  let st : StyledText := "hello"
  let result := st.toLines base
  assertTrue (result == [[(base, "hello")]]) "bare string is one line"

def testStyledText_newlineSplit : IO Unit := do
  let st : StyledText := "line1\nline2"
  let result := st.toLines base
  assertTrue (result.length == 2) "newline produces two lines"
  assertTrue (result == [[(base, "line1")], [(base, "line2")]]) "content split correctly"

def testStyledText_trailingNewline : IO Unit := do
  let st : StyledText := "line1\n"
  let result := st.toLines base
  -- The empty second line is what lets `"line1\n" ++ bold "line2"` put "line2"
  -- on its own line: joinLines merges [] with the next fragment's first line.
  assertTrue (result == [[(base, "line1")], []]) "trailing newline produces empty second line"

def testStyledText_append : IO Unit := do
  let st : StyledText := "hello " ++ "world"
  let result := st.toLines base
  assertTrue (result == [[(base, "hello "), (base, "world")]]) "append joins on same line"

def testStyledText_appendAcrossNewline : IO Unit := do
  let st : StyledText := "line1\n" ++ "line2"
  let result := st.toLines base
  assertTrue (result == [[(base, "line1")], [(base, "line2")]]) "append after newline starts new line"

def testStyledText_bold : IO Unit := do
  let st : StyledText := "normal " ++ bold "strong"
  let result := st.toLines base
  assertTrue (result.length == 1) "bold stays on same line"
  match result with
  | [[(_s1, t1), (s2, t2)] ] =>
    assertTrue (t1 == "normal ") "first segment text"
    assertTrue (t2 == "strong") "second segment text"
    assertTrue (s2.bold == true) "bold flag set"
  | _ => throw <| IO.userError s!"unexpected structure: {repr result}"

def testStyledText_family : IO Unit := do
  let st : StyledText := "text " ++ family "monospace" "code"
  let result := st.toLines base
  match result with
  | [[(_, t1), (s2, t2)]] =>
    assertTrue (t1 == "text ") "first segment"
    assertTrue (t2 == "code") "second segment"
    assertTrue (s2.fontFamily == "monospace") "font family set"
  | _ => throw <| IO.userError s!"unexpected structure: {repr result}"

def testStyledText_styled : IO Unit := do
  let st : StyledText := styled ({ · with bold := true, fontFamily := "monospace" }) "both"
  let result := st.toLines base
  match result with
  | [[(s, t)]] =>
    assertTrue (t == "both") "text correct"
    assertTrue (s.bold == true) "bold set"
    assertTrue (s.fontFamily == "monospace") "family set"
  | _ => throw <| IO.userError s!"unexpected structure: {repr result}"

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
    ("Style/gradientVertical", testStyle_gradientVertical),
    -- joinLines (4)
    ("joinLines/bothEmpty", testJoinLines_bothEmpty),
    ("joinLines/leftEmpty", testJoinLines_leftEmpty),
    ("joinLines/rightEmpty", testJoinLines_rightEmpty),
    ("joinLines/merge", testJoinLines_merge),
    -- StyledText combinators (8)
    ("StyledText/bareString", testStyledText_bareString),
    ("StyledText/newlineSplit", testStyledText_newlineSplit),
    ("StyledText/trailingNewline", testStyledText_trailingNewline),
    ("StyledText/append", testStyledText_append),
    ("StyledText/appendAcrossNewline", testStyledText_appendAcrossNewline),
    ("StyledText/bold", testStyledText_bold),
    ("StyledText/family", testStyledText_family),
    ("StyledText/styled", testStyledText_styled)
  ]
