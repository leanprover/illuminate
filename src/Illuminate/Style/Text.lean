/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/
module
public import Illuminate.Style.Color
import Lean.DocString.Syntax
public section

namespace Illuminate


/-- Horizontal anchor point for text rendering. -/
inductive TextAnchor where
  /-- Text is centered on its position (SVG {lit}`text-anchor="middle"`). -/
  | middle
  /-- Text starts at its position (SVG {lit}`text-anchor="start"`). -/
  | start
  /-- Text ends at its position (SVG {lit}`text-anchor="end"`). -/
  | «end»
deriving Repr, BEq, Inhabited, Hashable

/-- Text rendering style: font family, size, weight, slant, and color. -/
structure TextStyle where
  /-- CSS font family name. -/
  fontFamily : String := "sans-serif"
  /-- Font size in diagram units. -/
  fontSize : Float := 16
  /-- Whether to render in bold weight. -/
  bold : Bool := false
  /-- Whether to render in italic style. -/
  italic : Bool := false
  /-- Text fill color. -/
  color : Color := Color.black
  /-- Horizontal anchor point for text layout. -/
  anchor : TextAnchor := .middle
deriving Repr, BEq, Inhabited, Hashable

/-- Estimates the advance width of a single character for a proportional sans-serif approximation. -/
def estimateCharWidth (fontSize : Float) (c : Char) : Float :=
  if c.isUpper then fontSize * 0.7
  else if c.isLower then fontSize * 0.5
  else if c.isDigit then fontSize * 0.55
  else if c == ' ' then fontSize * 0.3
  else fontSize * 0.4

/-- Estimates the total advance width of a string using per-class character heuristics. -/
def estimateTextWidth (fontSize : Float) (s : String) : Float :=
  s.foldl (fun acc c => acc + estimateCharWidth fontSize c) 0

/-- Font specification for text measurement. -/
structure FontSpec where
  /-- CSS font family name. -/
  family : String := "sans-serif"
  /-- Font size in diagram units. -/
  size : Float := 16
  /-- Whether the font is bold. -/
  bold : Bool := false
  /-- Whether the font is italic. -/
  italic : Bool := false
deriving Repr, BEq, Inhabited, Hashable
