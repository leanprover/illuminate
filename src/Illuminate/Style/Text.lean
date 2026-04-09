/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/
module
public import Illuminate.Style.Types
import Lean.DocString.Syntax
meta import Lean.Parser.Term
public section

namespace Illuminate

instance : Coe TextStyle FontStyle where
  coe s := s.toFontStyle

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

/-- Estimates the total advance width of a styled line (sum of per-span widths). -/
def estimateLineWidth (spans : Array (FontStyle × String)) : Float :=
  spans.foldl (fun acc (style, s) => acc + estimateTextWidth style.fontSize s) 0

/-- Computes the maximum font size across all spans in a line, defaulting to 16 if empty. -/
def lineMaxFontSize (spans : Array (FontStyle × String)) : Float :=
  spans.foldl (fun acc (style, _) => max acc style.fontSize) 0

/-!
# Styled text combinators

{lit}`StyledText` is a Reader over {name}`FontStyle`: given a base style, it produces lines of
styled segments. Bare strings coerce to {lit}`StyledText` via {name}`Coe`, inheriting the base
style and splitting on {lit}`"\n"`. Fragments compose with {lit}`++` via {lit}`joinLines`, which
merges the last line of the left operand with the first line of the right.
-/

/--
Joins two line-structured lists by merging the last line of the left
with the first line of the right. Single-pass recursive traversal.
-/
def joinLines {α : Type} (ls rs : List (List α)) : List (List α) :=
  match ls with
  | [] => rs
  | [last] =>
    match rs with
    | [] => [last]
    | first :: rest => (last ++ first) :: rest
  | head :: tail => head :: joinLines tail rs

instance : Coe String StyledText where
  coe s := .mk fun base =>
    (s.splitOn "\n").map (fun part =>
      if part.isEmpty then [] else [(base, part)])

instance : Append StyledText where
  append st1 st2 :=
    .mk fun base =>
      joinLines (st1.toLines base) (st2.toLines base)

/-- Modifies the ambient style for an inner styled text fragment. -/
def styled (mod : FontStyle → FontStyle) (inner : StyledText) : StyledText :=
  .mk fun base => inner.toLines (mod base)

/-- Sets the font family for an inner styled text fragment. -/
def family (fam : String) := styled ({ · with fontFamily := fam })

/-- Renders an inner styled text fragment in bold. -/
def bold := styled ({ · with bold := true })

/-- Renders an inner styled text fragment in italic. -/
def italic := styled ({ · with italic := true })

/-- Sets the text color for an inner styled text fragment. -/
def colored (c : Color) := styled ({ · with color := c })
