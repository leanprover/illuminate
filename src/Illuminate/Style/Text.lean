/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/
module
import Lean.DocString.Syntax
meta import Lean.Parser.Term
public section

namespace Illuminate

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
