/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/

namespace Illuminate


/-- Visual style of an arrowhead tip. -/
inductive ArrowType where
  /-- Open two-line head (like LaTeX default). -/
  | latex
  /-- A filled triangular "stealth fighter" head. -/
  | stealth
  /-- A filled equilateral triangle head. -/
  | triangle
  /-- A filled circle at the tip. -/
  | circle
deriving Repr, BEq, Inhabited

/-- Configuration for an arrowhead. -/
structure Arrowhead where
  /-- Visual type of the arrowhead. -/
  type : ArrowType := .latex
  /-- Scaling factor for head length (1 = default 8px). -/
  length : Float := 1
  /-- Scaling factor for head width (1 = default). -/
  width : Float := 1
deriving Repr, BEq, Inhabited
