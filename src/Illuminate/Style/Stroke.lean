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

namespace Illuminate.Stroke

/-- Black stroke at the given width with butt cap, miter join, and solid dash. -/
def ofWidth (w : Float) : Stroke :=
  { width := w }

/-- Default arrow/line stroke (black, width 1.5 diagram units). -/
def defaultArrow : Stroke := ofWidth 1.5
