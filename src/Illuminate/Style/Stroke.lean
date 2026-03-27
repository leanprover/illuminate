/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/

import Illuminate.Style.Color
import Illuminate.Geometry.Length

namespace Illuminate


/-- Line cap style for stroke endpoints. -/
inductive LineCap where
  /-- Flat cap flush with the endpoint. -/
  | butt
  /-- Rounded cap extending past the endpoint. -/
  | round
  /-- Square cap extending past the endpoint. -/
  | square
deriving Repr, BEq, Inhabited, Hashable

/-- Line join style for stroke corners. -/
inductive LineJoin where
  /-- Sharp corner join. -/
  | miter
  /-- Rounded corner join. -/
  | round
  /-- Beveled (flat) corner join. -/
  | bevel
deriving Repr, BEq, Inhabited, Hashable

/-- Semantic dash pattern for stroked paths. -/
inductive StrokeDash where
  /-- Continuous line (default). -/
  | solid
  /-- Dashed line (long gaps). -/
  | dashed
  /-- Dotted line (short gaps). -/
  | dotted
  /-- Alternating dash-dot pattern. -/
  | dashDot
deriving Repr, BEq, Inhabited, Hashable

/-- Stroke style for paths: color, width, line cap, line join, and dash pattern. -/
structure Stroke where
  /-- Stroke color. -/
  color : Color := Color.black
  /-- Stroke width as a {name}`Length` with diagram-unit and pixel components. -/
  width : Length := .ofDiag 1.0
  /-- Style of line endpoints. -/
  lineCap : LineCap := .butt
  /-- Style of line joins. -/
  lineJoin : LineJoin := .miter
  /-- Dash pattern. -/
  dash : StrokeDash := .solid
deriving Repr, BEq, Inhabited, Hashable

namespace Stroke

/-- Black stroke at the given diagram-unit width with butt cap, miter join, and solid dash. -/
def ofWidth (w : Float) : Stroke :=
  { width := .ofDiag w }

/-- Black stroke at the given pixel width with butt cap, miter join, and solid dash. -/
def ofPxWidth (w : Float) : Stroke :=
  { width := .ofPx w }

/-- Default arrow/line stroke (black, width 1.5 diagram units). -/
def defaultArrow : Stroke := ofWidth 1.5

/-- Default arrow/line stroke with scale-invariant width (black, 1.5 screen pixels). -/
def defaultArrowInvariant : Stroke := ofPxWidth 1.5
