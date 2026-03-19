/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/

import Illuminate.Style.Color

namespace Illuminate


/-- Line cap style for stroke endpoints. -/
inductive LineCap where
  /-- Flat cap flush with the endpoint. -/
  | butt
  /-- Rounded cap extending past the endpoint. -/
  | round
  /-- Square cap extending past the endpoint. -/
  | square
deriving Repr, BEq, Inhabited

/-- Line join style for stroke corners. -/
inductive LineJoin where
  /-- Sharp corner join. -/
  | miter
  /-- Rounded corner join. -/
  | round
  /-- Beveled (flat) corner join. -/
  | bevel
deriving Repr, BEq, Inhabited

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
deriving Repr, BEq, Inhabited

/-- Stroke style override for paths. Fields set to `none` inherit from the draw config. -/
structure Stroke where
  /-- Stroke color. Inherits from config when `none`. Default: opaque black. -/
  color : Option Color := none
  /-- Stroke width in diagram units. Inherits from config when `none`. Default: 1.0. -/
  width : Option Float := none
  /-- Style of line endpoints. Inherits from config when `none`. Default: butt. -/
  lineCap : Option LineCap := none
  /-- Style of line joins. Inherits from config when `none`. Default: miter. -/
  lineJoin : Option LineJoin := none
  /-- Dash pattern. Inherits from config when `none`. Default: solid. -/
  dash : Option StrokeDash := none
deriving Repr, BEq, Inhabited

/-- Resolved stroke style with all fields concrete. -/
structure FullStroke where
  /-- Stroke color. -/
  color : Color
  /-- Stroke width in diagram units. -/
  width : Float
  /-- Style of line endpoints. -/
  lineCap : LineCap
  /-- Style of line joins. -/
  lineJoin : LineJoin
  /-- Dash pattern. -/
  dash : StrokeDash
deriving Repr, BEq, Inhabited

namespace FullStroke

/-- Black stroke at the given width with butt cap, miter join, and solid dash. -/
def ofWidth (w : Float) : FullStroke :=
  { color := Color.black, width := w, lineCap := .butt, lineJoin := .miter, dash := .solid }

/-- Default arrow/line stroke (black, width 1.5). -/
def defaultArrow : FullStroke := ofWidth 1.5

/-- Converts a resolved stroke back to an override stroke (all fields set). -/
def toStroke (fs : FullStroke) : Stroke :=
  { color := fs.color, width := fs.width, lineCap := fs.lineCap, lineJoin := fs.lineJoin, dash := fs.dash }

end FullStroke
