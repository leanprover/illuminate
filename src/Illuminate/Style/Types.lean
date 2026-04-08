/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/
module
public import Illuminate.Geometry.Vec2
public section

namespace Illuminate

/-- An RGBA color with 8-bit channels and a floating-point alpha. -/
structure Color where
  /-- Red channel (0–255). -/
  r : UInt8
  /-- Green channel (0–255). -/
  g : UInt8
  /-- Blue channel (0–255). -/
  b : UInt8
  /-- Alpha (opacity), from 0.0 (transparent) to 1.0 (opaque). -/
  a : Float := 1.0
deriving Repr, BEq, Inhabited, Hashable

/-- Specifies the color of a solid fill. -/
structure FillSpec where
  /-- Fill color. -/
  color : Color :=
    { r := 211, g := 211, b := 211 }
deriving Repr, BEq, Hashable

/-- A color stop in a gradient, positioned at a fractional offset along the gradient axis. -/
structure GradientStop where
  /-- Fractional position along the gradient (0.0 = start, 1.0 = end). -/
  offset : Float
  /-- Color at this stop. -/
  color : Color
deriving Inhabited, Repr, BEq, Hashable

/-- Controls how a gradient extends beyond its defined region. -/
inductive SpreadMethod where
  /-- Extends the terminal colors beyond the gradient bounds. -/
  | pad
  /-- Mirrors the gradient repeatedly. -/
  | reflect
  /-- Tiles the gradient repeatedly. -/
  | repeat
deriving Inhabited, Repr, BEq, Inhabited, Hashable

/--
A gradient fill specification with coordinates in diagram-local space.

Gradient coordinates are absolute in the diagram's local coordinate system and transform
with the diagram when affine transforms (translate, rotate, scale) are applied.
-/
inductive Gradient where
  /-- Linear gradient between two points. -/
  | linear (x1 y1 x2 y2 : Float) (stops : Array GradientStop)
      (spread : SpreadMethod := .pad)
  /--
  Radial gradient between two circles (SVG/Cairo two-circle model).

  The gradient radiates from the focal circle ({name}`fx`, {name}`fy`, {name}`fr`)
  to the outer circle ({name}`cx`, {name}`cy`, {name}`r`).
  -/
  | radial (cx cy r : Float) (fx fy fr : Float)
      (stops : Array GradientStop) (spread : SpreadMethod := .pad)
deriving Inhabited, Repr, BEq, Hashable


/--
Resolved fill style for closed paths, with absolute gradient coordinates.

Used internally by the core primitive and draw command types. User-facing code should
use the {lit}`Fill` type instead, which resolves relative gradients against the shape's envelope.
-/
inductive ResolvedFill where
  /-- No fill — the interior is not rendered and not hittable. -/
  | none
  /-- Solid color fill — the interior is rendered and hittable, even if the color is fully transparent. -/
  | solid : FillSpec → ResolvedFill
  /-- Gradient fill with absolute coordinates — the interior is rendered and hittable. -/
  | gradient : Gradient → ResolvedFill
deriving Repr, BEq, Hashable

/--
User-facing fill style for closed paths.

Gradient variants specify direction and stops relative to the shape; absolute
coordinates are computed automatically from the shape's envelope when the fill
is resolved automatically by shape constructors.
-/
inductive Fill where
  /-- No fill — the interior is not rendered and not hittable. -/
  | none
  /-- Solid color fill. -/
  | solid : FillSpec → Fill
  /-- Linear gradient along a direction, sized to the shape's envelope. -/
  | linearGradient (dir : Vec2) (stops : Array GradientStop)
      (spread : SpreadMethod := .pad) : Fill
  /-- Radial gradient sized to the shape's inscribed circle. -/
  | radialGradient (stops : Array GradientStop) (center : Vec2 := Vec2.zero)
      (focal : Vec2 := Vec2.zero) (spread : SpreadMethod := .pad) : Fill
  /-- Pre-resolved fill with absolute gradient coordinates. -/
  | resolved : ResolvedFill → Fill
deriving Repr, BEq, Hashable


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
  color : Color := { r := 0, g := 0, b := 0 }
  /-- Stroke width in diagram units. -/
  width : Float := 1.0
  /-- Style of line endpoints. -/
  lineCap : LineCap := .butt
  /-- Style of line joins. -/
  lineJoin : LineJoin := .miter
  /-- Dash pattern. -/
  dash : StrokeDash := .solid
deriving Repr, BEq, Inhabited, Hashable

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
  color : Color := { r := 0, g := 0, b := 0 }
  /-- Horizontal anchor point for text layout. -/
  anchor : TextAnchor := .middle
deriving Repr, BEq, Inhabited, Hashable

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
