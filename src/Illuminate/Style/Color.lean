/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/

import Illuminate.Geometry.Basic

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

namespace Color

/-- Opaque black. -/
def black : Color := { r := 0, g := 0, b := 0 }
/-- Opaque white. -/
def white : Color := { r := 255, g := 255, b := 255 }
/-- Opaque red. -/
def red : Color := { r := 255, g := 0, b := 0 }
/-- Opaque green. -/
def green : Color := { r := 0, g := 128, b := 0 }
/-- Opaque blue. -/
def blue : Color := { r := 0, g := 0, b := 255 }
/-- Light gray. -/
def lightGray : Color := { r := 211, g := 211, b := 211 }
/-- Fully transparent black. -/
def transparent : Color := { r := 0, g := 0, b := 0, a := 0.0 }

end Color

/-- Specifies the color of a solid fill. -/
structure FillSpec where
  /-- Fill color. -/
  color : Color := Color.lightGray
deriving Repr, BEq, Hashable

instance : Inhabited FillSpec := ⟨{ color := Color.lightGray }⟩

/-- A color stop in a gradient, positioned at a fractional offset along the gradient axis. -/
structure GradientStop where
  /-- Fractional position along the gradient (0.0 = start, 1.0 = end). -/
  offset : Float
  /-- Color at this stop. -/
  color : Color
deriving Repr, BEq, Hashable

/-- Controls how a gradient extends beyond its defined region. -/
inductive SpreadMethod where
  /-- Extends the terminal colors beyond the gradient bounds. -/
  | pad
  /-- Mirrors the gradient repeatedly. -/
  | reflect
  /-- Tiles the gradient repeatedly. -/
  | repeat
deriving Repr, BEq, Inhabited, Hashable

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
deriving Repr, BEq, Hashable

namespace Gradient

/-- Creates a vertical linear gradient spanning the given height, centered at the origin. -/
def vertical (height : Float) (stops : Array GradientStop)
    (spread : SpreadMethod := .pad) : Gradient :=
  .linear 0 (height / 2) 0 (-height / 2) stops spread

/-- Creates a horizontal linear gradient spanning the given width, centered at the origin. -/
def horizontal (width : Float) (stops : Array GradientStop)
    (spread : SpreadMethod := .pad) : Gradient :=
  .linear (-width / 2) 0 (width / 2) 0 stops spread

/-- Creates a centered radial gradient with the given radius. -/
def radialSymmetric (radius : Float) (stops : Array GradientStop)
    (spread : SpreadMethod := .pad) : Gradient :=
  .radial 0 0 radius 0 0 0 stops spread

end Gradient

/--
Fill style for closed paths.

: {name (full := Fill.none)}`none`

  Unfilled; the interior is not rendered and not hittable.

: {name (full := Fill.solid)}`solid`

  Filled with a color; the interior is rendered and hittable, even if the color is fully transparent.

: {name (full := Fill.gradient)}`gradient`

  Gradient fill; the interior is rendered and hittable. Gradient coordinates are in diagram-local
  space and transform with the diagram.
-/
inductive Fill where
  /-- No fill — the interior is not rendered and not hittable. -/
  | none
  /-- Solid color fill — the interior is rendered and hittable, even if the color is fully transparent. -/
  | solid : FillSpec → Fill
  /-- Gradient fill — the interior is rendered and hittable. -/
  | gradient : Gradient → Fill
deriving Repr, BEq, Hashable

instance : Inhabited Fill := ⟨.solid default⟩

instance : Coe Color FillSpec := ⟨FillSpec.mk⟩

instance : Coe FillSpec Fill := ⟨.solid⟩

instance : Coe Gradient Fill := ⟨.gradient⟩
