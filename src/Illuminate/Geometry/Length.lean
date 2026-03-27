/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/


import Illuminate.Geometry.Basic

namespace Illuminate

/--
A length with both a diagram-unit component and a screen-pixel component.

The diagram-unit component scales with transforms and viewBox sizing, while the
screen-pixel component stays constant regardless of zoom. At render time, a length
resolves to {lit}`diag + px * scale` where {lit}`scale` is the diagram-units-per-pixel ratio.
-/
structure Length where
  /-- Diagram-unit component (scales with transforms). -/
  diag : Float := 0
  /-- Screen-pixel component (constant visual size). -/
  px : Float := 0
deriving Repr, BEq, Inhabited, Hashable

namespace Length

/-- A length of zero in both components. -/
def zero : Length := ⟨0, 0⟩

/-- Creates a pure diagram-unit length. -/
def ofDiag (d : Float) : Length := ⟨d, 0⟩

/-- Creates a pure screen-pixel length. -/
def ofPx (p : Float) : Length := ⟨0, p⟩

/--
Resolves a length to diagram units at a given scale.

The scale is {lit}`diagramUnitsPerPixel = envelopeWidth / viewBoxPixelWidth`.
-/
def resolve (l : Length) (scale : Float) : Float :=
  l.diag + l.px * scale

/-- Tests whether both components are zero. -/
def isZero (l : Length) : Bool :=
  l.diag == 0 && l.px == 0

/-- Negates both components. -/
def neg (l : Length) : Length := ⟨-l.diag, -l.px⟩

instance : Neg Length where
  neg := neg

instance : Add Length where
  add a b := ⟨a.diag + b.diag, a.px + b.px⟩

instance : Sub Length where
  sub a b := ⟨a.diag - b.diag, a.px - b.px⟩

instance : HMul Float Length Length where
  hMul s l := ⟨s * l.diag, s * l.px⟩

instance : HDiv Length Float Length where
  hDiv l s := ⟨l.diag / s, l.px / s⟩

/-- Adds a diagram-unit value to a length's diag component. -/
instance : HAdd Float Length Length where
  hAdd f l := ⟨f + l.diag, l.px⟩

/-- Adds a diagram-unit value to a length's diag component. -/
instance : HAdd Length Float Length where
  hAdd l f := ⟨l.diag + f, l.px⟩

/-- Subtracts a diagram-unit value from a length's diag component. -/
instance : HSub Length Float Length where
  hSub l f := ⟨l.diag - f, l.px⟩

instance : Max Length where
  max a b := ⟨max a.diag b.diag, max a.px b.px⟩

instance : OfScientific Length where
  ofScientific m s dp := .ofDiag (OfScientific.ofScientific m s dp)

/-- Numeric literals (e.g. {lit}`1`, {lit}`2`) as pure diagram-unit lengths. -/
instance (n : Nat) : OfNat Length n where
  ofNat := .ofDiag (OfNat.ofNat n)

/-- Coercion from Float to a pure diagram-unit length. -/
instance : Coe Float Length where
  coe f := .ofDiag f

end Length
