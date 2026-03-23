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

/--
Fill style for closed paths.

- {lit}`none` — unfilled; the interior is not rendered and not hittable.
- {lit}`solid` — filled with a color; the interior is rendered and hittable,
  even if the color is fully transparent.
-/
inductive Fill where
  /-- No fill — the interior is not rendered and not hittable. -/
  | none
  /-- Solid color fill — the interior is rendered and hittable, even if the color is fully transparent. -/
  | solid : FillSpec → Fill
deriving Repr, BEq, Hashable

instance : Inhabited Fill := ⟨.solid default⟩

instance : Coe Color FillSpec := ⟨FillSpec.mk⟩

instance : Coe FillSpec Fill := ⟨.solid⟩
