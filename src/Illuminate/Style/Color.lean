/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/

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
deriving Repr, BEq, Inhabited

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

/-- Fill style override for closed paths. Fields set to `none` inherit from the draw config. -/
structure Fill where
  /-- Fill color. Inherits from config when `none`. Default: opaque black. -/
  color : Option Color := none
deriving Repr, BEq, Inhabited

/-- Resolved fill style with all fields concrete. -/
structure FullFill where
  /-- Fill color. -/
  color : Color
deriving Repr, BEq, Inhabited

namespace FullFill

/-- Converts a resolved fill back to an override fill (color set). -/
def toFill (ff : FullFill) : Fill :=
  { color := ff.color }

end FullFill
