/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/

import Illuminate.Geometry.Vec2
import Illuminate.Geometry.Matrix
import Illuminate.Style.Color


namespace Illuminate

/-- Types that support linear interpolation between two values. -/
class Interpolate (α : Type) where
  /-- Interpolates between {lit}`a` and {lit}`b` at parameter {lit}`t` in {lit}`[0, 1]`. -/
  interpolate : α → α → Float → α

namespace Interpolate

instance : Interpolate Float where
  interpolate a b t := a + t * (b - a)

instance : Interpolate Vec2 where
  interpolate a b t := ⟨a.x + t * (b.x - a.x), a.y + t * (b.y - a.y)⟩

instance : Interpolate Color where
  interpolate a b t :=
    let lerpByte (x y : UInt8) : UInt8 :=
      (x.toFloat + t * (y.toFloat - x.toFloat)).toUInt8
    { r := lerpByte a.r b.r
      g := lerpByte a.g b.g
      b := lerpByte a.b b.b
      a := a.a + t * (b.a - a.a) }

instance : Interpolate Matrix where
  interpolate a b t :=
    { a := a.a + t * (b.a - a.a)
      b := a.b + t * (b.b - a.b)
      tx := a.tx + t * (b.tx - a.tx)
      c := a.c + t * (b.c - a.c)
      d := a.d + t * (b.d - a.d)
      ty := a.ty + t * (b.ty - a.ty) }
