/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/

import Illuminate.Geometry.Basic


namespace Illuminate

/-- A 2D vector with `x` and `y` components. -/
structure Vec2 where
  /-- Horizontal component. -/
  x : Float
  /-- Vertical component. -/
  y : Float
deriving Repr, BEq, Inhabited

namespace Vec2

/-- The zero vector `(0, 0)`. -/
def zero : Vec2 := ⟨0, 0⟩

/-- Adds two vectors component-wise. -/
def add (a b : Vec2) : Vec2 := ⟨a.x + b.x, a.y + b.y⟩
/-- Subtracts `b` from `a` component-wise. -/
def sub (a b : Vec2) : Vec2 := ⟨a.x - b.x, a.y - b.y⟩
/-- Scales a vector by a scalar factor. -/
def scale (s : Float) (v : Vec2) : Vec2 := ⟨s * v.x, s * v.y⟩
/-- Negates both components of a vector. -/
def neg (v : Vec2) : Vec2 := ⟨-v.x, -v.y⟩

/-- Computes the dot product of two vectors. -/
def dot (a b : Vec2) : Float := a.x * b.x + a.y * b.y

/-- Returns the squared length of a vector. -/
def lengthSq (v : Vec2) : Float := v.dot v
/-- Returns the Euclidean length of a vector. -/
def length (v : Vec2) : Float := v.lengthSq.sqrt

/-- Returns a unit vector in the same direction, or zero if the input is zero. -/
def normalize (v : Vec2) : Vec2 :=
  let l := v.length
  if nearZero l then zero else ⟨v.x / l, v.y / l⟩

/-- Rotates a vector by angle `θ` (radians) counter-clockwise. -/
def rotate (θ : Float) (v : Vec2) : Vec2 :=
  let c := θ.cos
  let s := θ.sin
  ⟨c * v.x - s * v.y, s * v.x + c * v.y⟩

instance : Zero Vec2 := ⟨zero⟩
instance : Add Vec2 := ⟨add⟩
instance : Sub Vec2 := ⟨sub⟩
instance : Neg Vec2 := ⟨neg⟩
instance : HMul Float Vec2 Vec2 := ⟨scale⟩
instance : HSMul Float Vec2 Vec2 := ⟨scale⟩

/-- Unit vector in the +x direction. -/
def east : Vec2 := ⟨1, 0⟩
/-- Unit vector in the -x direction. -/
def west : Vec2 := ⟨-1, 0⟩
/-- Unit vector in the +y direction. -/
def north : Vec2 := ⟨0, 1⟩
/-- Unit vector in the -y direction. -/
def south : Vec2 := ⟨0, -1⟩

/-- Unit vector in the +x, +y diagonal direction. -/
def northeast : Vec2 := (⟨1, 1⟩ : Vec2).normalize
/-- Unit vector in the -x, +y diagonal direction. -/
def northwest : Vec2 := (⟨-1, 1⟩ : Vec2).normalize
/-- Unit vector in the +x, -y diagonal direction. -/
def southeast : Vec2 := (⟨1, -1⟩ : Vec2).normalize
/-- Unit vector in the -x, -y diagonal direction. -/
def southwest : Vec2 := (⟨-1, -1⟩ : Vec2).normalize
