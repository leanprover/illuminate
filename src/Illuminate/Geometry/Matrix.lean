/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/

import Illuminate.Geometry.Vec2


namespace Illuminate

/--
A 3×3 homogeneous matrix for 2D affine transforms.
Stored as six values (the third row is always [0, 0, 1]):
```
⎡ a  b  tx ⎤
⎢ c  d  ty ⎥
⎣ 0  0   1 ⎦
```
-/
structure Matrix where
  /-- First-row x-scale / rotation component. -/
  a : Float
  /-- First-row y-shear / rotation component. -/
  b : Float
  /-- Horizontal translation. -/
  tx : Float
  /-- Second-row x-shear / rotation component. -/
  c : Float
  /-- Second-row y-scale / rotation component. -/
  d : Float
  /-- Vertical translation. -/
  ty : Float
deriving Repr, BEq, Inhabited

namespace Matrix

/-- The identity matrix (no transformation). -/
def identity : Matrix := ⟨1, 0, 0, 0, 1, 0⟩

/-- Creates a pure translation matrix. -/
def translate (dx dy : Float) : Matrix := ⟨1, 0, dx, 0, 1, dy⟩

/-- Creates an axis-aligned scaling matrix. -/
def scale (sx sy : Float) : Matrix := ⟨sx, 0, 0, 0, sy, 0⟩

/-- Creates a uniform scaling matrix. -/
def uniformScale (s : Float) : Matrix := scale s s

/-- Creates a rotation matrix for angle `θ` (radians) counter-clockwise. -/
def rotate (θ : Float) : Matrix :=
  let c := θ.cos
  let s := θ.sin
  ⟨c, -s, 0, s, c, 0⟩

/-- Creates a shear matrix with factors `kx` (horizontal) and `ky` (vertical). -/
def shear (kx ky : Float) : Matrix := ⟨1, kx, 0, ky, 1, 0⟩

/-- Matrix multiplication (composition). `mul m1 m2` applies `m2` first, then `m1`. -/
def mul (m1 m2 : Matrix) : Matrix :=
  { a := m1.a * m2.a + m1.b * m2.c
    b := m1.a * m2.b + m1.b * m2.d
    tx := m1.a * m2.tx + m1.b * m2.ty + m1.tx
    c := m1.c * m2.a + m1.d * m2.c
    d := m1.c * m2.b + m1.d * m2.d
    ty := m1.c * m2.tx + m1.d * m2.ty + m1.ty }

instance : Mul Matrix := ⟨mul⟩

/-- Determinant of the linear part. -/
def det (m : Matrix) : Float := m.a * m.d - m.b * m.c

/-- Inverse of the matrix, if the determinant is nonzero. -/
def inverse (m : Matrix) : Option Matrix :=
  let d := m.det
  if nearZero d then none
  else
    let invD := 1 / d
    some {
      a := m.d * invD
      b := -m.b * invD
      tx := (m.b * m.ty - m.d * m.tx) * invD
      c := -m.c * invD
      d := m.a * invD
      ty := (m.c * m.tx - m.a * m.ty) * invD
    }

/-- Applies the matrix to a point (full affine transform). -/
def apply (m : Matrix) (v : Vec2) : Vec2 :=
  ⟨m.a * v.x + m.b * v.y + m.tx,
   m.c * v.x + m.d * v.y + m.ty⟩

/-- Applies only the linear part (no translation). Used for transforming directions. -/
def applyLinear (m : Matrix) (v : Vec2) : Vec2 :=
  ⟨m.a * v.x + m.b * v.y,
   m.c * v.x + m.d * v.y⟩
