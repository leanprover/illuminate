/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/


namespace Illuminate

/-- The mathematical constant π, computed as `Float.acos (-1.0)`. -/
def pi : Float := Float.acos (-1.0)

/-- Tests whether a float is near zero (absolute value below 1e-12). -/
def nearZero (f : Float) : Bool := f.abs < 1e-12

instance : Hashable Float where
  hash f := hash f.toBits

instance : Hashable Empty where
  hash e := nomatch e
