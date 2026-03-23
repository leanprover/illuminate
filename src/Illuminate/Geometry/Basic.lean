/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/


namespace Illuminate

/-- The mathematical constant π, computed as {lean}`Float.acos (-1.0)`. -/
def pi : Float := Float.acos (-1.0)

/-- Tests whether a float is near zero (absolute value below 1e-12). -/
def nearZero (f : Float) : Bool := f.abs < 1e-12

instance : Hashable Float where
  hash f :=
    if f != f then 0            -- all NaNs hash the same
    else if f == 0 then hash (0.0 : Float).toBits  -- +0.0 and -0.0 hash the same
    else hash f.toBits

instance : Hashable Empty where
  hash e := nomatch e
