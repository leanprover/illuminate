/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/

import Illuminate.Geometry.Basic


namespace Illuminate

/-- An easing function maps normalized time `[0, 1]` to `[0, 1]`. -/
abbrev Easing := Float → Float

namespace Easing

/-- No easing; progress is proportional to time. -/
def linear : Easing := id

/-- Quadratic ease-in; starts slow, accelerates. -/
def easeIn : Easing := fun t => t * t

/-- Quadratic ease-out; starts fast, decelerates. -/
def easeOut : Easing := fun t => t * (2 - t)

/-- Cubic ease-in-out; S-curve with slow start and end. -/
def easeInOut : Easing := fun t =>
  if t < 0.5 then 2 * t * t
  else -1 + (4 - 2 * t) * t

/-- Overshoots slightly then settles at the target. -/
def backOut : Easing := fun t =>
  let c := 1.70158
  1 + (c + 1) * (t - 1) ^ 3 + c * (t - 1) ^ 2

/-- Sinusoidal ease-in-out. -/
def sineInOut : Easing := fun t =>
  0.5 * (1 - Float.cos (pi * t))
