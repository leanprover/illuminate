/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/

import Illuminate.Animation.Types


namespace Illuminate

/-- Clamps a float to be at least zero. -/
def clampNonneg (f : Float) : Float :=
  if f > 0 then f else 0

/-- Computes the total duration of a list of steps in seconds. -/
def totalDuration (steps : List Step) : Float :=
  steps.foldl (fun acc s => acc + clampNonneg s.duration) 0

/--
Computes the progress vector for a given absolute time.

Each entry in the result is the corresponding step's progress in `[0, 1]`.
Steps play sequentially: step `i+1` begins when step `i` reaches 1.0.
A step with zero or negative duration is skipped (progress immediately 1.0).
A looping step wraps its progress around rather than clamping at 1.0.
-/
def progressAt (steps : List Step) (time : Float) : List Float :=
  go steps time []
where
  go : List Step → Float → List Float → List Float
    | [], _, acc => acc.reverse
    | s :: rest, remaining, acc =>
      let dur := clampNonneg s.duration
      -- Exact equality is safe: dur is either 0.0 (from clampNonneg on non-positive
      -- input) or the original positive Float, never a computed near-zero value.
      if dur == 0 then
        go rest remaining (1.0 :: acc)
      else if remaining <= 0 then
        go rest 0 (0.0 :: acc)
      else if remaining >= dur then
        -- Step complete; looping steps wrap their final progress value
        if s.loop then
          let raw := remaining / dur
          go rest (remaining - dur) ((raw - raw.floor) :: acc)
        else
          go rest (remaining - dur) (1.0 :: acc)
      else if s.loop then
        let raw := remaining / dur
        go rest 0 ((raw - raw.floor) :: acc)
      else
        go rest 0 ((remaining / dur) :: acc)

/--
Converts a list of progress values to a `Vector Float` of the given size.
Pads with 0.0 if the list is shorter than expected, or truncates if longer.
-/
def progressVector (progress : List Float) (n : Nat) : Vector Float n :=
  Vector.ofFn fun i =>
    match progress[i]? with
    | some v => v
    | none => 0.0
