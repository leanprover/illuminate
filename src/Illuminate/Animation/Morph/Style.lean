/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/

import Illuminate.Style
import Illuminate.Animation.Interpolate


namespace Illuminate.Morph

open Illuminate

/-!
# Gradient stop normalization

Merges two gradient stop arrays so both have stops at the same offset positions.
Missing stops are filled by interpolating the color between neighboring stops.
-/

/-- Samples a color from a stop array at a given offset by linear interpolation. -/
private def sampleStops (stops : Array GradientStop) (offset : Float) : Color :=
  if stops.size == 0 then Color.black
  else if stops.size == 1 then (stops[0]?.getD (default : GradientStop)).color
  else Id.run do
    -- Find the bracketing stops
    for i in List.range (stops.size - 1) do
      let lo := stops[i]?.getD (default : GradientStop)
      let hi := stops[i + 1]?.getD { offset := 1, color := .black }
      if offset <= hi.offset then
        let range := hi.offset - lo.offset
        if range <= 0 then return lo.color
        let t := (offset - lo.offset) / range
        return Interpolate.interpolate lo.color hi.color t
    -- Past the last stop
    return (stops[stops.size - 1]?.getD { offset := 1, color := .black }).color

/--
Normalizes two gradient stop arrays to share the same offset positions.

Collects all unique offsets from both arrays, then samples each array at every
offset to produce two arrays of equal length with matching positions.
-/
def normalizeStops (a b : Array GradientStop) :
    Array GradientStop × Array GradientStop := Id.run do
  -- Collect all unique offsets, sorted
  let mut offsets : Array Float := #[]
  for s in a do
    offsets := offsets.push s.offset
  for s in b do
    if !offsets.any (·== s.offset) then
      offsets := offsets.push s.offset
  offsets := offsets.qsort (· < ·)
  -- Sample both arrays at each offset
  let stopsA := offsets.map fun o => { offset := o, color := sampleStops a o : GradientStop }
  let stopsB := offsets.map fun o => { offset := o, color := sampleStops b o : GradientStop }
  (stopsA, stopsB)

/-!
# Fill interpolation helpers

These produce pre-normalized fill pairs for the morph plan.
Cross-type fills return {lean}`Option.none`, signaling the caller to use cross-fade.
-/

/-- Lerps a {name}`FillSpec` (solid color). -/
def lerpFillSpec (a b : FillSpec) (t : Float) : FillSpec :=
  { color := Interpolate.interpolate a.color b.color t }

/--
Lerps two gradients of the same type with pre-normalized stops.

Returns {lean}`Option.none` for cross-type gradients (linear vs radial).
-/
def lerpGradient (a b : Gradient) (t : Float) : Option Gradient :=
  let l (x y : Float) := x + t * (y - x)
  let lerpStops (sa sb : Array GradientStop) : Array GradientStop :=
    Array.zipWith (fun (x : GradientStop) (y : GradientStop) =>
      GradientStop.mk (l x.offset y.offset) (Interpolate.interpolate x.color y.color t)) sa sb
  match a, b with
  | .linear ax1 ay1 ax2 ay2 sa _, .linear bx1 by1 bx2 by2 sb _ =>
    some (.linear (l ax1 bx1) (l ay1 by1) (l ax2 bx2) (l ay2 by2) (lerpStops sa sb))
  | .radial acx acy ar afx afy afr sa _, .radial bcx bcy br bfx bfy bfr sb _ =>
    some (.radial (l acx bcx) (l acy bcy) (l ar br)
      (l afx bfx) (l afy bfy) (l afr bfr) (lerpStops sa sb))
  | _, _ => none

/--
Lerps two resolved fills. Returns {lean}`Option.none` when the fills are incompatible
(different gradient types, or none-vs-something), signaling cross-fade.
-/
def lerpResolvedFill (a b : ResolvedFill) (t : Float) : Option ResolvedFill :=
  match a, b with
  | .solid fa, .solid fb => some (.solid (lerpFillSpec fa fb t))
  | .gradient ga, .gradient gb => (lerpGradient ga gb t).map .gradient
  | .none, .none => some .none
  | _, _ => none

/--
Pre-normalizes two resolved fills for the morph plan.

Same-type gradient fills get their stops normalized to matching offsets.
Returns the normalized pair, or {lean}`Option.none` if the fills are incompatible.
-/
def prepareFills (a b : ResolvedFill) : Option (ResolvedFill × ResolvedFill) :=
  match a, b with
  | .solid _, .solid _ => some (a, b)
  | .none, .none => some (a, b)
  | .gradient (.linear x1 y1 x2 y2 sa sp), .gradient (.linear bx1 by1 bx2 by2 sb bsp) =>
    let (na, nb) := normalizeStops sa sb
    some (.gradient (.linear x1 y1 x2 y2 na sp), .gradient (.linear bx1 by1 bx2 by2 nb bsp))
  | .gradient (.radial cx cy r fx fy fr sa sp), .gradient (.radial bcx bcy br bfx bfy bfr sb bsp) =>
    let (na, nb) := normalizeStops sa sb
    some (.gradient (.radial cx cy r fx fy fr na sp), .gradient (.radial bcx bcy br bfx bfy bfr nb bsp))
  | _, _ => none

/-!
# Stroke interpolation
-/

/-- Lerps two strokes: interpolates color and width, discrete-switches cap/join/dash at t=0.5. -/
def lerpStroke (a b : Stroke) (t : Float) : Stroke :=
  { color := Interpolate.interpolate a.color b.color t
    width := a.width + t * (b.width - a.width)
    lineCap := if t < 0.5 then a.lineCap else b.lineCap
    lineJoin := if t < 0.5 then a.lineJoin else b.lineJoin
    dash := if t < 0.5 then a.dash else b.dash }

/-!
# TextStyle interpolation
-/

/-- Lerps two text styles: interpolates fontSize and color, discrete-switches the rest at t=0.5. -/
def lerpTextStyle (a b : TextStyle) (t : Float) : TextStyle :=
  { fontFamily := if t < 0.5 then a.fontFamily else b.fontFamily
    fontSize := a.fontSize + t * (b.fontSize - a.fontSize)
    bold := if t < 0.5 then a.bold else b.bold
    italic := if t < 0.5 then a.italic else b.italic
    color := Interpolate.interpolate a.color b.color t
    anchor := if t < 0.5 then a.anchor else b.anchor }
