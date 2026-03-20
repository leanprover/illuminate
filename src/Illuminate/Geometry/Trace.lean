/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/

import Illuminate.Geometry.Vec2
import Illuminate.Geometry.Point
import Illuminate.Geometry.Matrix
import Illuminate.Geometry.PathData


namespace Illuminate

/--
A trace maps a ray (origin point + unit direction) to the sorted array of
parameter values `t ≥ 0` where the ray intersects a shape's boundary.
Given point `p` and direction `v`, each returned `t` means the ray hits
the boundary at `p + t • v`.
-/
structure Trace where
  /-- The trace function: given a ray origin and unit direction, returns sorted intersection parameters. -/
  trace : Point → Vec2 → Array Float

namespace Trace

/-- The empty trace, which reports no intersections for any ray. -/
def empty : Trace := ⟨fun _ _ => #[]⟩

/-- Queries the trace for all intersection parameters along a ray. -/
def query (t : Trace) (p : Point) (v : Vec2) : Array Float := t.trace p v

/-- Returns the closest (smallest) intersection parameter, if any. -/
def closest (t : Trace) (p : Point) (v : Vec2) : Option Float :=
  let hits := t.trace p v
  if hits.isEmpty then none else some hits[0]!

/-- Merges two sorted arrays of floats into a single sorted array. -/
private def sortedMerge (a b : Array Float) : Array Float := Id.run do
  let mut result := Array.mkEmpty (a.size + b.size)
  let mut i := 0
  let mut j := 0
  while i < a.size && j < b.size do
    if a[i]! ≤ b[j]! then
      result := result.push a[i]!
      i := i + 1
    else
      result := result.push b[j]!
      j := j + 1
  while i < a.size do
    result := result.push a[i]!
    i := i + 1
  while j < b.size do
    result := result.push b[j]!
    j := j + 1
  return result

/-- Combines two traces by merging their intersection results. -/
def union (t1 t2 : Trace) : Trace :=
  ⟨fun p v => sortedMerge (t1.trace p v) (t2.trace p v)⟩

/-- Translates a trace by a displacement vector. -/
def translateBy (d : Vec2) (t : Trace) : Trace :=
  ⟨fun p v => t.trace (Point.mk (p.x - d.x) (p.y - d.y)) v⟩

/-- Transforms a trace by an affine matrix. -/
def transform (m : Matrix) (t : Trace) : Trace :=
  match Matrix.inverse m with
  | none => empty
  | some mInv =>
    ⟨fun p v =>
      let p' := Matrix.applyPoint mInv p
      let v' := Matrix.applyLinear mInv v
      let vLen := v'.length
      if nearZero vLen then #[]
      else
        let scale := 1 / vLen
        let vNorm := Vec2.mk (v'.x / vLen) (v'.y / vLen)
        (t.trace p' vNorm).map (· * scale)⟩

-- ═══════════════════════════════════════════════════════════════
-- Ray-segment intersection
-- ═══════════════════════════════════════════════════════════════

/--
Computes the ray parameter `t` where the ray `p + t * v` intersects the
line segment from `a` to `b`. Returns `none` if no intersection or `t < 0`.
-/
def raySegmentIntersect (p : Point) (v : Vec2) (a b : Vec2) : Option Float :=
  let d := Vec2.mk (b.x - a.x) (b.y - a.y)
  let denom := v.x * d.y - v.y * d.x
  if nearZero denom then none
  else
    let ap := Vec2.mk (a.x - p.x) (a.y - p.y)
    let t := (ap.x * d.y - ap.y * d.x) / denom
    let u := (ap.x * v.y - ap.y * v.x) / denom
    if t >= 0 && u >= 0 && u <= 1 then some t else none

-- ═══════════════════════════════════════════════════════════════
-- Primitive shape traces
-- ═══════════════════════════════════════════════════════════════

/-- Inserts a value into a sorted position in a small array. -/
private def sortedInsert (arr : Array Float) (val : Float) : Array Float := Id.run do
  let mut inserted := false
  let mut result : Array Float := Array.mkEmpty (arr.size + 1)
  for h : i in [0:arr.size] do
    if !inserted && val ≤ arr[i] then
      result := result.push val
      inserted := true
    result := result.push arr[i]
  if !inserted then result := result.push val
  return result

/-- Collects non-negative roots from a quadratic with discriminant ≥ 0 into a sorted array. -/
private def collectQuadRoots (a b sqrtDisc : Float) : Array Float := Id.run do
  let t1 := (-b - sqrtDisc) / (2 * a)
  let t2 := (-b + sqrtDisc) / (2 * a)
  let mut result : Array Float := #[]
  if t1 >= 0 then result := result.push t1
  if t2 >= 0 then result := result.push t2
  return result

/--
Trace of a circle centered at the origin with the given radius.
Solves `|p + t*v|² = r²` as a quadratic in `t`.
-/
def ofCircle (radius : Float) : Trace :=
  ⟨fun p v =>
    let a := v.x * v.x + v.y * v.y
    let b := 2 * (p.x * v.x + p.y * v.y)
    let c := p.x * p.x + p.y * p.y - radius * radius
    let disc := b * b - 4 * a * c
    if disc < 0 || nearZero a then #[]
    else if nearZero disc then
      let t := -b / (2 * a)
      if t >= 0 then #[t] else #[]
    else collectQuadRoots a b disc.sqrt⟩

/--
Trace of an axis-aligned rectangle centered at the origin,
with half-width `hw` and half-height `hh`.
-/
def ofRect (hw hh : Float) : Trace :=
  ⟨fun p v => Id.run do
    let corners := [
      (Vec2.mk (-hw) hh, Vec2.mk hw hh),
      (Vec2.mk hw hh, Vec2.mk hw (-hh)),
      (Vec2.mk hw (-hh), Vec2.mk (-hw) (-hh)),
      (Vec2.mk (-hw) (-hh), Vec2.mk (-hw) hh)
    ]
    let mut result : Array Float := #[]
    for (a, b) in corners do
      if let some t := raySegmentIntersect p v a b then
        result := sortedInsert result t
    return result⟩

/--
Trace of an ellipse centered at the origin with half-widths `rx` and `ry`.
Solves `(px + t*vx)²/rx² + (py + t*vy)²/ry² = 1`.
-/
def ofEllipse (rx ry : Float) : Trace :=
  ⟨fun p v =>
    if nearZero rx || nearZero ry then #[]
    else
      let vx := v.x / rx; let vy := v.y / ry
      let px := p.x / rx; let py := p.y / ry
      let a := vx * vx + vy * vy
      let b := 2 * (px * vx + py * vy)
      let c := px * px + py * py - 1
      let disc := b * b - 4 * a * c
      if disc < 0 || nearZero a then #[]
      else if nearZero disc then
        let t := -b / (2 * a)
        if t >= 0 then #[t] else #[]
      else collectQuadRoots a b disc.sqrt⟩

/--
Trace of a rounded rectangle centered at the origin.
Tests the 4 straight edges (shortened by corner radius) and the 4 corner arcs.
-/
def ofRoundedRect (hw hh : Float) (cr : Float) : Trace :=
  let r := min cr (min hw hh)
  ⟨fun p v => Id.run do
    let mut result : Array Float := #[]
    -- Straight edges (shortened by corner radius)
    let edges := [
      (Vec2.mk (-hw + r) hh, Vec2.mk (hw - r) hh),
      (Vec2.mk hw (hh - r), Vec2.mk hw (-hh + r)),
      (Vec2.mk (hw - r) (-hh), Vec2.mk (-hw + r) (-hh)),
      (Vec2.mk (-hw) (-hh + r), Vec2.mk (-hw) (hh - r))
    ]
    for (a, b) in edges do
      if let some t := raySegmentIntersect p v a b then
        result := sortedInsert result t
    -- Corner arcs: test against the full circle at each corner center,
    -- then filter to the 90° arc range
    let corners := [
      (Vec2.mk (hw - r) (hh - r), 0.0, pi / 2),
      (Vec2.mk (-hw + r) (hh - r), pi / 2, pi),
      (Vec2.mk (-hw + r) (-hh + r), pi, 3 * pi / 2),
      (Vec2.mk (hw - r) (-hh + r), 3 * pi / 2, 2 * pi)
    ]
    for (center, arcStart, arcEnd) in corners do
      let dx := p.x - center.x; let dy := p.y - center.y
      let ac := v.x * v.x + v.y * v.y
      let bc := 2 * (dx * v.x + dy * v.y)
      let cc := dx * dx + dy * dy - r * r
      let disc := bc * bc - 4 * ac * cc
      if disc >= 0 && !nearZero ac then
        let sqrtDisc := disc.sqrt
        for t in [(-bc - sqrtDisc) / (2 * ac), (-bc + sqrtDisc) / (2 * ac)] do
          if t >= 0 then
            let ix := p.x + t * v.x - center.x
            let iy := p.y + t * v.y - center.y
            let mut angle := Float.atan2 iy ix
            if angle < 0 then angle := angle + 2 * pi
            if angle >= arcStart - 1e-9 && angle <= arcEnd + 1e-9 then
              result := sortedInsert result t
    return result⟩

/--
Trace of a path defined by `PathCmd` commands.
Line segments are tested exactly. Cubic Béziers are subdivided into
short line segments for approximate intersection.
-/
def ofPathData (commands : Array PathCmd) : Trace :=
  ⟨fun p v => Id.run do
    let mut result : Array Float := #[]
    let mut current : Vec2 := ⟨0, 0⟩
    let mut subpathStart : Vec2 := ⟨0, 0⟩
    for cmd in commands do
      match cmd with
      | .moveTo pt =>
        current := pt
        subpathStart := pt
      | .lineTo pt =>
        if let some t := raySegmentIntersect p v current pt then
          result := sortedInsert result t
        current := pt
      | .curveTo c1 c2 ep =>
        let nSegs : Nat := 16
        let p0 := current
        for _h : i in [0:nSegs] do
          let sPrev := i.toFloat / nSegs.toFloat
          let s := (i + 1).toFloat / nSegs.toFloat
          let evalBez (t : Float) : Vec2 :=
            let u := 1 - t
            Vec2.mk
              (u*u*u * p0.x + 3*u*u*t * c1.x + 3*u*t*t * c2.x + t*t*t * ep.x)
              (u*u*u * p0.y + 3*u*u*t * c1.y + 3*u*t*t * c2.y + t*t*t * ep.y)
          let segA := evalBez sPrev
          let segB := evalBez s
          if let some t := raySegmentIntersect p v segA segB then
            result := sortedInsert result t
        current := ep
      | .closePath =>
        if let some t := raySegmentIntersect p v current subpathStart then
          result := sortedInsert result t
        current := subpathStart
    return result⟩

end Trace

-- ═══════════════════════════════════════════════════════════════
-- StrokeTrace: width-aware trace
-- ═══════════════════════════════════════════════════════════════

/--
A stroke-aware intersection: the ray parameter where the ray first contacts the
painted edge of a stroked shape, paired with the apparent stroke width along the ray.

The `edge` field gives the offset along the ray where the outer edge of the stroke
begins (i.e., the centerline intersection minus half the apparent width). The `width`
field gives the full apparent stroke width along the ray direction, accounting for
the incidence angle.
-/
structure StrokeHit where
  /-- Ray parameter where the outer edge of the stroke begins. -/
  edge : Float
  /-- Apparent stroke width along the ray direction. -/
  width : Float
deriving Repr, BEq, Inhabited

/--
A stroke-aware trace maps a ray to a sorted array of `StrokeHit` values.
Unlike `Trace` which returns centerline intersections, `StrokeTrace` accounts
for stroke width and incidence angle to report where the ray contacts the
visible painted surface.
-/
structure StrokeTrace where
  /-- The trace function: given a ray origin and unit direction, returns sorted stroke hits. -/
  trace : Point → Vec2 → Array StrokeHit

namespace StrokeTrace

/-- The empty stroke trace, which reports no intersections for any ray. -/
def empty : StrokeTrace := ⟨fun _ _ => #[]⟩

/-- Queries the stroke trace for all hits along a ray. -/
def query (t : StrokeTrace) (p : Point) (v : Vec2) : Array StrokeHit := t.trace p v

/-- Returns the closest (smallest edge parameter) stroke hit, if any. -/
def closest (t : StrokeTrace) (p : Point) (v : Vec2) : Option StrokeHit :=
  let hits := t.trace p v
  if hits.isEmpty then none else some hits[0]!

/-- Inserts a stroke hit into sorted position by edge parameter. -/
private def sortedInsertHit (arr : Array StrokeHit) (val : StrokeHit) : Array StrokeHit := Id.run do
  let mut inserted := false
  let mut result : Array StrokeHit := Array.mkEmpty (arr.size + 1)
  for _h : i in [0:arr.size] do
    if !inserted && val.edge ≤ arr[i]!.edge then
      result := result.push val
      inserted := true
    result := result.push arr[i]!
  if !inserted then result := result.push val
  return result

/-- Merges two sorted arrays of stroke hits. -/
private def sortedMergeHits (a b : Array StrokeHit) : Array StrokeHit := Id.run do
  let mut result := Array.mkEmpty (a.size + b.size)
  let mut i := 0
  let mut j := 0
  while i < a.size && j < b.size do
    if a[i]!.edge ≤ b[j]!.edge then
      result := result.push a[i]!
      i := i + 1
    else
      result := result.push b[j]!
      j := j + 1
  while i < a.size do
    result := result.push a[i]!
    i := i + 1
  while j < b.size do
    result := result.push b[j]!
    j := j + 1
  return result

/-- Combines two stroke traces by merging their results. -/
def union (t1 t2 : StrokeTrace) : StrokeTrace :=
  ⟨fun p v => sortedMergeHits (t1.trace p v) (t2.trace p v)⟩

/-- Translates a stroke trace by a displacement vector. -/
def translateBy (d : Vec2) (t : StrokeTrace) : StrokeTrace :=
  ⟨fun p v => t.trace (Point.mk (p.x - d.x) (p.y - d.y)) v⟩

/-- Transforms a stroke trace by an affine matrix. -/
def transform (m : Matrix) (t : StrokeTrace) : StrokeTrace :=
  match Matrix.inverse m with
  | none => empty
  | some mInv =>
    ⟨fun p v =>
      let p' := Matrix.applyPoint mInv p
      let v' := Matrix.applyLinear mInv v
      let vLen := v'.length
      if nearZero vLen then #[]
      else
        let scale := 1 / vLen
        let vNorm := Vec2.mk (v'.x / vLen) (v'.y / vLen)
        (t.trace p' vNorm).map fun hit =>
          { edge := hit.edge * scale, width := hit.width * scale }⟩

/--
Computes the apparent stroke width along a ray given the stroke width and the
sine of the angle between the ray and the surface normal. Clamps to avoid
division by near-zero (grazing incidence), capping at `10 * strokeWidth`.
-/
private def apparentWidth (strokeWidth sinTheta : Float) : Float :=
  let absSin := sinTheta.abs
  if absSin < 0.1 then strokeWidth * 10
  else strokeWidth / absSin

/--
Builds a `StrokeHit` from a centerline intersection parameter `t`, the ray
direction `v`, the outward surface normal `n`, and the stroke width.
-/
private def mkStrokeHit (t : Float) (v n : Vec2) (strokeWidth : Float) : StrokeHit :=
  let sinTheta := (v.x * n.x + v.y * n.y).abs
  let aw := apparentWidth strokeWidth sinTheta
  { edge := t - aw / 2, width := aw }

-- ═══════════════════════════════════════════════════════════════
-- Ray-segment intersection with normal
-- ═══════════════════════════════════════════════════════════════

/--
Computes the ray parameter and outward normal where the ray intersects a
line segment from `a` to `b`. The normal points to the left of the segment
direction (from `a` to `b`).
-/
private def raySegmentWithNormal (p : Point) (v : Vec2) (a b : Vec2)
    : Option (Float × Vec2) :=
  let d := Vec2.mk (b.x - a.x) (b.y - a.y)
  let denom := v.x * d.y - v.y * d.x
  if nearZero denom then none
  else
    let ap := Vec2.mk (a.x - p.x) (a.y - p.y)
    let t := (ap.x * d.y - ap.y * d.x) / denom
    let u := (ap.x * v.y - ap.y * v.x) / denom
    if t >= 0 && u >= 0 && u <= 1 then
      let n := Vec2.normalize ⟨-d.y, d.x⟩
      some (t, n)
    else none

-- ═══════════════════════════════════════════════════════════════
-- Primitive shape stroke traces
-- ═══════════════════════════════════════════════════════════════

/--
Stroke trace of a circle centered at the origin with the given radius and stroke width.
The normal at any point on the circle is the radial direction.
-/
def ofCircle (radius strokeWidth : Float) : StrokeTrace :=
  ⟨fun p v =>
    let a := v.x * v.x + v.y * v.y
    let b := 2 * (p.x * v.x + p.y * v.y)
    let c := p.x * p.x + p.y * p.y - radius * radius
    let disc := b * b - 4 * a * c
    if disc < 0 || nearZero a then #[]
    else Id.run do
      let mut result : Array StrokeHit := #[]
      if nearZero disc then
        let t := -b / (2 * a)
        if t >= 0 then
          let ix := p.x + t * v.x; let iy := p.y + t * v.y
          let n := Vec2.normalize ⟨ix, iy⟩
          result := result.push (mkStrokeHit t v n strokeWidth)
      else
        let sqrtDisc := disc.sqrt
        let t1 := (-b - sqrtDisc) / (2 * a)
        let t2 := (-b + sqrtDisc) / (2 * a)
        if t1 >= 0 then
          let ix := p.x + t1 * v.x; let iy := p.y + t1 * v.y
          let n := Vec2.normalize ⟨ix, iy⟩
          result := result.push (mkStrokeHit t1 v n strokeWidth)
        if t2 >= 0 then
          let ix := p.x + t2 * v.x; let iy := p.y + t2 * v.y
          let n := Vec2.normalize ⟨ix, iy⟩
          result := result.push (mkStrokeHit t2 v n strokeWidth)
      return result⟩

/--
Stroke trace of an axis-aligned rectangle centered at the origin.
-/
def ofRect (hw hh strokeWidth : Float) : StrokeTrace :=
  ⟨fun p v => Id.run do
    let edges := [
      (Vec2.mk (-hw) hh, Vec2.mk hw hh),
      (Vec2.mk hw hh, Vec2.mk hw (-hh)),
      (Vec2.mk hw (-hh), Vec2.mk (-hw) (-hh)),
      (Vec2.mk (-hw) (-hh), Vec2.mk (-hw) hh)
    ]
    let mut result : Array StrokeHit := #[]
    for (a, b) in edges do
      if let some (t, n) := raySegmentWithNormal p v a b then
        result := sortedInsertHit result (mkStrokeHit t v n strokeWidth)
    return result⟩

/--
Stroke trace of an ellipse centered at the origin with half-widths `rx` and `ry`.
The outward normal at point `(x, y)` on the ellipse is `normalize(x/rx², y/ry²)`.
-/
def ofEllipse (rx ry strokeWidth : Float) : StrokeTrace :=
  ⟨fun p v =>
    if nearZero rx || nearZero ry then #[]
    else Id.run do
      let vx := v.x / rx; let vy := v.y / ry
      let px := p.x / rx; let py := p.y / ry
      let a := vx * vx + vy * vy
      let b := 2 * (px * vx + py * vy)
      let c := px * px + py * py - 1
      let disc := b * b - 4 * a * c
      if disc < 0 || nearZero a then return #[]
      let mut result : Array StrokeHit := #[]
      if nearZero disc then
        let t := -b / (2 * a)
        if t >= 0 then
          let ix := p.x + t * v.x; let iy := p.y + t * v.y
          let n := Vec2.normalize ⟨ix / (rx * rx), iy / (ry * ry)⟩
          result := result.push (mkStrokeHit t v n strokeWidth)
      else
        let sqrtDisc := disc.sqrt
        let t1 := (-b - sqrtDisc) / (2 * a)
        let t2 := (-b + sqrtDisc) / (2 * a)
        if t1 >= 0 then
          let ix := p.x + t1 * v.x; let iy := p.y + t1 * v.y
          let n := Vec2.normalize ⟨ix / (rx * rx), iy / (ry * ry)⟩
          result := result.push (mkStrokeHit t1 v n strokeWidth)
        if t2 >= 0 then
          let ix := p.x + t2 * v.x; let iy := p.y + t2 * v.y
          let n := Vec2.normalize ⟨ix / (rx * rx), iy / (ry * ry)⟩
          result := result.push (mkStrokeHit t2 v n strokeWidth)
      return result⟩

/--
Stroke trace of a rounded rectangle centered at the origin.
-/
def ofRoundedRect (hw hh cr strokeWidth : Float) : StrokeTrace :=
  let r := min cr (min hw hh)
  ⟨fun p v => Id.run do
    let mut result : Array StrokeHit := #[]
    let edges := [
      (Vec2.mk (-hw + r) hh, Vec2.mk (hw - r) hh),
      (Vec2.mk hw (hh - r), Vec2.mk hw (-hh + r)),
      (Vec2.mk (hw - r) (-hh), Vec2.mk (-hw + r) (-hh)),
      (Vec2.mk (-hw) (-hh + r), Vec2.mk (-hw) (hh - r))
    ]
    for (a, b) in edges do
      if let some (t, n) := raySegmentWithNormal p v a b then
        result := sortedInsertHit result (mkStrokeHit t v n strokeWidth)
    let corners := [
      (Vec2.mk (hw - r) (hh - r), 0.0, pi / 2),
      (Vec2.mk (-hw + r) (hh - r), pi / 2, pi),
      (Vec2.mk (-hw + r) (-hh + r), pi, 3 * pi / 2),
      (Vec2.mk (hw - r) (-hh + r), 3 * pi / 2, 2 * pi)
    ]
    for (center, arcStart, arcEnd) in corners do
      let dx := p.x - center.x; let dy := p.y - center.y
      let ac := v.x * v.x + v.y * v.y
      let bc := 2 * (dx * v.x + dy * v.y)
      let cc := dx * dx + dy * dy - r * r
      let disc := bc * bc - 4 * ac * cc
      if disc >= 0 && !nearZero ac then
        let sqrtDisc := disc.sqrt
        for t in [(-bc - sqrtDisc) / (2 * ac), (-bc + sqrtDisc) / (2 * ac)] do
          if t >= 0 then
            let ix := p.x + t * v.x - center.x
            let iy := p.y + t * v.y - center.y
            let mut angle := Float.atan2 iy ix
            if angle < 0 then angle := angle + 2 * pi
            if angle >= arcStart - 1e-9 && angle <= arcEnd + 1e-9 then
              let n := Vec2.normalize ⟨ix, iy⟩
              result := sortedInsertHit result (mkStrokeHit t v n strokeWidth)
    return result⟩

/--
Stroke trace of a path defined by `PathCmd` commands.
Line segments are tested exactly. Cubic Béziers are subdivided into
short line segments for approximate intersection.
-/
def ofPathData (commands : Array PathCmd) (strokeWidth : Float) : StrokeTrace :=
  ⟨fun p v => Id.run do
    let mut result : Array StrokeHit := #[]
    let mut current : Vec2 := ⟨0, 0⟩
    let mut subpathStart : Vec2 := ⟨0, 0⟩
    for cmd in commands do
      match cmd with
      | .moveTo pt =>
        current := pt
        subpathStart := pt
      | .lineTo pt =>
        if let some (t, n) := raySegmentWithNormal p v current pt then
          result := sortedInsertHit result (mkStrokeHit t v n strokeWidth)
        current := pt
      | .curveTo c1 c2 ep =>
        let nSegs : Nat := 16
        let p0 := current
        for _h : i in [0:nSegs] do
          let sPrev := i.toFloat / nSegs.toFloat
          let s := (i + 1).toFloat / nSegs.toFloat
          let evalBez (t : Float) : Vec2 :=
            let u := 1 - t
            Vec2.mk
              (u*u*u * p0.x + 3*u*u*t * c1.x + 3*u*t*t * c2.x + t*t*t * ep.x)
              (u*u*u * p0.y + 3*u*u*t * c1.y + 3*u*t*t * c2.y + t*t*t * ep.y)
          let segA := evalBez sPrev
          let segB := evalBez s
          if let some (t, n) := raySegmentWithNormal p v segA segB then
            result := sortedInsertHit result (mkStrokeHit t v n strokeWidth)
        current := ep
      | .closePath =>
        if let some (t, n) := raySegmentWithNormal p v current subpathStart then
          result := sortedInsertHit result (mkStrokeHit t v n strokeWidth)
        current := subpathStart
    return result⟩

end StrokeTrace
