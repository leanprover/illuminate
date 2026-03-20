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

-- ═══════════════════════════════════════════════════════════════
-- Raw intersection type (shared by Trace and StrokeTrace)
-- ═══════════════════════════════════════════════════════════════

/-- A raw ray intersection: the ray parameter and the outward surface normal at the hit point. -/
private structure RawHit where
  /-- Ray parameter where the intersection occurs. -/
  t : Float
  /-- Outward surface normal at the intersection point. -/
  normal : Vec2

/-- Inserts a raw hit into a sorted position (by `t`) in a small array. -/
private def sortedInsertRaw (arr : Array RawHit) (val : RawHit) : Array RawHit := Id.run do
  let mut inserted := false
  let mut result : Array RawHit := Array.mkEmpty (arr.size + 1)
  for h : i in [0:arr.size] do
    if !inserted && val.t ≤ arr[i].t then
      result := result.push val
      inserted := true
    result := result.push arr[i]
  if !inserted then result := result.push val
  return result

-- ═══════════════════════════════════════════════════════════════
-- Ray-segment intersection
-- ═══════════════════════════════════════════════════════════════

/--
Computes the ray parameter and outward normal where the ray `p + t * v` intersects
the line segment from `a` to `b`. The normal points to the left of the segment
direction (from `a` to `b`). Returns `none` if no intersection or `t < 0`.
-/
private def raySegment (p : Point) (v : Vec2) (a b : Vec2) : Option RawHit :=
  let d := Vec2.mk (b.x - a.x) (b.y - a.y)
  let denom := v.x * d.y - v.y * d.x
  if nearZero denom then none
  else
    let ap := Vec2.mk (a.x - p.x) (a.y - p.y)
    let t := (ap.x * d.y - ap.y * d.x) / denom
    let u := (ap.x * v.y - ap.y * v.x) / denom
    if t >= 0 && u >= 0 && u <= 1 then
      some { t, normal := Vec2.normalize ⟨-d.y, d.x⟩ }
    else none

-- ═══════════════════════════════════════════════════════════════
-- Shared shape intersection functions
-- ═══════════════════════════════════════════════════════════════

/--
Computes ray intersections with a circle centered at the origin.
Solves `|p + t*v|² = r²` as a quadratic in `t`.
-/
private def circleHits (radius : Float) (p : Point) (v : Vec2) : Array RawHit := Id.run do
  let a := v.x * v.x + v.y * v.y
  let b := 2 * (p.x * v.x + p.y * v.y)
  let c := p.x * p.x + p.y * p.y - radius * radius
  let disc := b * b - 4 * a * c
  if disc < 0 || nearZero a then return #[]
  let mut result : Array RawHit := #[]
  if nearZero disc then
    let t := -b / (2 * a)
    if t >= 0 then
      let ix := p.x + t * v.x; let iy := p.y + t * v.y
      result := result.push { t, normal := Vec2.normalize ⟨ix, iy⟩ }
  else
    let sqrtDisc := disc.sqrt
    let t1 := (-b - sqrtDisc) / (2 * a)
    let t2 := (-b + sqrtDisc) / (2 * a)
    if t1 >= 0 then
      let ix := p.x + t1 * v.x; let iy := p.y + t1 * v.y
      result := result.push { t := t1, normal := Vec2.normalize ⟨ix, iy⟩ }
    if t2 >= 0 then
      let ix := p.x + t2 * v.x; let iy := p.y + t2 * v.y
      result := result.push { t := t2, normal := Vec2.normalize ⟨ix, iy⟩ }
  return result

/--
Computes ray intersections with an axis-aligned rectangle centered at the origin,
with half-width `hw` and half-height `hh`.
-/
private def rectHits (hw hh : Float) (p : Point) (v : Vec2) : Array RawHit := Id.run do
  let edges := [
    (Vec2.mk (-hw) hh, Vec2.mk hw hh),
    (Vec2.mk hw hh, Vec2.mk hw (-hh)),
    (Vec2.mk hw (-hh), Vec2.mk (-hw) (-hh)),
    (Vec2.mk (-hw) (-hh), Vec2.mk (-hw) hh)
  ]
  let mut result : Array RawHit := #[]
  for (a, b) in edges do
    if let some hit := raySegment p v a b then
      result := sortedInsertRaw result hit
  return result

/--
Computes ray intersections with an ellipse centered at the origin with half-widths `rx` and `ry`.
Solves `(px + t*vx)²/rx² + (py + t*vy)²/ry² = 1`.
-/
private def ellipseHits (rx ry : Float) (p : Point) (v : Vec2) : Array RawHit := Id.run do
  if nearZero rx || nearZero ry then return #[]
  let vx := v.x / rx; let vy := v.y / ry
  let px := p.x / rx; let py := p.y / ry
  let a := vx * vx + vy * vy
  let b := 2 * (px * vx + py * vy)
  let c := px * px + py * py - 1
  let disc := b * b - 4 * a * c
  if disc < 0 || nearZero a then return #[]
  let mut result : Array RawHit := #[]
  if nearZero disc then
    let t := -b / (2 * a)
    if t >= 0 then
      let ix := p.x + t * v.x; let iy := p.y + t * v.y
      result := result.push { t, normal := Vec2.normalize ⟨ix / (rx * rx), iy / (ry * ry)⟩ }
  else
    let sqrtDisc := disc.sqrt
    let t1 := (-b - sqrtDisc) / (2 * a)
    let t2 := (-b + sqrtDisc) / (2 * a)
    if t1 >= 0 then
      let ix := p.x + t1 * v.x; let iy := p.y + t1 * v.y
      result := result.push { t := t1, normal := Vec2.normalize ⟨ix / (rx * rx), iy / (ry * ry)⟩ }
    if t2 >= 0 then
      let ix := p.x + t2 * v.x; let iy := p.y + t2 * v.y
      result := result.push { t := t2, normal := Vec2.normalize ⟨ix / (rx * rx), iy / (ry * ry)⟩ }
  return result

/--
Computes ray intersections with a rounded rectangle centered at the origin.
Tests the 4 straight edges (shortened by corner radius) and the 4 corner arcs.
-/
private def roundedRectHits (hw hh : Float) (cr : Float) (p : Point) (v : Vec2)
    : Array RawHit := Id.run do
  let r := min cr (min hw hh)
  let mut result : Array RawHit := #[]
  -- Straight edges (shortened by corner radius)
  let edges := [
    (Vec2.mk (-hw + r) hh, Vec2.mk (hw - r) hh),
    (Vec2.mk hw (hh - r), Vec2.mk hw (-hh + r)),
    (Vec2.mk (hw - r) (-hh), Vec2.mk (-hw + r) (-hh)),
    (Vec2.mk (-hw) (-hh + r), Vec2.mk (-hw) (hh - r))
  ]
  for (a, b) in edges do
    if let some hit := raySegment p v a b then
      result := sortedInsertRaw result hit
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
            let n := Vec2.normalize ⟨ix, iy⟩
            result := sortedInsertRaw result { t, normal := n }
  return result

/--
Computes ray intersections with a path defined by `PathCmd` commands.
Line segments are tested exactly. Cubic Béziers are subdivided into
short line segments for approximate intersection.
-/
private def pathDataHits (commands : Array PathCmd) (p : Point) (v : Vec2)
    : Array RawHit := Id.run do
  let mut result : Array RawHit := #[]
  let mut current : Vec2 := ⟨0, 0⟩
  let mut subpathStart : Vec2 := ⟨0, 0⟩
  for cmd in commands do
    match cmd with
    | .moveTo pt =>
      current := pt
      subpathStart := pt
    | .lineTo pt =>
      if let some hit := raySegment p v current pt then
        result := sortedInsertRaw result hit
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
        if let some hit := raySegment p v segA segB then
          result := sortedInsertRaw result hit
      current := ep
    | .closePath =>
      if let some hit := raySegment p v current subpathStart then
        result := sortedInsertRaw result hit
      current := subpathStart
  return result

-- ═══════════════════════════════════════════════════════════════
-- Trace
-- ═══════════════════════════════════════════════════════════════

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
-- Primitive shape traces
-- ═══════════════════════════════════════════════════════════════

/-- Trace of a circle centered at the origin with the given radius. -/
def ofCircle (radius : Float) : Trace :=
  ⟨fun p v => (circleHits radius p v).map (·.t)⟩

/-- Trace of an axis-aligned rectangle centered at the origin. -/
def ofRect (hw hh : Float) : Trace :=
  ⟨fun p v => (rectHits hw hh p v).map (·.t)⟩

/-- Trace of an ellipse centered at the origin with half-widths `rx` and `ry`. -/
def ofEllipse (rx ry : Float) : Trace :=
  ⟨fun p v => (ellipseHits rx ry p v).map (·.t)⟩

/-- Trace of a rounded rectangle centered at the origin. -/
def ofRoundedRect (hw hh : Float) (cr : Float) : Trace :=
  ⟨fun p v => (roundedRectHits hw hh cr p v).map (·.t)⟩

/--
Trace of a path defined by `PathCmd` commands.
Line segments are tested exactly. Cubic Béziers are subdivided into
short line segments for approximate intersection.
-/
def ofPathData (commands : Array PathCmd) : Trace :=
  ⟨fun p v => (pathDataHits commands p v).map (·.t)⟩

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
cosine of the angle between the ray and the surface normal. Clamps to avoid
division by near-zero (grazing incidence), capping at `10 * strokeWidth`.
-/
private def apparentWidth (strokeWidth cosTheta : Float) : Float :=
  let absCos := cosTheta.abs
  if absCos < 0.1 then strokeWidth * 10
  else strokeWidth / absCos

/--
Builds a `StrokeHit` from a raw intersection hit, the ray direction `v`, and
the stroke width. Uses the dot product `|v · n|` (cosine of incidence angle)
to compute the apparent width.
-/
private def mkStrokeHit (hit : RawHit) (v : Vec2) (strokeWidth : Float) : StrokeHit :=
  let cosTheta := (v.x * hit.normal.x + v.y * hit.normal.y).abs
  let aw := apparentWidth strokeWidth cosTheta
  { edge := hit.t - aw / 2, width := aw }

/-- Converts an array of raw hits to stroke hits with the given stroke width. -/
private def rawToStroke (hits : Array RawHit) (v : Vec2) (strokeWidth : Float)
    : Array StrokeHit :=
  hits.map fun hit => mkStrokeHit hit v strokeWidth

-- ═══════════════════════════════════════════════════════════════
-- Primitive shape stroke traces
-- ═══════════════════════════════════════════════════════════════

/-- Stroke trace of a circle centered at the origin with the given radius and stroke width. -/
def ofCircle (radius strokeWidth : Float) : StrokeTrace :=
  ⟨fun p v => rawToStroke (circleHits radius p v) v strokeWidth⟩

/-- Stroke trace of an axis-aligned rectangle centered at the origin. -/
def ofRect (hw hh strokeWidth : Float) : StrokeTrace :=
  ⟨fun p v => rawToStroke (rectHits hw hh p v) v strokeWidth⟩

/-- Stroke trace of an ellipse centered at the origin with half-widths `rx` and `ry`. -/
def ofEllipse (rx ry strokeWidth : Float) : StrokeTrace :=
  ⟨fun p v => rawToStroke (ellipseHits rx ry p v) v strokeWidth⟩

/-- Stroke trace of a rounded rectangle centered at the origin. -/
def ofRoundedRect (hw hh cr strokeWidth : Float) : StrokeTrace :=
  ⟨fun p v => rawToStroke (roundedRectHits hw hh cr p v) v strokeWidth⟩

/--
Stroke trace of a path defined by `PathCmd` commands.
Line segments are tested exactly. Cubic Béziers are subdivided into
short line segments for approximate intersection.
-/
def ofPathData (commands : Array PathCmd) (strokeWidth : Float) : StrokeTrace :=
  ⟨fun p v => rawToStroke (pathDataHits commands p v) v strokeWidth⟩

end StrokeTrace
