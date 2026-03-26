/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/

import Illuminate.Geometry.Basic
import Illuminate.Geometry.Vec2
import Illuminate.Geometry.PathData


namespace Illuminate

/-- An absolute cubic Bézier segment with explicit start point. -/
structure CubicSeg where
  /-- Start point. -/
  p0 : Vec2
  /-- First control point. -/
  c1 : Vec2
  /-- Second control point. -/
  c2 : Vec2
  /-- End point. -/
  p3 : Vec2
deriving Repr, BEq, Inhabited

namespace CubicSeg

/-- Linearly interpolates two segments at parameter {name}`t`. -/
def lerp (a b : CubicSeg) (t : Float) : CubicSeg :=
  let l (u v : Vec2) : Vec2 := u + t * (v - u)
  { p0 := l a.p0 b.p0, c1 := l a.c1 b.c1, c2 := l a.c2 b.c2, p3 := l a.p3 b.p3 }

/-- Splits a cubic Bézier at parameter {name}`t` using de Casteljau subdivision. -/
def splitAt (seg : CubicSeg) (t : Float) : CubicSeg × CubicSeg :=
  let l (u v : Vec2) : Vec2 := u + t * (v - u)
  let q0 := l seg.p0 seg.c1
  let q1 := l seg.c1 seg.c2
  let q2 := l seg.c2 seg.p3
  let r0 := l q0 q1
  let r1 := l q1 q2
  let s  := l r0 r1
  ({ p0 := seg.p0, c1 := q0, c2 := r0, p3 := s },
   { p0 := s, c1 := r1, c2 := q2, p3 := seg.p3 })

/-- Computes the squared chord length (distance from start to end). -/
def chordLenSq (seg : CubicSeg) : Float :=
  let d := seg.p3 - seg.p0
  d.x * d.x + d.y * d.y

end CubicSeg

/-!
# Line-to-cubic conversion
-/

/-- Converts a line segment to a degenerate cubic with control points at 1/3 and 2/3. -/
def lineToCubic (from_ to : Vec2) : CubicSeg :=
  let d := to - from_
  { p0 := from_, c1 := from_ + (1.0 / 3.0) * d, c2 := from_ + (2.0 / 3.0) * d, p3 := to }

/-!
# Arc-to-cubic conversion
-/

/--
Approximates a single arc segment (sweep ≤ π/2) as a cubic Bézier.

Uses the standard tangent-matching approximation with control arm
length α = 4/3 · tan(dθ/4).
-/
private def arcSegmentToCubic (cx cy rx ry cosPhi sinPhi theta dtheta : Float) : CubicSeg :=
  let alpha := 4.0 / 3.0 * Float.tan (dtheta / 4.0)
  let cos1 := Float.cos theta
  let sin1 := Float.sin theta
  let cos2 := Float.cos (theta + dtheta)
  let sin2 := Float.sin (theta + dtheta)
  -- Endpoints in ellipse-local coordinates
  let ex1 := rx * cos1
  let ey1 := ry * sin1
  let ex2 := rx * cos2
  let ey2 := ry * sin2
  -- Tangent vectors scaled by alpha
  let tx1 := -rx * sin1 * alpha
  let ty1 := ry * cos1 * alpha
  let tx2 := -rx * sin2 * alpha
  let ty2 := ry * cos2 * alpha
  -- Transform to world coordinates
  let toWorld (lx ly : Float) : Vec2 :=
    ⟨cosPhi * lx - sinPhi * ly + cx, sinPhi * lx + cosPhi * ly + cy⟩
  { p0 := toWorld ex1 ey1
    c1 := toWorld (ex1 + tx1) (ey1 + ty1)
    c2 := toWorld (ex2 - tx2) (ey2 - ty2)
    p3 := toWorld ex2 ey2 }

/--
Approximates an SVG elliptical arc as one or more cubic Bézier segments.

Splits arcs with sweep > π/2 into quarter-arc pieces for accuracy.
-/
def arcToCubics (cur : Vec2) (rx ry xRotation : Float) (largeArc sweep : Bool)
    (endpoint : Vec2) : Array CubicSeg :=
  if nearZero (cur.x - endpoint.x) && nearZero (cur.y - endpoint.y) then #[]
  else if nearZero rx || nearZero ry then #[lineToCubic cur endpoint]
  else
    let ac := endpointToCenter cur.x cur.y rx ry xRotation largeArc sweep endpoint.x endpoint.y
    let cosPhi := Float.cos xRotation
    let sinPhi := Float.sin xRotation
    -- Adjust radii the same way endpointToCenter does
    let rxA := rx.abs
    let ryA := ry.abs
    let x1p := cosPhi * (cur.x - endpoint.x) / 2 + sinPhi * (cur.y - endpoint.y) / 2
    let y1p := -sinPhi * (cur.x - endpoint.x) / 2 + cosPhi * (cur.y - endpoint.y) / 2
    let lambda := x1p * x1p / (rxA * rxA) + y1p * y1p / (ryA * ryA)
    let (rxF, ryF) := if lambda > 1 then
      let s := lambda.sqrt; (s * rxA, s * ryA)
    else (rxA, ryA)
    -- Split into segments of at most π/2
    let halfPi := pi / 2
    let numSegs := max 1 (ac.dtheta.abs / halfPi).ceil.toUInt64.toNat
    let step := ac.dtheta / numSegs.toFloat
    Id.run do
      let mut result : Array CubicSeg := #[]
      for i in List.range numSegs do
        let theta := ac.theta1 + i.toFloat * step
        result := result.push (arcSegmentToCubic ac.cx ac.cy rxF ryF cosPhi sinPhi theta step)
      result

/-!
# PathData normalization
-/

/-- A single normalized subpath: an array of cubic segments and whether it is closed. -/
structure NormalizedSubpath where
  /-- The cubic Bézier segments forming this subpath. -/
  segments : Array CubicSeg
  /-- Whether this subpath is closed. -/
  closed : Bool
deriving Repr, BEq, Inhabited

/-- A normalized path: all commands converted to absolute cubic Bézier segments. -/
structure NormalizedPath where
  /-- The subpaths. -/
  subpaths : Array NormalizedSubpath
deriving Repr, BEq, Inhabited

/--
Converts a {name}`PathData` to normalized cubic Bézier form.

All line segments become degenerate cubics and all arcs are approximated
with cubics. Each subpath is tracked separately.
-/
def pathToCubics (pd : PathData) : NormalizedPath := Id.run do
  let mut subpaths : Array NormalizedSubpath := #[]
  let mut segs : Array CubicSeg := #[]
  let mut cur : Vec2 := ⟨0, 0⟩
  let mut subStart : Vec2 := ⟨0, 0⟩
  for cmd in pd.commands do
    match cmd with
    | .moveTo p =>
      if segs.size > 0 then
        subpaths := subpaths.push { segments := segs, closed := false }
        segs := #[]
      cur := p
      subStart := p
    | .lineTo p =>
      segs := segs.push (lineToCubic cur p)
      cur := p
    | .curveTo c1 c2 ep =>
      segs := segs.push { p0 := cur, c1, c2, p3 := ep }
      cur := ep
    | .arcTo rx ry xRot la sw ep =>
      segs := segs ++ arcToCubics cur rx ry xRot la sw ep
      cur := ep
    | .closePath =>
      if !(nearZero (cur.x - subStart.x) && nearZero (cur.y - subStart.y)) then
        segs := segs.push (lineToCubic cur subStart)
      subpaths := subpaths.push { segments := segs, closed := true }
      segs := #[]
      cur := subStart
  if segs.size > 0 then
    subpaths := subpaths.push { segments := segs, closed := false }
  { subpaths }

/-!
# Arc-length computation and resampling
-/

/--
Approximates the arc length of a cubic Bézier segment.

Uses the average of the chord length and the control polygon length,
which is a reasonable O(1) estimate for smooth curves.
-/
def CubicSeg.arcLength (seg : CubicSeg) : Float :=
  let chord := ((seg.p3 - seg.p0).length)
  let poly := (seg.c1 - seg.p0).length + (seg.c2 - seg.c1).length + (seg.p3 - seg.c2).length
  (chord + poly) / 2

/-- Computes cumulative arc lengths for a segment array. Entry {lit}`i` is the total length up to and including segment {lit}`i`. -/
private def cumulativeLengths (segs : Array CubicSeg) : Array Float := Id.run do
  let mut acc := 0.0
  let mut result : Array Float := #[]
  for seg in segs do
    acc := acc + seg.arcLength
    result := result.push acc
  result

/--
Finds the segment index and local parameter for a given arc-length distance.

Given cumulative lengths and a target distance, returns the segment index
and the parametric value within that segment.
-/
private def findParam (cumLens : Array Float)
    (targetLen : Float) : Nat × Float :=
  if cumLens.size == 0 then (0, 0)
  else Id.run do
    for i in List.range cumLens.size do
      let cumEnd := cumLens[i]?.getD 0
      let cumStart := if i == 0 then 0.0 else cumLens[i - 1]?.getD 0
      let segLen := cumEnd - cumStart
      if targetLen <= cumEnd || i == cumLens.size - 1 then
        let localDist := targetLen - cumStart
        let t := if segLen > 0 then Min.min (localDist / segLen) 1.0 |> Max.max 0.0 else 0.0
        return (i, t)
    (cumLens.size - 1, 1.0)

/--
Resamples a segment array to {name}`count` segments at equal arc-length intervals.

This ensures vertices are evenly distributed around the path perimeter,
which produces much better morph results than naive segment subdivision.
-/
def resampleToCount (segs : Array CubicSeg) (count : Nat) : Array CubicSeg :=
  if segs.size == 0 || count == 0 then #[]
  else if count == segs.size then segs
  else Id.run do
    -- Flatten all segments into a single cubic by collecting split points
    let cumLens := cumulativeLengths segs
    let totalLen := cumLens[cumLens.size - 1]?.getD 1.0
    let step := totalLen / count.toFloat
    let mut result : Array CubicSeg := #[]
    -- Walk through original segments, splitting at arc-length boundaries
    let mut segIdx : Nat := 0
    let mut seg : CubicSeg := segs[0]?.getD default
    let mut segCumStart : Float := 0.0
    let mut segLen : Float := seg.arcLength
    -- tConsumed: how much of the current seg has been output (parametric [0,1])
    let mut tConsumed : Float := 0.0
    for i in List.range count do
      let cutLen := (i + 1).toFloat * step
      -- Advance past any fully consumed segments
      while segIdx < segs.size && cutLen > segCumStart + segLen + 1e-9 && tConsumed > 1.0 - 1e-9 do
        segIdx := segIdx + 1
        if segIdx < segs.size then
          seg := segs[segIdx]?.getD default
          segCumStart := cumLens[segIdx - 1]?.getD 0
          segLen := seg.arcLength
          tConsumed := 0.0
      -- How far into the current segment does this cut fall?
      let distIntoSeg := cutLen - segCumStart
      let tCut := if segLen > 1e-12 then
        Min.min (distIntoSeg / segLen) 1.0 |> Max.max 0.0
      else 1.0
      -- Extract the piece from tConsumed to tCut within the current segment
      if tCut <= tConsumed + 1e-9 then
        -- Degenerate: zero-length piece at a point
        let pt := if tConsumed > 1e-9 then (seg.splitAt tConsumed).2.p0 else seg.p0
        result := result.push { p0 := pt, c1 := pt, c2 := pt, p3 := pt }
      else if tConsumed < 1e-9 then
        if tCut > 1.0 - 1e-9 then
          -- Take the whole segment
          result := result.push seg
        else
          -- Take the first portion
          result := result.push (seg.splitAt tCut).1
      else
        -- Take a middle slice: split at tConsumed to get the tail,
        -- then split that tail at the right proportion for tCut
        let tail := (seg.splitAt tConsumed).2
        let tInTail := (tCut - tConsumed) / (1.0 - tConsumed)
        let tInTail := Min.min (Max.max tInTail 0.0) 1.0
        if tInTail > 1.0 - 1e-9 then
          result := result.push tail
        else
          result := result.push (tail.splitAt tInTail).1
      -- Update consumed position
      tConsumed := tCut
      -- If we've consumed the whole segment, advance
      if tConsumed > 1.0 - 1e-9 then
        segIdx := segIdx + 1
        if segIdx < segs.size then
          seg := segs[segIdx]?.getD default
          segCumStart := cumLens[segIdx - 1]?.getD 0
          segLen := seg.arcLength
          tConsumed := 0.0
    result

/--
Equalizes segment counts between two arrays using arc-length resampling.

Both arrays are resampled to the larger count with vertices at equal
arc-length intervals, ensuring even vertex distribution for smooth morphing.
-/
def equalizeCubics (a b : Array CubicSeg) : Array CubicSeg × Array CubicSeg :=
  let target := max a.size b.size |> max 8  -- at least 8 segments for smooth morphing
  (resampleToCount a target, resampleToCount b target)

/-!
# Closed-path rotation alignment
-/

/--
Finds the cyclic rotation offset that minimizes total squared distance between
corresponding start points of two equal-length segment arrays.
-/
def alignRotation (a b : Array CubicSeg) : Nat := Id.run do
  let n := a.size
  if n == 0 then return 0
  let mut bestOffset := 0
  let mut bestCost := 1.0 / 0.0
  for offset in List.range n do
    let mut cost := 0.0
    for i in List.range n do
      let ai := (a[i]?.getD default).p0
      let bi := (b[(i + offset) % n]?.getD default).p0
      let d := ai - bi
      cost := cost + d.x * d.x + d.y * d.y
    if cost < bestCost then
      bestCost := cost
      bestOffset := offset
  bestOffset

/-- Rotates an array by the given offset (cyclic shift). -/
def rotateArray (arr : Array CubicSeg) (offset : Nat) : Array CubicSeg :=
  if arr.size == 0 || offset % arr.size == 0 then arr
  else
    let n := arr.size
    let off := offset % n
    arr.extract off n ++ arr.extract 0 off

/-- Reverses a segment array, flipping each segment's direction. -/
def reverseSegments (arr : Array CubicSeg) : Array CubicSeg :=
  arr.reverse.map fun seg => { p0 := seg.p3, c1 := seg.c2, c2 := seg.c1, p3 := seg.p0 }

/-- Computes the total squared distance between corresponding start points. -/
private def alignCost (a b : Array CubicSeg) (offset : Nat) : Float := Id.run do
  let n := a.size
  let mut cost := 0.0
  for i in List.range n do
    let ai := (a[i]?.getD default).p0
    let bi := (b[(i + offset) % n]?.getD default).p0
    let d := ai - bi
    cost := cost + d.x * d.x + d.y * d.y
  cost

/--
Finds the best rotation offset and winding direction for alignment.

Tries all cyclic rotations of {name}`b` in both forward and reversed winding,
returning the offset, whether to reverse, and the cost.
-/
private def bestAlignment (a b : Array CubicSeg) : Nat × Bool := Id.run do
  let n := a.size
  if n == 0 then return (0, false)
  let bRev := reverseSegments b
  let mut bestOffset := 0
  let mut bestReverse := false
  let mut bestCost := 1.0 / 0.0
  for offset in List.range n do
    let cost := alignCost a b offset
    if cost < bestCost then
      bestCost := cost
      bestOffset := offset
      bestReverse := false
    let costRev := alignCost a bRev offset
    if costRev < bestCost then
      bestCost := costRev
      bestOffset := offset
      bestReverse := true
  (bestOffset, bestReverse)

/--
Equalizes and aligns two segment arrays for interpolation.

Equalizes segment counts via arc-length resampling, then (for closed paths) finds the
optimal rotation and winding direction to minimize vertex displacement.
-/
def prepareSegments (a b : Array CubicSeg) (closed : Bool) :
    Array CubicSeg × Array CubicSeg :=
  let (ea, eb) := equalizeCubics a b
  if closed && ea.size > 0 then
    let (offset, rev) := bestAlignment ea eb
    let eb' := if rev then reverseSegments eb else eb
    (ea, rotateArray eb' offset)
  else
    (ea, eb)

/-!
# Conversion back to PathData
-/

/-- Converts an array of cubic segments back to a {name}`PathData`. -/
def cubicsToPathData (segs : Array CubicSeg) (closed : Bool) : PathData :=
  if segs.size == 0 then PathData.empty
  else Id.run do
    let mut cmds : Array PathCmd := #[]
    cmds := cmds.push (.moveTo (segs[0]?.getD default).p0)
    for seg in segs do
      cmds := cmds.push (.curveTo seg.c1 seg.c2 seg.p3)
    if closed then
      cmds := cmds.push .closePath
    { commands := cmds }

/-- Converts a {name}`NormalizedPath` back to a {name}`PathData`. -/
def normalizedToPathData (np : NormalizedPath) : PathData := Id.run do
  let mut cmds : Array PathCmd := #[]
  for sub in np.subpaths do
    if sub.segments.size > 0 then
      cmds := cmds.push (.moveTo (sub.segments[0]?.getD default).p0)
      for seg in sub.segments do
        cmds := cmds.push (.curveTo seg.c1 seg.c2 seg.p3)
      if sub.closed then
        cmds := cmds.push .closePath
  { commands := cmds }
