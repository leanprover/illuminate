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
# Segment equalization
-/

/--
Equalizes segment counts between two arrays by subdividing the shorter one.

Repeatedly splits the longest segment (by chord length) at its midpoint until
both arrays have the same count.
-/
def equalizeCubics (a b : Array CubicSeg) : Array CubicSeg × Array CubicSeg :=
  if a.size == b.size then (a, b)
  else if a.size < b.size then
    let a' := subdivideToCount a b.size
    (a', b)
  else
    let b' := subdivideToCount b a.size
    (a, b')
where
  /-- Finds the index of the segment with the largest squared chord length. -/
  longestIdx (segs : Array CubicSeg) : Nat := Id.run do
    let mut best := 0
    let mut bestLen := 0.0
    for i in List.range segs.size do
      let len := (segs[i]?.getD default).chordLenSq
      if len > bestLen then
        best := i
        bestLen := len
    best
  /-- Subdivides segments until the array reaches the target count. -/
  subdivideToCount (segs : Array CubicSeg) (target : Nat) : Array CubicSeg := Id.run do
    let mut s := segs
    while s.size < target do
      let idx := longestIdx s
      let seg := s[idx]?.getD default
      let (left, right) := seg.splitAt 0.5
      s := (s.extract 0 idx).push left |>.push right |> (· ++ s.extract (idx + 1) s.size)
    s

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

/--
Equalizes and aligns two segment arrays for interpolation.

Equalizes segment counts, then (for closed paths) finds the optimal rotation alignment.
-/
def prepareSegments (a b : Array CubicSeg) (closed : Bool) :
    Array CubicSeg × Array CubicSeg :=
  let (ea, eb) := equalizeCubics a b
  if closed && ea.size > 0 then
    let offset := alignRotation ea eb
    (ea, rotateArray eb offset)
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
