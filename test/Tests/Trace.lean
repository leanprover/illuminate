/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/

import Tests.Helpers

set_option linter.missingDocs false

open Illuminate

-- ══════════════════════════════════════════════════════════════════
-- Testable predicates
-- ══════════════════════════════════════════════════════════════════

/-- Checks that a trace query returns exactly `n` intersections. -/
def traceHitsExactly (trace : Trace) (p : Point) (v : Vec2) (n : Nat) : Bool :=
  (trace.query p v).size == n

/-- Checks that the closest intersection is approximately `expected`. -/
def traceClosestApprox (trace : Trace) (p : Point) (v : Vec2) (expected : Float)
    (tol : Float := 1e-6) : Bool :=
  match trace.closest p v with
  | none => false
  | some t => (t - expected).abs < tol

/-- Checks that all intersection parameters are sorted in non-decreasing order. -/
def traceHitsSorted (trace : Trace) (p : Point) (v : Vec2) : Bool :=
  let hits := trace.query p v
  if hits.size ≤ 1 then true
  else Id.run do
    for _h : i in [0:hits.size - 1] do
      if hits[i]! > hits[i + 1]! + 1e-12 then return false
    return true

/-- Checks that all intersection parameters are non-negative. -/
def traceHitsAllPositive (trace : Trace) (p : Point) (v : Vec2) : Bool :=
  let hits := trace.query p v
  hits.all (· >= 0)

/-- Checks that a specific hit (by index) is approximately `expected`. -/
def traceHitApprox (trace : Trace) (p : Point) (v : Vec2) (idx : Nat) (expected : Float)
    (tol : Float := 1e-6) : Bool :=
  let hits := trace.query p v
  match hits[idx]? with
  | none => false
  | some t => (t - expected).abs < tol

-- ══════════════════════════════════════════════════════════════════
-- Circle trace tests
-- ══════════════════════════════════════════════════════════════════

def circleTrace := Trace.ofCircle 10

def testTrace_circle_fromOutside_hitCount : IO Unit := do
  -- Ray from (-20, 0) going east through a circle of radius 10
  assertTrue (traceHitsExactly circleTrace ⟨-20, 0⟩ Vec2.east 2)
    "circle: ray from outside should hit twice"

def testTrace_circle_fromOutside_sorted : IO Unit := do
  assertTrue (traceHitsSorted circleTrace ⟨-20, 0⟩ Vec2.east)
    "circle: hits from outside should be sorted"

def testTrace_circle_fromOutside_positive : IO Unit := do
  assertTrue (traceHitsAllPositive circleTrace ⟨-20, 0⟩ Vec2.east)
    "circle: hits from outside should be positive"

def testTrace_circle_fromOutside_closest : IO Unit := do
  -- From (-20, 0) going east, closest hit is at t=10 (x=-10)
  assertTrue (traceClosestApprox circleTrace ⟨-20, 0⟩ Vec2.east 10)
    "circle: closest from outside should be 10"

def testTrace_circle_fromOutside_farthest : IO Unit := do
  -- Farthest hit at t=30 (x=10)
  assertTrue (traceHitApprox circleTrace ⟨-20, 0⟩ Vec2.east 1 30)
    "circle: farthest from outside should be 30"

def testTrace_circle_fromInside : IO Unit := do
  -- Ray from origin going east, inside circle of radius 10
  assertTrue (traceHitsExactly circleTrace ⟨0, 0⟩ Vec2.east 1)
    "circle: ray from inside should hit once (forward)"

def testTrace_circle_fromInside_closest : IO Unit := do
  assertTrue (traceClosestApprox circleTrace ⟨0, 0⟩ Vec2.east 10)
    "circle: closest from center should be radius"

def testTrace_circle_miss : IO Unit := do
  -- Ray from (0, 20) going east, misses circle of radius 10
  assertTrue (traceHitsExactly circleTrace ⟨0, 20⟩ Vec2.east 0)
    "circle: ray that misses should have 0 hits"

def testTrace_circle_tangent : IO Unit := do
  -- Ray from (0, 10) going east, tangent to circle at (0, 10)
  assertTrue (traceHitsExactly circleTrace ⟨-20, 10⟩ Vec2.east 1)
    "circle: tangent ray should hit once"

-- ══════════════════════════════════════════════════════════════════
-- Rectangle trace tests
-- ══════════════════════════════════════════════════════════════════

def rectTrace := Trace.ofRect 15 10

def testTrace_rect_axisAligned_hitCount : IO Unit := do
  -- Ray from (-30, 0) going east through a 30×20 rect
  assertTrue (traceHitsExactly rectTrace ⟨-30, 0⟩ Vec2.east 2)
    "rect: axis-aligned ray from outside should hit twice"

def testTrace_rect_axisAligned_closest : IO Unit := do
  -- Closest hit at x=-15, so t=15
  assertTrue (traceClosestApprox rectTrace ⟨-30, 0⟩ Vec2.east 15)
    "rect: closest axis-aligned hit should be at edge"

def testTrace_rect_axisAligned_farthest : IO Unit := do
  assertTrue (traceHitApprox rectTrace ⟨-30, 0⟩ Vec2.east 1 45)
    "rect: farthest axis-aligned hit should be at far edge"

def testTrace_rect_diagonal : IO Unit := do
  -- Diagonal ray from (-30, 0) toward (30, 5), hits left and right edges
  let v := Vec2.normalize ⟨1, 0.1⟩
  assertTrue (traceHitsExactly rectTrace ⟨-30, 0⟩ v 2)
    "rect: diagonal ray should hit twice"

def testTrace_rect_diagonal_sorted : IO Unit := do
  let v := Vec2.normalize ⟨1, 0.1⟩
  assertTrue (traceHitsSorted rectTrace ⟨-30, 0⟩ v)
    "rect: diagonal hits should be sorted"

def testTrace_rect_miss : IO Unit := do
  -- Ray going away from rect
  assertTrue (traceHitsExactly rectTrace ⟨-30, 0⟩ Vec2.west 0)
    "rect: ray going away should miss"

def testTrace_rect_fromInside : IO Unit := do
  assertTrue (traceHitsExactly rectTrace ⟨0, 0⟩ Vec2.east 1)
    "rect: ray from inside should hit once"

-- ══════════════════════════════════════════════════════════════════
-- Composition tests
-- ══════════════════════════════════════════════════════════════════

def testTrace_union_hitCount : IO Unit := do
  -- Two circles at the origin, one r=5 inside one r=10
  let t := Trace.union (Trace.ofCircle 5) (Trace.ofCircle 10)
  -- Ray from (-20, 0) going east hits both circles: 4 intersections
  assertTrue (traceHitsExactly t ⟨-20, 0⟩ Vec2.east 4)
    "union: two concentric circles should give 4 hits"

def testTrace_union_sorted : IO Unit := do
  let t := Trace.union (Trace.ofCircle 5) (Trace.ofCircle 10)
  assertTrue (traceHitsSorted t ⟨-20, 0⟩ Vec2.east)
    "union: merged hits should be sorted"

-- ══════════════════════════════════════════════════════════════════
-- Translation tests
-- ══════════════════════════════════════════════════════════════════

def testTrace_translate_hitCount : IO Unit := do
  -- Circle of radius 10 translated to (20, 0)
  let t := Trace.translateBy ⟨20, 0⟩ (Trace.ofCircle 10)
  -- Ray from origin going east should hit the translated circle
  assertTrue (traceHitsExactly t ⟨0, 0⟩ Vec2.east 2)
    "translate: ray should hit translated circle"

def testTrace_translate_closest : IO Unit := do
  let t := Trace.translateBy ⟨20, 0⟩ (Trace.ofCircle 10)
  -- Closest hit at x=10, so t=10
  assertTrue (traceClosestApprox t ⟨0, 0⟩ Vec2.east 10)
    "translate: closest hit of translated circle"

def testTrace_translate_miss : IO Unit := do
  let t := Trace.translateBy ⟨20, 0⟩ (Trace.ofCircle 10)
  -- Ray from origin going west should miss
  assertTrue (traceHitsExactly t ⟨0, 0⟩ Vec2.west 0)
    "translate: ray away from translated circle should miss"

-- ══════════════════════════════════════════════════════════════════
-- Transform tests
-- ══════════════════════════════════════════════════════════════════

def testTrace_transform_translate : IO Unit := do
  let t := Trace.transform (Matrix.translate 20 0) (Trace.ofCircle 10)
  assertTrue (traceHitsExactly t ⟨0, 0⟩ Vec2.east 2)
    "transform translate: should hit circle"

def testTrace_transform_translate_closest : IO Unit := do
  let t := Trace.transform (Matrix.translate 20 0) (Trace.ofCircle 10)
  assertTrue (traceClosestApprox t ⟨0, 0⟩ Vec2.east 10 (tol := 1e-4))
    "transform translate: closest hit"

def testTrace_transform_rotate : IO Unit := do
  -- A 30×20 rect rotated 90° becomes a 20×30 rect
  let t := Trace.transform (Matrix.rotate (Illuminate.pi / 2)) (Trace.ofRect 15 10)
  -- Ray from (-20, 0) going east should hit at x=-10 (t=10)
  assertTrue (traceHitsExactly t ⟨-20, 0⟩ Vec2.east 2)
    "transform rotate: rotated rect should have 2 hits"

def testTrace_transform_scale : IO Unit := do
  -- Circle of radius 10 scaled by 2 becomes radius 20
  let t := Trace.transform (Matrix.uniformScale 2) (Trace.ofCircle 10)
  assertTrue (traceClosestApprox t ⟨-30, 0⟩ Vec2.east 10 (tol := 1e-4))
    "transform scale: scaled circle closest hit"

-- ══════════════════════════════════════════════════════════════════
-- StrokeTrace testable predicates
-- ══════════════════════════════════════════════════════════════════

/-- Checks that a stroke trace query returns exactly `n` hits. -/
def strokeHitsExactly (st : StrokeTrace) (p : Point) (v : Vec2) (n : Nat) : Bool :=
  (st.query p v).size == n

/-- Checks that the closest stroke hit's edge is approximately `expected`. -/
def strokeClosestEdgeApprox (st : StrokeTrace) (p : Point) (v : Vec2) (expected : Float)
    (tol : Float := 1e-4) : Bool :=
  match st.closest p v with
  | none => false
  | some hit => (hit.edge - expected).abs < tol

/-- Checks that all stroke hit edges are sorted in non-decreasing order. -/
def strokeHitsSorted (st : StrokeTrace) (p : Point) (v : Vec2) : Bool :=
  let hits := st.query p v
  if hits.size ≤ 1 then true
  else Id.run do
    for _h : i in [0:hits.size - 1] do
      if hits[i]!.edge > hits[i + 1]!.edge + 1e-12 then return false
    return true

/-- Checks that all stroke hit widths are positive. -/
def strokeWidthsPositive (st : StrokeTrace) (p : Point) (v : Vec2) : Bool :=
  let hits := st.query p v
  hits.all (·.width > 0)

/-- Checks that the closest stroke hit edge is less than the centerline trace's closest. -/
def strokeEdgeBeforeCenterline (st : StrokeTrace) (tr : Trace) (p : Point) (v : Vec2) : Bool :=
  match st.closest p v, tr.closest p v with
  | some hit, some ct => hit.edge < ct
  | _, _ => true

-- ══════════════════════════════════════════════════════════════════
-- StrokeTrace circle tests (stroke width 1, 10, 20)
-- ══════════════════════════════════════════════════════════════════

def testStroke_circle_sw1_hitCount : IO Unit := do
  let st := StrokeTrace.ofCircle 10 1
  assertTrue (strokeHitsExactly st ⟨-20, 0⟩ Vec2.east 2)
    "strokeCircle sw=1: should hit twice from outside"

def testStroke_circle_sw1_edgeBefore : IO Unit := do
  let st := StrokeTrace.ofCircle 10 1
  let tr := Trace.ofCircle 10
  assertTrue (strokeEdgeBeforeCenterline st tr ⟨-20, 0⟩ Vec2.east)
    "strokeCircle sw=1: edge should be before centerline"

def testStroke_circle_sw1_closestEdge : IO Unit := do
  -- Perpendicular hit: edge = centerline - strokeWidth/2 = 10 - 0.5 = 9.5
  let st := StrokeTrace.ofCircle 10 1
  assertTrue (strokeClosestEdgeApprox st ⟨-20, 0⟩ Vec2.east 9.5)
    "strokeCircle sw=1: closest edge at perpendicular"

def testStroke_circle_sw10_closestEdge : IO Unit := do
  -- Perpendicular hit: edge = 10 - 5 = 5
  let st := StrokeTrace.ofCircle 10 10
  assertTrue (strokeClosestEdgeApprox st ⟨-20, 0⟩ Vec2.east 5)
    "strokeCircle sw=10: closest edge at perpendicular"

def testStroke_circle_sw20_closestEdge : IO Unit := do
  -- Perpendicular hit: edge = 10 - 10 = 0
  let st := StrokeTrace.ofCircle 10 20
  assertTrue (strokeClosestEdgeApprox st ⟨-20, 0⟩ Vec2.east 0)
    "strokeCircle sw=20: closest edge at perpendicular"

def testStroke_circle_sw10_widthsPositive : IO Unit := do
  let st := StrokeTrace.ofCircle 10 10
  assertTrue (strokeWidthsPositive st ⟨-20, 0⟩ Vec2.east)
    "strokeCircle sw=10: all widths positive"

def testStroke_circle_sw1_sorted : IO Unit := do
  let st := StrokeTrace.ofCircle 10 1
  assertTrue (strokeHitsSorted st ⟨-20, 0⟩ Vec2.east)
    "strokeCircle sw=1: hits sorted"

def testStroke_circle_miss : IO Unit := do
  let st := StrokeTrace.ofCircle 10 1
  assertTrue (strokeHitsExactly st ⟨0, 20⟩ Vec2.east 0)
    "strokeCircle: miss should have 0 hits"

-- ══════════════════════════════════════════════════════════════════
-- StrokeTrace rectangle tests (stroke width 1, 10, 20)
-- ══════════════════════════════════════════════════════════════════

def testStroke_rect_sw1_hitCount : IO Unit := do
  let st := StrokeTrace.ofRect 15 10 1
  assertTrue (strokeHitsExactly st ⟨-30, 0⟩ Vec2.east 2)
    "strokeRect sw=1: should hit twice"

def testStroke_rect_sw1_closestEdge : IO Unit := do
  -- Perpendicular to left edge: edge = 15 - 0.5 = 14.5
  let st := StrokeTrace.ofRect 15 10 1
  assertTrue (strokeClosestEdgeApprox st ⟨-30, 0⟩ Vec2.east 14.5)
    "strokeRect sw=1: closest edge at perpendicular"

def testStroke_rect_sw10_closestEdge : IO Unit := do
  -- Perpendicular: edge = 15 - 5 = 10
  let st := StrokeTrace.ofRect 15 10 10
  assertTrue (strokeClosestEdgeApprox st ⟨-30, 0⟩ Vec2.east 10)
    "strokeRect sw=10: closest edge at perpendicular"

def testStroke_rect_sw20_closestEdge : IO Unit := do
  -- Perpendicular: edge = 15 - 10 = 5
  let st := StrokeTrace.ofRect 15 10 20
  assertTrue (strokeClosestEdgeApprox st ⟨-30, 0⟩ Vec2.east 5)
    "strokeRect sw=20: closest edge at perpendicular"

def testStroke_rect_sw1_edgeBefore : IO Unit := do
  let st := StrokeTrace.ofRect 15 10 1
  let tr := Trace.ofRect 15 10
  assertTrue (strokeEdgeBeforeCenterline st tr ⟨-30, 0⟩ Vec2.east)
    "strokeRect sw=1: edge should be before centerline"

def testStroke_rect_diagonal_wider : IO Unit := do
  -- Diagonal ray: apparent width should be larger than stroke width
  let st := StrokeTrace.ofRect 15 10 10
  let hits := st.query ⟨-30, 0⟩ (Vec2.normalize ⟨1, 0.1⟩)
  match hits[0]? with
  | none => throw <| IO.userError "strokeRect diagonal: expected a hit"
  | some hit =>
    unless hit.width > 10 do
      throw <| IO.userError s!"strokeRect diagonal: apparent width {hit.width} should exceed stroke width 10"

-- ══════════════════════════════════════════════════════════════════
-- StrokeTrace composition and transform tests
-- ══════════════════════════════════════════════════════════════════

def testStroke_union_hitCount : IO Unit := do
  let st := StrokeTrace.union (StrokeTrace.ofCircle 5 2) (StrokeTrace.ofCircle 10 2)
  assertTrue (strokeHitsExactly st ⟨-20, 0⟩ Vec2.east 4)
    "strokeUnion: two concentric circles should give 4 hits"

def testStroke_translate_closestEdge : IO Unit := do
  let st := StrokeTrace.translateBy ⟨20, 0⟩ (StrokeTrace.ofCircle 10 2)
  -- Perpendicular: edge = 10 - 1 = 9
  assertTrue (strokeClosestEdgeApprox st ⟨0, 0⟩ Vec2.east 9)
    "strokeTranslate: closest edge of translated circle"

def testStroke_transform_scale : IO Unit := do
  -- Circle r=10, sw=2, scaled by 2 → r=20, sw=4 visually
  -- Perpendicular from (-40, 0): centerline at t=20, edge = 20 - 2 = 18
  let st := StrokeTrace.transform (Matrix.uniformScale 2) (StrokeTrace.ofCircle 10 2)
  assertTrue (strokeClosestEdgeApprox st ⟨-40, 0⟩ Vec2.east 18 (tol := 0.1))
    "strokeTransform scale: closest edge of scaled circle"

-- ══════════════════════════════════════════════════════════════════
-- Test list
-- ══════════════════════════════════════════════════════════════════

def traceTests : List (String × IO Unit) := [
  ("Trace/circle_fromOutside_hitCount", testTrace_circle_fromOutside_hitCount),
  ("Trace/circle_fromOutside_sorted", testTrace_circle_fromOutside_sorted),
  ("Trace/circle_fromOutside_positive", testTrace_circle_fromOutside_positive),
  ("Trace/circle_fromOutside_closest", testTrace_circle_fromOutside_closest),
  ("Trace/circle_fromOutside_farthest", testTrace_circle_fromOutside_farthest),
  ("Trace/circle_fromInside", testTrace_circle_fromInside),
  ("Trace/circle_fromInside_closest", testTrace_circle_fromInside_closest),
  ("Trace/circle_miss", testTrace_circle_miss),
  ("Trace/circle_tangent", testTrace_circle_tangent),
  ("Trace/rect_axisAligned_hitCount", testTrace_rect_axisAligned_hitCount),
  ("Trace/rect_axisAligned_closest", testTrace_rect_axisAligned_closest),
  ("Trace/rect_axisAligned_farthest", testTrace_rect_axisAligned_farthest),
  ("Trace/rect_diagonal", testTrace_rect_diagonal),
  ("Trace/rect_diagonal_sorted", testTrace_rect_diagonal_sorted),
  ("Trace/rect_miss", testTrace_rect_miss),
  ("Trace/rect_fromInside", testTrace_rect_fromInside),
  ("Trace/union_hitCount", testTrace_union_hitCount),
  ("Trace/union_sorted", testTrace_union_sorted),
  ("Trace/translate_hitCount", testTrace_translate_hitCount),
  ("Trace/translate_closest", testTrace_translate_closest),
  ("Trace/translate_miss", testTrace_translate_miss),
  ("Trace/transform_translate", testTrace_transform_translate),
  ("Trace/transform_translate_closest", testTrace_transform_translate_closest),
  ("Trace/transform_rotate", testTrace_transform_rotate),
  ("Trace/transform_scale", testTrace_transform_scale),
  ("StrokeTrace/circle_sw1_hitCount", testStroke_circle_sw1_hitCount),
  ("StrokeTrace/circle_sw1_edgeBefore", testStroke_circle_sw1_edgeBefore),
  ("StrokeTrace/circle_sw1_closestEdge", testStroke_circle_sw1_closestEdge),
  ("StrokeTrace/circle_sw10_closestEdge", testStroke_circle_sw10_closestEdge),
  ("StrokeTrace/circle_sw20_closestEdge", testStroke_circle_sw20_closestEdge),
  ("StrokeTrace/circle_sw10_widthsPositive", testStroke_circle_sw10_widthsPositive),
  ("StrokeTrace/circle_sw1_sorted", testStroke_circle_sw1_sorted),
  ("StrokeTrace/circle_miss", testStroke_circle_miss),
  ("StrokeTrace/rect_sw1_hitCount", testStroke_rect_sw1_hitCount),
  ("StrokeTrace/rect_sw1_closestEdge", testStroke_rect_sw1_closestEdge),
  ("StrokeTrace/rect_sw10_closestEdge", testStroke_rect_sw10_closestEdge),
  ("StrokeTrace/rect_sw20_closestEdge", testStroke_rect_sw20_closestEdge),
  ("StrokeTrace/rect_sw1_edgeBefore", testStroke_rect_sw1_edgeBefore),
  ("StrokeTrace/rect_diagonal_wider", testStroke_rect_diagonal_wider),
  ("StrokeTrace/union_hitCount", testStroke_union_hitCount),
  ("StrokeTrace/translate_closestEdge", testStroke_translate_closestEdge),
  ("StrokeTrace/transform_scale", testStroke_transform_scale)
]
