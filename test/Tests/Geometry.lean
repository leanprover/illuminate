/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/

import Tests.Helpers

open Illuminate

-- ══════════════════════════════════════════════════════════════════
-- Vec2.dot
-- ══════════════════════════════════════════════════════════════════

def testDot_perpendicular : IO Unit := do
  assertApproxEq (Vec2.east.dot Vec2.north) 0 "perpendicular dot"

def testDot_parallel : IO Unit := do
  let v : Vec2 := ⟨3, 0⟩
  assertApproxEq (v.dot Vec2.east) 3 "parallel dot"

def testDot_antiparallel : IO Unit := do
  let v : Vec2 := ⟨3, 0⟩
  assertApproxEq (v.dot Vec2.west) (-3) "antiparallel dot"

def testDot_commutative : IO Unit := do
  let a : Vec2 := ⟨3, 4⟩
  let b : Vec2 := ⟨-1, 7⟩
  assertApproxEq (a.dot b) (b.dot a) "dot commutative"

def testDot_general : IO Unit := do
  let a : Vec2 := ⟨3, 4⟩
  let b : Vec2 := ⟨1, 2⟩
  assertApproxEq (a.dot b) 11 "dot general"

-- ══════════════════════════════════════════════════════════════════
-- Vec2.length
-- ══════════════════════════════════════════════════════════════════

def testLength_345 : IO Unit := do
  assertApproxEq (Vec2.mk 3 4).length 5 "3-4-5 triangle"

def testLength_unit : IO Unit := do
  assertApproxEq Vec2.east.length 1 "unit east"

def testLength_zero : IO Unit := do
  assertApproxEq Vec2.zero.length 0 "zero length"

def testLength_negative : IO Unit := do
  assertApproxEq (Vec2.mk (-3) (-4)).length 5 "negative components"

def testLength_diagonal : IO Unit := do
  assertApproxEq (Vec2.mk 1 1).length (Float.sqrt 2) "diagonal" (tol := 1e-9)

-- ══════════════════════════════════════════════════════════════════
-- Vec2.normalize
-- ══════════════════════════════════════════════════════════════════

def testNormalize_unit : IO Unit := do
  assertApproxEq (Vec2.mk 3 4).normalize.length 1 "normalized has unit length"

def testNormalize_direction : IO Unit := do
  let n := (Vec2.mk 3 4).normalize
  assertApproxEq n.x 0.6 "normalized x direction"
  assertApproxEq n.y 0.8 "normalized y direction"

def testNormalize_alreadyUnit : IO Unit := do
  assertVec2Eq Vec2.east.normalize Vec2.east "normalizing unit vector"

def testNormalize_zero : IO Unit := do
  assertVec2Eq Vec2.zero.normalize Vec2.zero "normalizing zero returns zero"

def testNormalize_large : IO Unit := do
  let n := (Vec2.mk 1000 0).normalize
  assertVec2Eq n Vec2.east "normalizing large vector"

-- ══════════════════════════════════════════════════════════════════
-- Vec2.rotate
-- ══════════════════════════════════════════════════════════════════

def testRotate_90 : IO Unit := do
  assertVec2Eq (Vec2.rotate (pi / 2) Vec2.east) Vec2.north "90° east→north"

def testRotate_180 : IO Unit := do
  assertVec2Eq (Vec2.rotate pi Vec2.east) Vec2.west "180° east→west" (tol := 1e-9)

def testRotate_360 : IO Unit := do
  let v : Vec2 := ⟨3, 7⟩
  assertVec2Eq (Vec2.rotate (2 * pi) v) v "360° identity" (tol := 1e-7)

def testRotate_negative : IO Unit := do
  assertVec2Eq (Vec2.rotate (-pi / 2) Vec2.east) Vec2.south "-90° east→south"

def testRotate_preservesLength : IO Unit := do
  let v : Vec2 := ⟨3, 4⟩
  assertApproxEq (Vec2.rotate 1.23 v).length v.length "rotation preserves length" (tol := 1e-9)

-- ══════════════════════════════════════════════════════════════════
-- Vec2 arithmetic (add, sub, neg, scale)
-- ══════════════════════════════════════════════════════════════════

def testArith_addZero : IO Unit := do
  let v : Vec2 := ⟨3, 7⟩
  assertVec2Eq (v + Vec2.zero) v "add zero"

def testArith_addCommutative : IO Unit := do
  let a : Vec2 := ⟨1, 2⟩
  let b : Vec2 := ⟨3, 4⟩
  assertVec2Eq (a + b) (b + a) "add commutative"

def testArith_subSelf : IO Unit := do
  let v : Vec2 := ⟨5, 9⟩
  assertVec2Eq (v - v) Vec2.zero "sub self"

def testArith_negNeg : IO Unit := do
  let v : Vec2 := ⟨3, -7⟩
  assertVec2Eq (-(-v)) v "double neg"

def testArith_scaleZero : IO Unit := do
  let v : Vec2 := ⟨3, 4⟩
  assertVec2Eq (0.0 • v) Vec2.zero "scale by zero"

-- ══════════════════════════════════════════════════════════════════
-- Matrix.identity
-- ══════════════════════════════════════════════════════════════════

def testIdentity_apply : IO Unit := do
  assertVec2Eq (Matrix.identity.apply ⟨3, 7⟩) ⟨3, 7⟩ "identity apply"

def testIdentity_applyOrigin : IO Unit := do
  assertVec2Eq (Matrix.identity.apply Vec2.zero) Vec2.zero "identity at origin"

def testIdentity_applyLinear : IO Unit := do
  let v : Vec2 := ⟨5, -3⟩
  assertVec2Eq (Matrix.identity.applyLinear v) v "identity applyLinear"

def testIdentity_det : IO Unit := do
  assertApproxEq Matrix.identity.det 1 "identity det"

def testIdentity_inverse : IO Unit := do
  match Matrix.identity.inverse with
  | none => throw <| IO.userError "identity should be invertible"
  | some inv => assertVec2Eq (inv.apply ⟨3, 7⟩) ⟨3, 7⟩ "identity inverse"

-- ══════════════════════════════════════════════════════════════════
-- Matrix.translate
-- ══════════════════════════════════════════════════════════════════

def testTranslate_basic : IO Unit := do
  assertVec2Eq (Matrix.translate 5 (-3) |>.apply ⟨1, 2⟩) ⟨6, -1⟩ "translate basic"

def testTranslate_origin : IO Unit := do
  assertVec2Eq (Matrix.translate 3 4 |>.apply Vec2.zero) ⟨3, 4⟩ "translate origin"

def testTranslate_compose : IO Unit := do
  let m := Matrix.translate 1 2 * Matrix.translate 3 4
  assertVec2Eq (m.apply Vec2.zero) ⟨4, 6⟩ "translate compose"

def testTranslate_inverse : IO Unit := do
  let m := Matrix.translate 5 7
  match m.inverse with
  | none => throw <| IO.userError "translate should be invertible"
  | some inv => assertVec2Eq (inv.apply ⟨5, 7⟩) Vec2.zero "translate inverse"

def testTranslate_linearPart : IO Unit := do
  let v : Vec2 := ⟨3, 4⟩
  assertVec2Eq (Matrix.translate 100 200 |>.applyLinear v) v "translate doesn't affect linear"

-- ══════════════════════════════════════════════════════════════════
-- Matrix.scale
-- ══════════════════════════════════════════════════════════════════

def testScale_basic : IO Unit := do
  assertVec2Eq (Matrix.scale 2 3 |>.apply ⟨4, 5⟩) ⟨8, 15⟩ "scale basic"

def testScale_uniform : IO Unit := do
  assertVec2Eq (Matrix.uniformScale 3 |>.apply ⟨2, 5⟩) ⟨6, 15⟩ "uniform scale"

def testScale_origin : IO Unit := do
  assertVec2Eq (Matrix.scale 10 10 |>.apply Vec2.zero) Vec2.zero "scale at origin"

def testScale_compose : IO Unit := do
  let m := Matrix.scale 2 3 * Matrix.scale 4 5
  assertVec2Eq (m.apply ⟨1, 1⟩) ⟨8, 15⟩ "scale compose"

def testScale_det : IO Unit := do
  assertApproxEq (Matrix.scale 2 3).det 6 "scale det"

-- ══════════════════════════════════════════════════════════════════
-- Matrix.rotate
-- ══════════════════════════════════════════════════════════════════

def testMRotate_90 : IO Unit := do
  assertVec2Eq (Matrix.rotate (pi / 2) |>.apply Vec2.east) Vec2.north "rotate 90°"

def testMRotate_180 : IO Unit := do
  assertVec2Eq (Matrix.rotate pi |>.apply Vec2.east) Vec2.west "rotate 180°" (tol := 1e-9)

def testMRotate_composeTwice : IO Unit := do
  let r45 := Matrix.rotate (pi / 4)
  assertVec2Eq ((r45 * r45).apply Vec2.east) Vec2.north "two 45° = 90°" (tol := 1e-9)

def testMRotate_det : IO Unit := do
  assertApproxEq (Matrix.rotate 1.5).det 1 "rotation det = 1" (tol := 1e-9)

def testMRotate_preservesLength : IO Unit := do
  let v : Vec2 := ⟨3, 4⟩
  assertApproxEq (Matrix.rotate 2.1 |>.apply v).length v.length "rotation preserves length" (tol := 1e-9)

-- ══════════════════════════════════════════════════════════════════
-- Matrix.shear
-- ══════════════════════════════════════════════════════════════════

def testShear_xOnly : IO Unit := do
  assertVec2Eq (Matrix.shear 1 0 |>.apply ⟨0, 1⟩) ⟨1, 1⟩ "shear x"

def testShear_yOnly : IO Unit := do
  assertVec2Eq (Matrix.shear 0 1 |>.apply ⟨1, 0⟩) ⟨1, 1⟩ "shear y"

def testShear_identity : IO Unit := do
  assertVec2Eq (Matrix.shear 0 0 |>.apply ⟨3, 7⟩) ⟨3, 7⟩ "shear 0,0 = identity"

def testShear_det2 : IO Unit := do
  assertApproxEq (Matrix.shear 2 3).det (-5) "shear(2,3) det = -5"

def testShear_invertible : IO Unit := do
  let m := Matrix.shear 0.5 0
  match m.inverse with
  | none => throw <| IO.userError "shear should be invertible"
  | some inv =>
    let v : Vec2 := ⟨3, 7⟩
    assertVec2Eq (inv.apply (m.apply v)) v "shear inverse round-trip" (tol := 1e-9)

-- ══════════════════════════════════════════════════════════════════
-- Matrix.mul (composition)
-- ══════════════════════════════════════════════════════════════════

def testMul_scaleThenTranslate : IO Unit := do
  -- s * t: apply t first (translate), then s (scale)
  let composed := Matrix.scale 2 2 * Matrix.translate 1 0
  assertVec2Eq (composed.apply Vec2.zero) ⟨2, 0⟩ "scale ∘ translate"

def testMul_translateThenScale : IO Unit := do
  -- t * s: apply s first (scale), then t (translate)
  let composed := Matrix.translate 1 0 * Matrix.scale 2 2
  assertVec2Eq (composed.apply Vec2.zero) ⟨1, 0⟩ "translate ∘ scale"

def testMul_associative : IO Unit := do
  let a := Matrix.translate 1 2
  let b := Matrix.rotate 0.5
  let c := Matrix.scale 2 3
  let v : Vec2 := ⟨7, 11⟩
  assertVec2Eq ((a * b * c).apply v) ((a * (b * c)).apply v) "associative" (tol := 1e-7)

def testMul_identityLeft : IO Unit := do
  let m := Matrix.translate 3 4 * Matrix.rotate 1.0
  assertVec2Eq ((Matrix.identity * m).apply ⟨5, 6⟩) (m.apply ⟨5, 6⟩) "identity left unit"

def testMul_identityRight : IO Unit := do
  let m := Matrix.translate 3 4 * Matrix.rotate 1.0
  assertVec2Eq ((m * Matrix.identity).apply ⟨5, 6⟩) (m.apply ⟨5, 6⟩) "identity right unit"

-- ══════════════════════════════════════════════════════════════════
-- Matrix.inverse
-- ══════════════════════════════════════════════════════════════════

def testInverse_roundTrip : IO Unit := do
  let m := Matrix.translate 3 4 * Matrix.rotate 1.0 * Matrix.scale 2 3
  match m.inverse with
  | none => throw <| IO.userError "should be invertible"
  | some inv =>
    assertVec2Eq (inv.apply (m.apply ⟨7, 11⟩)) ⟨7, 11⟩ "m⁻¹ ∘ m = id" (tol := 1e-7)

def testInverse_roundTrip2 : IO Unit := do
  let m := Matrix.translate 3 4 * Matrix.rotate 1.0 * Matrix.scale 2 3
  match m.inverse with
  | none => throw <| IO.userError "should be invertible"
  | some inv =>
    assertVec2Eq (m.apply (inv.apply ⟨7, 11⟩)) ⟨7, 11⟩ "m ∘ m⁻¹ = id" (tol := 1e-7)

def testInverse_singular : IO Unit := do
  assertTrue (Matrix.scale 0 1).inverse.isNone "singular has no inverse"

def testInverse_identity : IO Unit := do
  match Matrix.identity.inverse with
  | none => throw <| IO.userError "identity should be invertible"
  | some inv => assertVec2Eq (inv.apply ⟨5, 9⟩) ⟨5, 9⟩ "identity inverse = identity"

def testInverse_translation : IO Unit := do
  match (Matrix.translate 3 7).inverse with
  | none => throw <| IO.userError "translate should be invertible"
  | some inv => assertVec2Eq (inv.apply ⟨3, 7⟩) Vec2.zero "inverse undoes translation"

-- ══════════════════════════════════════════════════════════════════
-- Matrix.apply / applyLinear
-- ══════════════════════════════════════════════════════════════════

def testApply_origin : IO Unit := do
  let m := Matrix.translate 5 3 * Matrix.rotate 1.0
  assertVec2Eq (m.apply Vec2.zero) ⟨5, 3⟩ "transform origin = translation"

def testApply_linearVsAffine : IO Unit := do
  let m := Matrix.translate 10 20 * Matrix.scale 2 3
  let v : Vec2 := ⟨1, 1⟩
  assertVec2Eq (m.apply v) ⟨12, 23⟩ "affine includes translation"
  assertVec2Eq (m.applyLinear v) ⟨2, 3⟩ "linear excludes translation"

def testApply_directionInvariant : IO Unit := do
  -- Translation shouldn't affect directions
  let m := Matrix.translate 100 200
  assertVec2Eq (m.applyLinear Vec2.east) Vec2.east "translate doesn't rotate direction"

def testApply_scaleDirection : IO Unit := do
  let m := Matrix.scale 3 1
  assertVec2Eq (m.applyLinear ⟨2, 0⟩) ⟨6, 0⟩ "scale stretches direction"

def testApply_compositeChain : IO Unit := do
  -- translate(1,0) then rotate 90° then scale 2
  let m := Matrix.uniformScale 2 * Matrix.rotate (pi / 2) * Matrix.translate 1 0
  let result := m.apply Vec2.zero
  -- origin → translate → (1,0) → rotate 90° → (0,1) → scale 2 → (0,2)
  assertVec2Eq result ⟨0, 2⟩ "composite chain" (tol := 1e-9)

-- ══════════════════════════════════════════════════════════════════
-- Envelope.empty
-- ══════════════════════════════════════════════════════════════════

def testEmpty_east : IO Unit := do
  assertApproxEq (Envelope.empty[Vec2.east]) 0 "empty east"

def testEmpty_north : IO Unit := do
  assertApproxEq (Envelope.empty[Vec2.north]) 0 "empty north"

def testEmpty_diagonal : IO Unit := do
  assertApproxEq (Envelope.empty[Vec2.northeast]) 0 "empty diagonal"

def testEmpty_arbitrary : IO Unit := do
  assertApproxEq (Envelope.empty[Vec2.mk 0.3 0.95]) 0 "empty arbitrary"

def testEmpty_unionIdentity : IO Unit := do
  let env := Envelope.ofRect 3 2
  assertApproxEq (Envelope.empty.union env)[Vec2.east] 3 "empty is union identity (east)"
  assertApproxEq (Envelope.empty.union env)[Vec2.north] 2 "empty is union identity (north)"

-- ══════════════════════════════════════════════════════════════════
-- Envelope.ofRect
-- ══════════════════════════════════════════════════════════════════

def testOfRect_cardinals : IO Unit := do
  let env := Envelope.ofRect 3 2
  assertApproxEq env[Vec2.east] 3 "rect east"
  assertApproxEq env[Vec2.west] 3 "rect west"
  assertApproxEq env[Vec2.north] 2 "rect north"
  assertApproxEq env[Vec2.south] 2 "rect south"

def testOfRect_diagonal : IO Unit := do
  let env := Envelope.ofRect 3 2
  assertApproxEq env[Vec2.northeast] (5 / Float.sqrt 2) "rect diagonal" (tol := 1e-7)

def testOfRect_square : IO Unit := do
  let env := Envelope.ofRect 5 5
  assertApproxEq env[Vec2.east] 5 "square east"
  assertApproxEq env[Vec2.north] 5 "square north"
  assertApproxEq env[Vec2.northeast] (10 / Float.sqrt 2) "square diagonal" (tol := 1e-7)

def testOfRect_zero : IO Unit := do
  let env := Envelope.ofRect 0 0
  assertApproxEq env[Vec2.east]  0 "zero rect east"
  assertApproxEq env[Vec2.north] 0 "zero rect north"

def testOfRect_viaSize : IO Unit := do
  let env := Envelope.ofSize 6 4
  assertApproxEq env[Vec2.east]  3 "ofSize east"
  assertApproxEq env[Vec2.north] 2 "ofSize north"

-- ══════════════════════════════════════════════════════════════════
-- Envelope.ofCircle
-- ══════════════════════════════════════════════════════════════════

def testCircle_cardinal : IO Unit := do
  let env := Envelope.ofCircle 5
  assertApproxEq env[Vec2.east]  5 "circle east"
  assertApproxEq env[Vec2.north] 5 "circle north"

def testCircle_diagonal : IO Unit := do
  assertApproxEq (Envelope.ofCircle 5)[Vec2.northeast] 5 "circle diagonal"

def testCircle_arbitrary : IO Unit := do
  assertApproxEq (Envelope.ofCircle 3)[Vec2.mk 0.6 0.8] 3 "circle arbitrary direction"

def testCircle_zero : IO Unit := do
  assertApproxEq (Envelope.ofCircle 0)[Vec2.east] 0 "zero circle"

def testCircle_symmetry : IO Unit := do
  let env := Envelope.ofCircle 7
  assertApproxEq env[Vec2.east] env[Vec2.west] "circle east = west"
  assertApproxEq env[Vec2.north] env[Vec2.south] "circle north = south"

-- ══════════════════════════════════════════════════════════════════
-- Envelope.transform
-- ══════════════════════════════════════════════════════════════════

def testTransform_rotate90 : IO Unit := do
  let env := Envelope.ofRect 3 2
  let rotated := Envelope.transform (Matrix.rotate (pi / 2)) env
  assertApproxEq rotated[Vec2.east] 2 "rotated east" (tol := 1e-7)
  assertApproxEq rotated[Vec2.north] 3 "rotated north" (tol := 1e-7)

def testTransform_uniformScale : IO Unit := do
  let env := Envelope.ofRect 3 2
  let scaled := Envelope.transform (Matrix.uniformScale 2) env
  assertApproxEq scaled[Vec2.east] 6 "scaled east" (tol := 1e-7)
  assertApproxEq scaled[Vec2.north] 4 "scaled north" (tol := 1e-7)

def testTransform_translate : IO Unit := do
  let env := Envelope.ofRect 3 2
  let moved := Envelope.transform (Matrix.translate 10 0) env
  assertApproxEq moved[Vec2.east] 13 "translated east" (tol := 1e-7)
  assertApproxEq moved[Vec2.west] (-7) "translated west" (tol := 1e-7)

def testTransform_identity : IO Unit := do
  let env := Envelope.ofRect 3 2
  let same := Envelope.transform Matrix.identity env
  assertApproxEq same[Vec2.east] 3 "identity transform east"
  assertApproxEq same[Vec2.north] 2 "identity transform north"

def testTransform_nonUniformScale : IO Unit := do
  let env := Envelope.ofRect 1 1
  let scaled := Envelope.transform (Matrix.scale 3 5) env
  assertApproxEq scaled[Vec2.east] 3 "non-uniform scale east" (tol := 1e-7)
  assertApproxEq scaled[Vec2.north] 5 "non-uniform scale north" (tol := 1e-7)

-- ══════════════════════════════════════════════════════════════════
-- Envelope.union
-- ══════════════════════════════════════════════════════════════════

def testUnion_disjointHorizontal : IO Unit := do
  let e1 := Envelope.translateBy ⟨-5, 0⟩ (Envelope.ofRect 1 1)
  let e2 := Envelope.translateBy ⟨5, 0⟩ (Envelope.ofRect 1 1)
  let u := Envelope.union e1 e2
  assertApproxEq u[Vec2.east] 6 "union east"
  assertApproxEq u[Vec2.west] 6 "union west"

def testUnion_disjointVertical : IO Unit := do
  let e1 := Envelope.translateBy ⟨0, -3⟩ (Envelope.ofRect 1 1)
  let e2 := Envelope.translateBy ⟨0, 3⟩ (Envelope.ofRect 1 1)
  let u := Envelope.union e1 e2
  assertApproxEq u[Vec2.north] 4 "union north"
  assertApproxEq u[Vec2.south] 4 "union south"

def testUnion_identical : IO Unit := do
  let env := Envelope.ofRect 3 2
  let u := Envelope.union env env
  assertApproxEq u[Vec2.east] 3 "union identical east"
  assertApproxEq u[Vec2.north] 2 "union identical north"

def testUnion_contained : IO Unit := do
  let small := Envelope.ofRect 1 1
  let big := Envelope.ofRect 5 5
  let u := small ∪ big
  assertApproxEq u[Vec2.east] 5 "union contained east"

def testUnion_commutative : IO Unit := do
  let a := Envelope.translateBy ⟨2, 0⟩ (Envelope.ofRect 1 3)
  let b := Envelope.translateBy ⟨-1, 0⟩ (Envelope.ofRect 4 1)
  assertApproxEq (a ∪ b)[Vec2.east] (b ∪ a)[Vec2.east] "union commutative east"
  assertApproxEq (a ∪ b)[Vec2.north] (b ∪ a)[Vec2.north] "union commutative north"

-- ══════════════════════════════════════════════════════════════════
-- Envelope.translateBy
-- ══════════════════════════════════════════════════════════════════

def testTranslateBy_east : IO Unit := do
  let env := Envelope.translateBy ⟨5, 0⟩ (Envelope.ofRect 1 1)
  assertApproxEq env[Vec2.east] 6 "translateBy east extent"

def testTranslateBy_west : IO Unit := do
  let env := Envelope.translateBy ⟨5, 0⟩ (Envelope.ofRect 1 1)
  assertApproxEq env[Vec2.west] (-4) "translateBy west extent"

def testTranslateBy_zero : IO Unit := do
  let env := Envelope.translateBy Vec2.zero (Envelope.ofRect 3 2)
  assertApproxEq env[Vec2.east] 3 "translateBy zero unchanged"

def testTranslateBy_bothAxes : IO Unit := do
  let env := Envelope.translateBy ⟨3, 4⟩ (Envelope.ofRect 1 1)
  assertApproxEq env[Vec2.east] 4 "translateBy both east"
  assertApproxEq env[Vec2.north] 5 "translateBy both north"

def testTranslateBy_negative : IO Unit := do
  let env := Envelope.translateBy ⟨-10, 0⟩ (Envelope.ofRect 1 1)
  assertApproxEq env[Vec2.east] (-9) "translateBy negative east"
  assertApproxEq env[Vec2.west] 11 "translateBy negative west"

-- ══════════════════════════════════════════════════════════════════
-- CardinalAnchors.fromEnvelope
-- ══════════════════════════════════════════════════════════════════

def testAnchors_symmetric : IO Unit := do
  let some anchors := CardinalAnchors.fromEnvelope (Envelope.ofRect 3 2)
    | throw <| .userError "No cardinal anchors for empty envelope"
  assertVec2Eq anchors.north ⟨0, 2⟩ "north"
  assertVec2Eq anchors.south ⟨0, -2⟩ "south"
  assertVec2Eq anchors.east ⟨3, 0⟩ "east"
  assertVec2Eq anchors.west ⟨-3, 0⟩ "west"

def testAnchors_center : IO Unit := do
  let some anchors := CardinalAnchors.fromEnvelope (Envelope.ofRect 3 2)
    | throw <| .userError "No cardinal anchors for empty envelope"
  assertVec2Eq anchors.center ⟨0, 0⟩ "center of symmetric rect"

def testAnchors_translated : IO Unit := do
  let some anchors := CardinalAnchors.fromEnvelope (Envelope.translateBy ⟨5, 3⟩ (Envelope.ofRect 1 1))
    | throw <| .userError "No cardinal anchors for empty envelope"
  assertVec2Eq anchors.center ⟨5, 3⟩ "translated center"

def testAnchors_circle : IO Unit := do
  let some anchors := CardinalAnchors.fromEnvelope (Envelope.ofCircle 4)
    | throw <| .userError "No cardinal anchors for empty envelope"
  assertVec2Eq anchors.north ⟨0, 4⟩ "circle north"
  assertVec2Eq anchors.east ⟨4, 0⟩ "circle east"
  assertVec2Eq anchors.center Vec2.zero "circle center"

def testAnchors_square : IO Unit := do
  let some anchors := CardinalAnchors.fromEnvelope (Envelope.ofRect 5 5)
    | throw <| .userError "No cardinal anchors for empty envelope"
  assertVec2Eq anchors.north ⟨0, 5⟩ "square north"
  assertVec2Eq anchors.east ⟨5, 0⟩ "square east"

-- ══════════════════════════════════════════════════════════════════
-- Layer 2: PathData constructors
-- ══════════════════════════════════════════════════════════════════

def testPathData_empty : IO Unit := do
  assertTrue (PathData.empty.commands.size == 0) "empty path has no commands"

def testPathData_line : IO Unit := do
  let pd := PathData.line ⟨0, 0⟩ ⟨1, 1⟩
  assertTrue (pd.commands.size == 2) "line has 2 commands"
  assertTrue (pd.commands[0]! == .moveTo ⟨0, 0⟩) "line starts with moveTo"
  assertTrue (pd.commands[1]! == .lineTo ⟨1, 1⟩) "line ends with lineTo"

def testPathData_rect : IO Unit := do
  let pd := PathData.rect 4 2
  assertTrue (pd.commands.size == 5) "rect has 5 commands (4 sides + close)"
  assertTrue (pd.commands[4]! == .closePath) "rect ends with closePath"

def testPathData_circle : IO Unit := do
  let pd := PathData.circle 5
  assertTrue (pd.commands.size == 6) "circle has 6 commands (moveTo + 4 curves + close)"
  assertTrue (pd.commands[0]! == .moveTo ⟨5, 0⟩) "circle starts at (r,0)"
  assertTrue (pd.commands[5]! == .closePath) "circle ends with closePath"

def testPathData_builder : IO Unit := do
  let pd := PathData.empty |>.moveTo ⟨0, 0⟩ |>.lineTo ⟨1, 0⟩ |>.lineTo ⟨1, 1⟩ |>.close
  assertTrue (pd.commands.size == 4) "builder chain has 4 commands"
  assertTrue (pd.commands[0]! == .moveTo ⟨0, 0⟩) "builder starts with moveTo"
  assertTrue (pd.commands[3]! == .closePath) "builder ends with close"

def geometryTests : List (String × IO Unit) := [
    -- Vec2.dot (5)
    ("Vec2.dot/perpendicular", testDot_perpendicular),
    ("Vec2.dot/parallel", testDot_parallel),
    ("Vec2.dot/antiparallel", testDot_antiparallel),
    ("Vec2.dot/commutative", testDot_commutative),
    ("Vec2.dot/general", testDot_general),
    -- Vec2.length (5)
    ("Vec2.length/345", testLength_345),
    ("Vec2.length/unit", testLength_unit),
    ("Vec2.length/zero", testLength_zero),
    ("Vec2.length/negative", testLength_negative),
    ("Vec2.length/diagonal", testLength_diagonal),
    -- Vec2.normalize (5)
    ("Vec2.normalize/unit", testNormalize_unit),
    ("Vec2.normalize/direction", testNormalize_direction),
    ("Vec2.normalize/alreadyUnit", testNormalize_alreadyUnit),
    ("Vec2.normalize/zero", testNormalize_zero),
    ("Vec2.normalize/large", testNormalize_large),
    -- Vec2.rotate (5)
    ("Vec2.rotate/90", testRotate_90),
    ("Vec2.rotate/180", testRotate_180),
    ("Vec2.rotate/360", testRotate_360),
    ("Vec2.rotate/negative", testRotate_negative),
    ("Vec2.rotate/preservesLength", testRotate_preservesLength),
    -- Vec2 arithmetic (5)
    ("Vec2.arith/addZero", testArith_addZero),
    ("Vec2.arith/addCommutative", testArith_addCommutative),
    ("Vec2.arith/subSelf", testArith_subSelf),
    ("Vec2.arith/negNeg", testArith_negNeg),
    ("Vec2.arith/scaleZero", testArith_scaleZero),
    -- Matrix.identity (5)
    ("Matrix.identity/apply", testIdentity_apply),
    ("Matrix.identity/origin", testIdentity_applyOrigin),
    ("Matrix.identity/linear", testIdentity_applyLinear),
    ("Matrix.identity/det", testIdentity_det),
    ("Matrix.identity/inverse", testIdentity_inverse),
    -- Matrix.translate (5)
    ("Matrix.translate/basic", testTranslate_basic),
    ("Matrix.translate/origin", testTranslate_origin),
    ("Matrix.translate/compose", testTranslate_compose),
    ("Matrix.translate/inverse", testTranslate_inverse),
    ("Matrix.translate/linear", testTranslate_linearPart),
    -- Matrix.scale (5)
    ("Matrix.scale/basic", testScale_basic),
    ("Matrix.scale/uniform", testScale_uniform),
    ("Matrix.scale/origin", testScale_origin),
    ("Matrix.scale/compose", testScale_compose),
    ("Matrix.scale/det", testScale_det),
    -- Matrix.rotate (5)
    ("Matrix.rotate/90", testMRotate_90),
    ("Matrix.rotate/180", testMRotate_180),
    ("Matrix.rotate/composeTwice", testMRotate_composeTwice),
    ("Matrix.rotate/det", testMRotate_det),
    ("Matrix.rotate/preservesLength", testMRotate_preservesLength),
    -- Matrix.shear (5)
    ("Matrix.shear/x", testShear_xOnly),
    ("Matrix.shear/y", testShear_yOnly),
    ("Matrix.shear/identity", testShear_identity),
    ("Matrix.shear/det", testShear_det2),
    ("Matrix.shear/invertible", testShear_invertible),
    -- Matrix.mul (5)
    ("Matrix.mul/scaleThenTranslate", testMul_scaleThenTranslate),
    ("Matrix.mul/translateThenScale", testMul_translateThenScale),
    ("Matrix.mul/associative", testMul_associative),
    ("Matrix.mul/identityLeft", testMul_identityLeft),
    ("Matrix.mul/identityRight", testMul_identityRight),
    -- Matrix.inverse (5)
    ("Matrix.inverse/roundTrip", testInverse_roundTrip),
    ("Matrix.inverse/roundTrip2", testInverse_roundTrip2),
    ("Matrix.inverse/singular", testInverse_singular),
    ("Matrix.inverse/identity", testInverse_identity),
    ("Matrix.inverse/translation", testInverse_translation),
    -- Matrix.apply (5)
    ("Matrix.apply/origin", testApply_origin),
    ("Matrix.apply/linearVsAffine", testApply_linearVsAffine),
    ("Matrix.apply/directionInvariant", testApply_directionInvariant),
    ("Matrix.apply/scaleDirection", testApply_scaleDirection),
    ("Matrix.apply/compositeChain", testApply_compositeChain),
    -- Envelope.empty (5)
    ("Envelope.empty/east", testEmpty_east),
    ("Envelope.empty/north", testEmpty_north),
    ("Envelope.empty/diagonal", testEmpty_diagonal),
    ("Envelope.empty/arbitrary", testEmpty_arbitrary),
    ("Envelope.empty/unionIdentity", testEmpty_unionIdentity),
    -- Envelope.ofRect (5)
    ("Envelope.ofRect/cardinals", testOfRect_cardinals),
    ("Envelope.ofRect/diagonal", testOfRect_diagonal),
    ("Envelope.ofRect/square", testOfRect_square),
    ("Envelope.ofRect/zero", testOfRect_zero),
    ("Envelope.ofRect/viaSize", testOfRect_viaSize),
    -- Envelope.ofCircle (5)
    ("Envelope.ofCircle/cardinal", testCircle_cardinal),
    ("Envelope.ofCircle/diagonal", testCircle_diagonal),
    ("Envelope.ofCircle/arbitrary", testCircle_arbitrary),
    ("Envelope.ofCircle/zero", testCircle_zero),
    ("Envelope.ofCircle/symmetry", testCircle_symmetry),
    -- Envelope.transform (5)
    ("Envelope.transform/rotate90", testTransform_rotate90),
    ("Envelope.transform/uniformScale", testTransform_uniformScale),
    ("Envelope.transform/translate", testTransform_translate),
    ("Envelope.transform/identity", testTransform_identity),
    ("Envelope.transform/nonUniformScale", testTransform_nonUniformScale),
    -- Envelope.union (5)
    ("Envelope.union/disjointH", testUnion_disjointHorizontal),
    ("Envelope.union/disjointV", testUnion_disjointVertical),
    ("Envelope.union/identical", testUnion_identical),
    ("Envelope.union/contained", testUnion_contained),
    ("Envelope.union/commutative", testUnion_commutative),
    -- Envelope.translateBy (5)
    ("Envelope.translateBy/east", testTranslateBy_east),
    ("Envelope.translateBy/west", testTranslateBy_west),
    ("Envelope.translateBy/zero", testTranslateBy_zero),
    ("Envelope.translateBy/bothAxes", testTranslateBy_bothAxes),
    ("Envelope.translateBy/negative", testTranslateBy_negative),
    -- CardinalAnchors (5)
    ("CardinalAnchors/symmetric", testAnchors_symmetric),
    ("CardinalAnchors/center", testAnchors_center),
    ("CardinalAnchors/translated", testAnchors_translated),
    ("CardinalAnchors/circle", testAnchors_circle),
    ("CardinalAnchors/square", testAnchors_square),
    -- PathData constructors (5)
    ("PathData/empty", testPathData_empty),
    ("PathData/line", testPathData_line),
    ("PathData/rect", testPathData_rect),
    ("PathData/circle", testPathData_circle),
    ("PathData/builder", testPathData_builder)
  ]
