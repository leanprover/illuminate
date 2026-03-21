/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/

import Illuminate
import Tests.Helpers

open Illuminate

private def step (d : Float) : Step := { duration := d }

-- ═══════════════════════════════════════════════════════════════
-- Easing tests
-- ═══════════════════════════════════════════════════════════════

def easingTests : List (String × IO Unit) :=
  [ ("easing: linear 0→0, 1→1", do
      assertApproxEq (Easing.linear 0) 0 "linear(0)"
      assertApproxEq (Easing.linear 1) 1 "linear(1)"
      assertApproxEq (Easing.linear 0.5) 0.5 "linear(0.5)")
  , ("easing: easeIn 0→0, 1→1", do
      assertApproxEq (Easing.easeIn 0) 0 "easeIn(0)"
      assertApproxEq (Easing.easeIn 1) 1 "easeIn(1)"
      assertApproxEq (Easing.easeIn 0.5) 0.25 "easeIn(0.5)")
  , ("easing: easeOut 0→0, 1→1", do
      assertApproxEq (Easing.easeOut 0) 0 "easeOut(0)"
      assertApproxEq (Easing.easeOut 1) 1 "easeOut(1)"
      assertApproxEq (Easing.easeOut 0.5) 0.75 "easeOut(0.5)")
  , ("easing: easeInOut 0→0, 1→1", do
      assertApproxEq (Easing.easeInOut 0) 0 "easeInOut(0)"
      assertApproxEq (Easing.easeInOut 1) 1 "easeInOut(1)"
      assertApproxEq (Easing.easeInOut 0.5) 0.5 "easeInOut(0.5)")
  , ("easing: backOut 0→0, 1→1", do
      assertApproxEq (Easing.backOut 0) 0 "backOut(0)" (tol := 1e-6)
      assertApproxEq (Easing.backOut 1) 1 "backOut(1)" (tol := 1e-6))
  ]

-- ═══════════════════════════════════════════════════════════════
-- Interpolation tests
-- ═══════════════════════════════════════════════════════════════

def interpolationTests : List (String × IO Unit) :=
  [ ("interpolate: Float", do
      assertApproxEq (Interpolate.interpolate 0 10 0) 0 "float t=0"
      assertApproxEq (Interpolate.interpolate 0 10 1) 10 "float t=1"
      assertApproxEq (Interpolate.interpolate 0 10 0.5) 5 "float t=0.5"
      assertApproxEq (Interpolate.interpolate 20 40 0.25) 25 "float 20-40 t=0.25")
  , ("interpolate: Vec2", do
      let a : Vec2 := ⟨0, 0⟩
      let b : Vec2 := ⟨10, 20⟩
      let mid := Interpolate.interpolate a b 0.5
      assertVec2Eq mid ⟨5, 10⟩ "vec2 midpoint"
      let start := Interpolate.interpolate a b 0
      assertVec2Eq start a "vec2 t=0"
      let finish := Interpolate.interpolate a b 1
      assertVec2Eq finish b "vec2 t=1")
  , ("interpolate: Color", do
      let a := Color.black
      let b := Color.white
      let mid := Interpolate.interpolate a b 0.5
      assertTrue (mid.r > 120 && mid.r < 135) "color mid r"
      assertTrue (mid.g > 120 && mid.g < 135) "color mid g"
      assertTrue (mid.b > 120 && mid.b < 135) "color mid b"
      let start := Interpolate.interpolate a b 0
      assertTrue (start.r == 0) "color t=0 r"
      let finish := Interpolate.interpolate a b 1
      assertTrue (finish.r == 255) "color t=1 r")
  , ("interpolate: Matrix", do
      let a := Matrix.identity
      let b := Matrix.translate 10 20
      let mid := Interpolate.interpolate a b 0.5
      assertApproxEq mid.tx 5 "matrix mid tx"
      assertApproxEq mid.ty 10 "matrix mid ty"
      assertApproxEq mid.a 1 "matrix mid a"
      assertApproxEq mid.d 1 "matrix mid d")
  ]

-- ═══════════════════════════════════════════════════════════════
-- Timeline tests
-- ═══════════════════════════════════════════════════════════════

def timelineTests : List (String × IO Unit) :=
  [ ("totalDuration: basic", do
      assertApproxEq (totalDuration [step 1.0, step 2.0, step 0.5]) 3.5 "3 steps")
  , ("totalDuration: zero and negative", do
      assertApproxEq (totalDuration [step 1.0, step 0, step (-1.0), step 2.0]) 3.0
        "zero and negative skipped")
  , ("totalDuration: empty", do
      assertApproxEq (totalDuration []) 0 "empty")
  , ("progressAt: before start", do
      let p := progressAt [step 1.0, step 1.0] 0
      assertApproxEq (p[0]?.getD 0) 0 "step 0 at t=0"
      assertApproxEq (p[1]?.getD 0) 0 "step 1 at t=0")
  , ("progressAt: midway through step 1", do
      let p := progressAt [step 2.0, step 1.0] 1.0
      assertApproxEq (p[0]?.getD 0) 0.5 "step 0 at t=1"
      assertApproxEq (p[1]?.getD 0) 0 "step 1 at t=1")
  , ("progressAt: at step boundary", do
      let p := progressAt [step 1.0, step 1.0] 1.0
      assertApproxEq (p[0]?.getD 0) 1.0 "step 0 at t=1"
      assertApproxEq (p[1]?.getD 0) 0 "step 1 at t=1")
  , ("progressAt: all done", do
      let p := progressAt [step 1.0, step 1.0] 2.0
      assertApproxEq (p[0]?.getD 0) 1.0 "step 0 at t=2"
      assertApproxEq (p[1]?.getD 0) 1.0 "step 1 at t=2")
  , ("progressAt: past end", do
      let p := progressAt [step 1.0] 5.0
      assertApproxEq (p[0]?.getD 0) 1.0 "step 0 clamped at 1")
  , ("progressAt: zero-duration step skipped", do
      let p := progressAt [step 0, step 1.0] 0.5
      assertApproxEq (p[0]?.getD 0) 1.0 "zero-dur step is 1.0"
      assertApproxEq (p[1]?.getD 0) 0.5 "next step progresses")
  , ("progressAt: looping step wraps", do
      let p := progressAt [{ duration := 1.0, loop := true }] 1.5
      assertApproxEq (p[0]?.getD 0) 0.5 "loop wraps at 1.5s")
  , ("progressVector: correct size", do
      let v := progressVector [0.5, 1.0] 4
      assertTrue (v.size == 4) "vector size is 4"
      assertApproxEq (v[0]) 0.5 "v[0]"
      assertApproxEq (v[1]) 1.0 "v[1]"
      assertApproxEq (v[2]) 0.0 "v[2] padded"
      assertApproxEq (v[3]) 0.0 "v[3] padded")
  ]

-- ═══════════════════════════════════════════════════════════════
-- Effects tests
-- ═══════════════════════════════════════════════════════════════

def effectsTests : List (String × IO Unit) :=
  [ ("fadeIn: t=0 is transparent", do
      let d : Diagram Empty := Diagram.circle 10 (fill := .solid { color := Color.red })
      let faded := fadeIn d 0
      match faded with
      | .cellophane α _ => assertApproxEq α 0 "fadeIn t=0 opacity"
      | _ => throw <| IO.userError "fadeIn should produce cellophane")
  , ("fadeIn: t=1 is opaque", do
      let d : Diagram Empty := Diagram.circle 10 (fill := .solid { color := Color.red })
      let faded := fadeIn d 1
      match faded with
      | .cellophane α _ => assertApproxEq α 1 "fadeIn t=1 opacity"
      | _ => throw <| IO.userError "fadeIn should produce cellophane")
  , ("fadeOut: t=0 is opaque", do
      let d : Diagram Empty := Diagram.circle 10 (fill := .solid { color := Color.red })
      let faded := fadeOut d 0
      match faded with
      | .cellophane α _ => assertApproxEq α 1 "fadeOut t=0 opacity"
      | _ => throw <| IO.userError "fadeOut should produce cellophane")
  , ("fadeOut: t=1 is transparent", do
      let d : Diagram Empty := Diagram.circle 10 (fill := .solid { color := Color.red })
      let faded := fadeOut d 1
      match faded with
      | .cellophane α _ => assertApproxEq α 0 "fadeOut t=1 opacity"
      | _ => throw <| IO.userError "fadeOut should produce cellophane")
  , ("crossFade: produces compose of two cellophanes", do
      let a : Diagram Empty := Diagram.circle 10 (fill := .solid { color := Color.red })
      let b : Diagram Empty := Diagram.circle 10 (fill := .solid { color := Color.blue })
      let cf := crossFade a b 0.5
      match cf with
      | .compose (.cellophane α1 _) (.cellophane α2 _) =>
        assertApproxEq α1 0.5 "crossFade a opacity"
        assertApproxEq α2 0.5 "crossFade b opacity"
      | _ => throw <| IO.userError "crossFade should produce compose of cellophanes")
  , ("slide: t=0 at start position", do
      let d : Diagram Empty := Diagram.circle 10 (fill := .solid { color := Color.red })
      let slid := slide d ⟨100, 0⟩ ⟨200, 0⟩ 0
      match slid with
      | .transform m _ =>
        assertApproxEq m.tx 100 "slide t=0 tx"
        assertApproxEq m.ty 0 "slide t=0 ty"
      | _ => throw <| IO.userError "slide should produce transform")
  , ("slide: t=1 at end position", do
      let d : Diagram Empty := Diagram.circle 10 (fill := .solid { color := Color.red })
      let slid := slide d ⟨100, 0⟩ ⟨200, 0⟩ 1
      match slid with
      | .transform m _ =>
        assertApproxEq m.tx 200 "slide t=1 tx"
        assertApproxEq m.ty 0 "slide t=1 ty"
      | _ => throw <| IO.userError "slide should produce transform")
  , ("animScale: t=0.5 midpoint", do
      let d : Diagram Empty := Diagram.circle 10 (fill := .solid { color := Color.red })
      let scaled := animScale d 1.0 3.0 0.5
      match scaled with
      | .transform m _ =>
        assertApproxEq m.a 2.0 "animScale t=0.5 sx"
        assertApproxEq m.d 2.0 "animScale t=0.5 sy"
      | _ => throw <| IO.userError "animScale should produce transform")
  , ("animRotate: t=0 no rotation", do
      let d : Diagram Empty := Diagram.circle 10 (fill := .solid { color := Color.red })
      let rotated := animRotate d 0 pi 0
      match rotated with
      | .transform m _ =>
        assertApproxEq m.a 1.0 "animRotate t=0 cos"
        assertApproxEq m.c 0.0 "animRotate t=0 sin" (tol := 1e-6)
      | _ => throw <| IO.userError "animRotate should produce transform")
  ]

-- ═══════════════════════════════════════════════════════════════
-- Compilation tests
-- ═══════════════════════════════════════════════════════════════

def compilationTests : List (String × IO Unit) :=
  [ ("compile: basic animation has correct frame count", do
      let steps : List Step := [step 1.0]
      let compiled := compileAnimation steps
        (fun progress => Diagram.circle (Interpolate.interpolate 10 50 progress[0])
          (fill := .solid { color := Color.red }))
        (fps := 10)
      assertTrue (compiled.totalFrames == 10) s!"expected 10 frames, got {compiled.totalFrames}"
      assertTrue (compiled.fps == 10) "fps preserved")
  , ("compile: segments are present", do
      let steps : List Step := [step 0.5, step 0.5]
      let compiled := compileAnimation steps
        (fun progress => Diagram.circle (Interpolate.interpolate 10 50 progress[0])
          (fill := .solid { color := Color.red }))
        (fps := 10)
      assertTrue (compiled.segments.size > 0) "has segments"
      let totalCovered := compiled.segments.foldl (fun acc s => acc + s.frameCount) 0
      assertTrue (totalCovered == compiled.totalFrames)
        s!"segments cover all frames: {totalCovered} vs {compiled.totalFrames}")
  , ("compile: step boundaries present", do
      let steps : List Step := [step 1.0, step 1.0]
      let compiled := compileAnimation steps
        (fun _ => Diagram.circle 20 (fill := .solid { color := Color.red }))
        (fps := 10)
      assertTrue (compiled.steps.size == 2) s!"expected 2 steps, got {compiled.steps.size}")
  , ("compile: sync frames are valid SVG", do
      let steps : List Step := [step 0.5]
      let compiled := compileAnimation steps
        (fun _ => Diagram.circle 20 (fill := .solid { color := Color.red }))
        (fps := 10)
      for seg in compiled.segments do
        assertContains seg.syncFrame "<svg" "sync frame is SVG")
  , ("compile: pause steps tracked", do
      let steps : List Step := [step 0.5, { duration := 0, pause := true }, step 0.5]
      let compiled := compileAnimation steps
        (fun _ => Diagram.circle 20 (fill := .solid { color := Color.red }))
        (fps := 10)
      let pauseSteps := compiled.steps.filter (·.pause)
      assertTrue (pauseSteps.size == 1) s!"expected 1 pause step, got {pauseSteps.size}")
  ]

-- ═══════════════════════════════════════════════════════════════
-- All animation tests
-- ═══════════════════════════════════════════════════════════════

def animationTests : List (String × IO Unit) :=
  easingTests ++ interpolationTests ++ timelineTests ++ effectsTests ++ compilationTests
