/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/

import Illuminate.Animation.Morph.CubicSeg
import Illuminate.Geometry.PathData
import Tests.Helpers

open Illuminate

/-!
# CubicSeg unit tests
-/

def cubicSegTests : List (String × IO Unit) :=
  [ ("lineToCubic: endpoints match", do
      let seg := lineToCubic ⟨0, 0⟩ ⟨6, 9⟩
      assertVec2Eq seg.p0 ⟨0, 0⟩ "p0"
      assertVec2Eq seg.p3 ⟨6, 9⟩ "p3"
      -- Control points at 1/3 and 2/3
      assertVec2Eq seg.c1 ⟨2, 3⟩ "c1"
      assertVec2Eq seg.c2 ⟨4, 6⟩ "c2")
  , ("splitAt: midpoint continuity", do
      let seg : CubicSeg := { p0 := ⟨0, 0⟩, c1 := ⟨1, 2⟩, c2 := ⟨3, 2⟩, p3 := ⟨4, 0⟩ }
      let (left, right) := seg.splitAt 0.5
      -- Left end matches original start
      assertVec2Eq left.p0 seg.p0 "left.p0"
      -- Right end matches original end
      assertVec2Eq right.p3 seg.p3 "right.p3"
      -- Junction point matches
      assertVec2Eq left.p3 right.p0 "junction" (tol := 1e-6))
  , ("splitAt: t=0 preserves original", do
      let seg : CubicSeg := { p0 := ⟨0, 0⟩, c1 := ⟨1, 2⟩, c2 := ⟨3, 2⟩, p3 := ⟨4, 0⟩ }
      let (left, _) := seg.splitAt 0.0
      assertVec2Eq left.p0 seg.p0 "p0"
      assertVec2Eq left.p3 seg.p0 "p3 = p0 at t=0" (tol := 1e-6))
  , ("splitAt: t=1 preserves original", do
      let seg : CubicSeg := { p0 := ⟨0, 0⟩, c1 := ⟨1, 2⟩, c2 := ⟨3, 2⟩, p3 := ⟨4, 0⟩ }
      let (_, right) := seg.splitAt 1.0
      assertVec2Eq right.p0 seg.p3 "p0 = p3 at t=1" (tol := 1e-6)
      assertVec2Eq right.p3 seg.p3 "p3")
  , ("pathToCubics: rectangle has 4 segments", do
      let pd := PathData.rect 10 8
      let np := pathToCubics pd
      assertTrue (np.subpaths.size == 1) s!"expected 1 subpath, got {np.subpaths.size}"
      let sub := np.subpaths[0]?.getD default
      assertTrue sub.closed "rect should be closed"
      -- 4 sides: 3 lineTos + 1 closePath line = 4 segments
      assertTrue (sub.segments.size == 4)
        s!"expected 4 segments, got {sub.segments.size}")
  , ("pathToCubics: circle has segments", do
      let pd := PathData.circle 20
      let np := pathToCubics pd
      assertTrue (np.subpaths.size == 1) s!"expected 1 subpath, got {np.subpaths.size}"
      let sub := np.subpaths[0]?.getD default
      assertTrue sub.closed "circle should be closed"
      -- 2 semicircular arcs, each split into at most 2 quarter-arcs = up to 4 segments
      assertTrue (sub.segments.size >= 2 && sub.segments.size <= 8)
        s!"expected 2-8 segments, got {sub.segments.size}")
  , ("pathToCubics: circle endpoints close", do
      let pd := PathData.circle 20
      let np := pathToCubics pd
      let sub := np.subpaths[0]?.getD default
      let first := (sub.segments[0]?.getD default).p0
      let last := (sub.segments[sub.segments.size - 1]?.getD default).p3
      assertVec2Eq first last "circle start/end" (tol := 0.1))
  , ("equalizeCubics: result has equal length", do
      let a := #[lineToCubic ⟨0, 0⟩ ⟨10, 0⟩, lineToCubic ⟨10, 0⟩ ⟨10, 10⟩]
      let b := #[lineToCubic ⟨0, 0⟩ ⟨5, 0⟩, lineToCubic ⟨5, 0⟩ ⟨10, 0⟩,
                  lineToCubic ⟨10, 0⟩ ⟨10, 5⟩, lineToCubic ⟨10, 5⟩ ⟨10, 10⟩]
      let (ea, eb) := equalizeCubics a b
      assertTrue (ea.size == eb.size)
        s!"expected equal sizes, got {ea.size} and {eb.size}"
      assertTrue (ea.size == 4) s!"expected 4, got {ea.size}")
  , ("equalizeCubics: same size unchanged", do
      let a := #[lineToCubic ⟨0, 0⟩ ⟨10, 0⟩]
      let b := #[lineToCubic ⟨0, 0⟩ ⟨5, 5⟩]
      let (ea, eb) := equalizeCubics a b
      assertTrue (ea.size == 1 && eb.size == 1)
        s!"expected both size 1, got {ea.size} and {eb.size}")
  , ("alignRotation: identity for identical arrays", do
      let segs := #[lineToCubic ⟨0, 0⟩ ⟨10, 0⟩, lineToCubic ⟨10, 0⟩ ⟨10, 10⟩]
      let offset := alignRotation segs segs
      assertTrue (offset == 0) s!"expected offset 0, got {offset}")
  , ("cubicsToPathData: round-trip has correct command count", do
      let segs := #[lineToCubic ⟨0, 0⟩ ⟨10, 0⟩, lineToCubic ⟨10, 0⟩ ⟨10, 10⟩]
      let pd := cubicsToPathData segs true
      -- moveTo + 2 curveTo + closePath = 4 commands
      assertTrue (pd.commands.size == 4)
        s!"expected 4 commands, got {pd.commands.size}")
  , ("arcToCubics: semicircle has segments", do
      let segs := arcToCubics ⟨20, 0⟩ 20 20 0 false true ⟨-20, 0⟩
      assertTrue (segs.size >= 1 && segs.size <= 4)
        s!"expected 1-4 segments for semicircle, got {segs.size}"
      -- Endpoints should match
      assertVec2Eq (segs[0]?.getD default).p0 ⟨20, 0⟩ "start" (tol := 0.1)
      assertVec2Eq (segs[segs.size - 1]?.getD default).p3 ⟨-20, 0⟩ "end" (tol := 0.1))
  , ("prepareSegments: rect vs circle equalizes", do
      let rectNp := pathToCubics (PathData.rect 40 40)
      let circNp := pathToCubics (PathData.circle 20)
      let rectSegs := (rectNp.subpaths[0]?.getD default).segments
      let circSegs := (circNp.subpaths[0]?.getD default).segments
      let (ea, eb) := prepareSegments rectSegs circSegs true
      assertTrue (ea.size == eb.size)
        s!"expected equal sizes, got {ea.size} and {eb.size}")
  , ("CubicSeg.lerp: t=0 gives first", do
      let a : CubicSeg := { p0 := ⟨0, 0⟩, c1 := ⟨1, 0⟩, c2 := ⟨2, 0⟩, p3 := ⟨3, 0⟩ }
      let b : CubicSeg := { p0 := ⟨10, 10⟩, c1 := ⟨11, 10⟩, c2 := ⟨12, 10⟩, p3 := ⟨13, 10⟩ }
      let r := CubicSeg.lerp a b 0
      assertVec2Eq r.p0 a.p0 "p0"
      assertVec2Eq r.p3 a.p3 "p3")
  , ("CubicSeg.lerp: t=1 gives second", do
      let a : CubicSeg := { p0 := ⟨0, 0⟩, c1 := ⟨1, 0⟩, c2 := ⟨2, 0⟩, p3 := ⟨3, 0⟩ }
      let b : CubicSeg := { p0 := ⟨10, 10⟩, c1 := ⟨11, 10⟩, c2 := ⟨12, 10⟩, p3 := ⟨13, 10⟩ }
      let r := CubicSeg.lerp a b 1
      assertVec2Eq r.p0 b.p0 "p0"
      assertVec2Eq r.p3 b.p3 "p3")
  ]

/-!
# All morph tests
-/

def morphTests : List (String × IO Unit) :=
  cubicSegTests
