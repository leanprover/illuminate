/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/
module
import Illuminate.Animation.Morph.CubicSeg
import Illuminate.Animation.Morph.Style
import Illuminate.Animation.Morph.Prepare
import Illuminate.Geometry.PathData
import Tests.Helpers
public section

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
      assertTrue (ea.size >= 4) s!"expected ≥4, got {ea.size}")
  , ("equalizeCubics: both resampled to equal count", do
      let a := #[lineToCubic ⟨0, 0⟩ ⟨10, 0⟩]
      let b := #[lineToCubic ⟨0, 0⟩ ⟨5, 5⟩]
      let (ea, eb) := equalizeCubics a b
      assertTrue (ea.size == eb.size)
        s!"expected equal sizes, got {ea.size} and {eb.size}")
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
# Style interpolation tests
-/

open Illuminate.Morph in
def styleTests' : List (String × IO Unit) :=
  [ ("normalizeStops: merges offsets", do
      let a : Array GradientStop := #[⟨0, Color.black⟩, ⟨1, Color.white⟩]
      let b : Array GradientStop := #[⟨0, Color.red⟩, ⟨0.5, Color.green⟩, ⟨1, Color.blue⟩]
      let (na, nb) := normalizeStops a b
      assertTrue (na.size == nb.size) s!"expected equal, got {na.size} and {nb.size}"
      assertTrue (na.size == 3) s!"expected 3 stops, got {na.size}")
  , ("normalizeStops: identical stays same", do
      let stops : Array GradientStop := #[⟨0, Color.black⟩, ⟨1, Color.white⟩]
      let (na, nb) := normalizeStops stops stops
      assertTrue (na.size == 2 && nb.size == 2) "should stay size 2")
  , ("lerpStroke: t=0 gives first", do
      let a : Stroke := { color := Color.red, width := 2 }
      let b : Stroke := { color := Color.blue, width := 4 }
      let r := lerpStroke a b 0
      assertApproxEq r.width 2 "width"
      assertTrue (r.color.r == 255) s!"red channel: {r.color.r}")
  , ("lerpStroke: t=1 gives second", do
      let a : Stroke := { color := Color.red, width := 2 }
      let b : Stroke := { color := Color.blue, width := 4 }
      let r := lerpStroke a b 1
      assertApproxEq r.width 4 "width"
      assertTrue (r.color.b == 255) s!"blue channel: {r.color.b}")
  , ("lerpStroke: t=0.5 midpoint", do
      let a : Stroke := { color := Color.black, width := 0 }
      let b : Stroke := { color := Color.white, width := 10 }
      let r := lerpStroke a b 0.5
      assertApproxEq r.width 5 "width")
  , ("lerpTextStyle: fontSize lerps", do
      let a : TextStyle := { fontSize := 10 }
      let b : TextStyle := { fontSize := 30 }
      let r := lerpTextStyle a b 0.5
      assertApproxEq r.fontSize 20 "fontSize")
  , ("lerpTextStyle: fontFamily switches at 0.5", do
      let a : TextStyle := { fontFamily := "sans-serif" }
      let b : TextStyle := { fontFamily := "monospace" }
      let r1 := lerpTextStyle a b 0.3
      let r2 := lerpTextStyle a b 0.7
      assertTrue (r1.fontFamily == "sans-serif") "before 0.5"
      assertTrue (r2.fontFamily == "monospace") "after 0.5")
  , ("prepareFills: solid/solid returns some", do
      let a : ResolvedFill := .solid { color := Color.red }
      let b : ResolvedFill := .solid { color := Color.blue }
      assertTrue (prepareFills a b).isSome "solid/solid")
  , ("prepareFills: linear/linear normalizes stops", do
      let a : ResolvedFill := .gradient (.linear 0 0 10 0 #[⟨0, Color.red⟩, ⟨1, Color.blue⟩])
      let b : ResolvedFill := .gradient (.linear 0 0 10 0 #[⟨0, Color.red⟩, ⟨0.5, Color.green⟩, ⟨1, Color.blue⟩])
      match prepareFills a b with
      | some (.gradient (.linear _ _ _ _ sa _), .gradient (.linear _ _ _ _ sb _)) =>
        assertTrue (sa.size == sb.size) s!"expected equal stops, got {sa.size} and {sb.size}"
      | _ => throw <| IO.userError "expected linear/linear")
  , ("prepareFills: linear/radial returns none", do
      let a : ResolvedFill := .gradient (.linear 0 0 10 0 #[⟨0, Color.red⟩, ⟨1, Color.blue⟩])
      let b : ResolvedFill := .gradient (.radial 0 0 10 0 0 0 #[⟨0, Color.red⟩, ⟨1, Color.blue⟩])
      assertTrue (prepareFills a b).isNone "linear/radial should be none")
  , ("lerpResolvedFill: solid midpoint", do
      let a : ResolvedFill := .solid { color := Color.black }
      let b : ResolvedFill := .solid { color := Color.white }
      match lerpResolvedFill a b 0.5 with
      | some (.solid fs) =>
        assertTrue (fs.color.r >= 127 && fs.color.r <= 128) s!"mid gray r={fs.color.r}"
      | _ => throw <| IO.userError "expected solid")
  ]

/-!
# Morph plan structure tests
-/

private def countCrossFades {β : Type} : MorphNode β → Nat
  | .crossFade _ _ => 1
  | .compose l r => countCrossFades l + countCrossFades r
  | .transform _ _ c => countCrossFades c
  | .cellophane _ _ c => countCrossFades c
  | .clip _ _ _ c => countCrossFades c
  | .matched _ _ _ c => countCrossFades c
  | .arrow _ _ _ _ _ _ _ _ c => countCrossFades c
  | _ => 0

private def countMatched {β : Type} : MorphNode β → Nat
  | .matched _ _ _ c => 1 + countMatched c
  | .compose l r => countMatched l + countMatched r
  | .transform _ _ c => countMatched c
  | .cellophane _ _ c => countMatched c
  | .clip _ _ _ c => countMatched c
  | .arrow _ _ _ _ _ _ _ _ c => countMatched c
  | _ => 0

private def countPaths {β : Type} : MorphNode β → Nat
  | .path .. => 1
  | .compose l r => countPaths l + countPaths r
  | .transform _ _ c => countPaths c
  | .cellophane _ _ c => countPaths c
  | .clip _ _ _ c => countPaths c
  | .matched _ _ _ c => countPaths c
  | .arrow _ _ _ _ _ _ _ _ c => countPaths c
  | _ => 0

private def countArrows {β : Type} : MorphNode β → Nat
  | .arrow _ _ _ _ _ _ _ _ c => 1 + countArrows c
  | .compose l r => countArrows l + countArrows r
  | .transform _ _ c => countArrows c
  | .cellophane _ _ c => countArrows c
  | .clip _ _ _ c => countArrows c
  | .matched _ _ _ c => countArrows c
  | _ => 0

def morphPlanTests : List (String × IO Unit) :=
  [ ("morph plan: named shapes are matched not cross-faded", do
      let tri := Diagram.fromPath
        (PathData.empty |>.moveTo ⟨0, 12⟩ |>.lineTo ⟨-10, -6⟩ |>.lineTo ⟨10, -6⟩ |>.close)
        (fill := Color.red) (stroke := { color := .black, width := 1 })
      let sq := Diagram.rect 18 18 (fill := Color.green) (stroke := { color := .black, width := 1 })
      let start : Diagram SVG := Diagram.hsep 40 [tri.named `A, sq.named `B]
      let stop : Diagram SVG := Diagram.hsep 40 [sq.named `B, tri.named `A]
      let m := start.morph stop
      let matched := countMatched m.node
      let crossFades := countCrossFades m.node
      IO.println s!"    matched={matched} crossFades={crossFades} paths={countPaths m.node}"
      assertTrue (matched >= 2) s!"expected ≥2 matched, got {matched}"
      assertTrue (crossFades == 0) s!"expected 0 cross-fades, got {crossFades}")
  , ("morph plan: nested named shapes with arrows", do
      let tri := Diagram.fromPath
        (PathData.empty |>.moveTo ⟨0, 12⟩ |>.lineTo ⟨-10, -6⟩ |>.lineTo ⟨10, -6⟩ |>.close)
        (fill := Color.red) (stroke := { color := .black, width := 1 })
      let sq := Diagram.rect 18 18 (fill := Color.green) (stroke := { color := .black, width := 1 })
      let rr := Diagram.roundedRect 22 18 4 (fill := Color.red) (stroke := { color := .black, width := 1 })
      let aStart := Diagram.hsep 30 [tri.named `X, sq.named `Y]
        |>.connect `X { point := `Y, arrowhead := some {} } |>.named `A
      let aEnd := Diagram.hsep 30 [sq.named `Y, rr.named `X]
        |>.connect `Y { point := `X, arrowhead := some {} } |>.named `A
      let bStart := (Diagram.circle 15 (fill := Color.blue) (stroke := { color := .black, width := 1 })).named `B
      let bEnd := (Diagram.star 4 18 8 (fill := Color.blue) (stroke := { color := .black, width := 1 })).named `B
      let start : Diagram SVG := Diagram.hsep 40 [aStart, bStart]
      let stop : Diagram SVG := Diagram.hsep 40 [bEnd, aEnd]
      let m := start.morph stop
      let matched := countMatched m.node
      let crossFades := countCrossFades m.node
      let paths := countPaths m.node
      let arrows := countArrows m.node
      IO.println s!"    matched={matched} crossFades={crossFades} paths={paths} arrows={arrows}"
      assertTrue (arrows >= 1) s!"expected ≥1 arrow morph, got {arrows}"
      assertTrue (crossFades == 0) s!"expected 0 cross-fades, got {crossFades}")
  ]

/-!
# Skeleton normalization tests
-/

private def rectPrim : Skeleton Empty :=
  .prim (.path (PathData.rect 20 10) (.solid Color.red) {})

def normalizationTests : List (String × IO Unit) :=
  [ ("normalize: collapse consecutive transforms", do
      let m1 := Matrix.rotate 0.5
      let m2 := Matrix.rotate 0.3
      let skel : Skeleton Empty := .transform m1 (.transform m2 rectPrim)
      let norm := normalizeSkeleton skel
      match norm with
      | .transform _ (.prim _) => pure ()
      | _ => throw <| IO.userError s!"expected single transform around prim")
  , ("normalize: collapse consecutive cellophanes", do
      let skel : Skeleton Empty := .cellophane 0.5 (.cellophane 0.8 rectPrim)
      let norm := normalizeSkeleton skel
      match norm with
      | .cellophane α (.prim _) =>
        assertApproxEq α 0.4 "collapsed opacity" (tol := 1e-6)
      | _ => throw <| IO.userError "expected single cellophane around prim")
  , ("normalize: reorder transform/cellophane", do
      let m := Matrix.rotate 1.0
      let skel : Skeleton Empty := .transform m (.cellophane 0.5 rectPrim)
      let norm := normalizeSkeleton skel
      match norm with
      | .cellophane _ (.transform _ (.prim _)) => pure ()
      | _ => throw <| IO.userError "expected cellophane outside transform")
  , ("normalize: push transform past clip", do
      let m := Matrix.rotate 1.0
      let clipPd := PathData.rect 30 30
      let skel : Skeleton Empty := .transform m (.clip clipPd rectPrim)
      let norm := normalizeSkeleton skel
      match norm with
      | .clip _ (.transform _ (.prim _)) => pure ()
      | _ => throw <| IO.userError "expected clip outside transform")
  , ("normalize: push clip past cellophane", do
      let clipPd := PathData.rect 30 30
      let skel : Skeleton Empty := .clip clipPd (.cellophane 0.5 rectPrim)
      let norm := normalizeSkeleton skel
      match norm with
      | .cellophane _ (.clip _ (.prim _)) => pure ()
      | _ => throw <| IO.userError "expected cellophane outside clip")
  , ("normalize: complex nesting", do
      let m1 := Matrix.rotate 0.5
      let m2 := Matrix.rotate 0.3
      let clipPd := PathData.rect 30 30
      let skel : Skeleton Empty :=
        .transform m1 (.transform m2 (.clip clipPd (.cellophane 0.7 rectPrim)))
      let norm := normalizeSkeleton skel
      match norm with
      | .cellophane _ (.clip _ (.transform _ (.prim _))) => pure ()
      | _ => throw <| IO.userError "expected cellophane > clip > transform > prim")
  ]

/-!
# Morph plan integration tests for wrapper handling
-/

private def blueRect (w h : Float) : Diagram SVG :=
  Diagram.rect w h (fill := Color.blue) (stroke := { color := .black, width := 1 })

private def redRect (w h : Float) : Diagram SVG :=
  Diagram.rect w h (fill := Color.red) (stroke := { color := .black, width := 1 })

private def blueCircle (r : Float) : Diagram SVG :=
  Diagram.circle r (fill := Color.blue) (stroke := { color := .black, width := 1 })

private def redCircle (r : Float) : Diagram SVG :=
  Diagram.circle r (fill := Color.red) (stroke := { color := .black, width := 1 })

def morphWrapperTests : List (String × IO Unit) :=
  [ ("morph wrapper: rotated rect to circle", do
      let a := (blueRect 20 30).rotate 2
      let b := redCircle 40
      let m := a.morph b
      let crossFades := countCrossFades m.node
      let paths := countPaths m.node
      IO.println s!"    crossFades={crossFades} paths={paths}"
      assertTrue (crossFades == 0) s!"expected 0 cross-fades, got {crossFades}"
      assertTrue (paths >= 1) s!"expected ≥1 path morph, got {paths}")
  , ("morph wrapper: reordered transform+cellophane", do
      let a := ((blueRect 30 30).rotate 1).cellophane 0.5
      let b := ((redRect 30 30).cellophane 0.7).rotate 0.5
      let m := a.morph b
      let crossFades := countCrossFades m.node
      IO.println s!"    crossFades={crossFades}"
      assertTrue (crossFades == 0) s!"expected 0 cross-fades, got {crossFades}")
  , ("morph wrapper: stacked transforms", do
      let a := ((blueRect 30 30).rotate 1).rotate 0.5
      let b := redRect 40 40
      let m := a.morph b
      let crossFades := countCrossFades m.node
      let paths := countPaths m.node
      IO.println s!"    crossFades={crossFades} paths={paths}"
      assertTrue (crossFades == 0) s!"expected 0 cross-fades, got {crossFades}"
      assertTrue (paths >= 1) s!"expected ≥1 path morph, got {paths}")
  , ("morph wrapper: cellophane asymmetry", do
      let a := (blueRect 40 40).cellophane 0.5
      let b := redRect 30 30
      let m := a.morph b
      let crossFades := countCrossFades m.node
      let paths := countPaths m.node
      IO.println s!"    crossFades={crossFades} paths={paths}"
      assertTrue (crossFades == 0) s!"expected 0 cross-fades, got {crossFades}"
      assertTrue (paths >= 1) s!"expected ≥1 path morph, got {paths}")
  , ("morph wrapper: clip asymmetry", do
      let a := (blueCircle 30).clip (PathData.rect 30 30)
      let b := redCircle 30
      let m := a.morph b
      let crossFades := countCrossFades m.node
      let paths := countPaths m.node
      IO.println s!"    crossFades={crossFades} paths={paths}"
      assertTrue (crossFades == 0) s!"expected 0 cross-fades, got {crossFades}"
      assertTrue (paths >= 1) s!"expected ≥1 path morph, got {paths}")
  ]

/-!
# All morph tests
-/

def morphTests : List (String × IO Unit) :=
  cubicSegTests ++ styleTests' ++ morphPlanTests ++ normalizationTests ++ morphWrapperTests
