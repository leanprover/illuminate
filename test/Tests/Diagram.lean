/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/

import Tests.Helpers

open Illuminate

/-!
# Layer 2: CorePrimitive variants
-/

def testCorePrim_path : IO Unit := do
  let p := CorePrimitive.path (PathData.rect 2 2) default {}
  match p with
  | .path _ _ _ => pure ()
  | _ => throw <| IO.userError "expected path"

def testCorePrim_text : IO Unit := do
  let p := CorePrimitive.text "hello" {}
  match p with
  | .text s _ => assertTrue (s == "hello") "text content"
  | _ => throw <| IO.userError "expected text"

def testCorePrim_image : IO Unit := do
  let p := CorePrimitive.image { path := "test.png", width := 100, height := 50 }
  match p with
  | .image ref => assertTrue (ref.width == 100) "image width"
  | _ => throw <| IO.userError "expected image"

def testCorePrim_beq : IO Unit := do
  let a := CorePrimitive.text "hi" {}
  let b := CorePrimitive.text "hi" {}
  let c := CorePrimitive.text "bye" {}
  assertTrue (a == b) "same text eq"
  assertTrue (a != c) "diff text neq"

/-!
# Layer 2: Diagram smart constructors
-/

def testDiag_empty : IO Unit := do
  let d : Diagram SVG := Diagram.emptyDiagram
  match d with
  | .empty => pure ()
  | _ => throw <| IO.userError "expected empty"

def testDiag_rect : IO Unit := do
  let d : Diagram SVG := Diagram.rect 4 2
  match d with
  | .prim (.path _ _ _) => pure ()
  | _ => throw <| IO.userError "expected prim/core/path"

def testDiag_circle : IO Unit := do
  let d : Diagram SVG := Diagram.circle 5
  match d with
  | .withEnv _ (.prim (.path _ _ _)) => pure ()
  | _ => throw <| IO.userError "expected withEnv/prim/core/path"

def testDiag_text : IO Unit := do
  let d : Diagram SVG := .text "hello"
  match d with
  | .prim (.text s _) => assertTrue (s == "hello") "text content"
  | _ => throw <| IO.userError "expected prim/core/text"

def testDiag_line : IO Unit := do
  let d : Diagram SVG := Diagram.line ⟨0, 0⟩ ⟨1, 1⟩
  match d with
  | .prim (.path _ .none _) => pure ()
  | .prim (.path _ (.solid _) _) => throw <| IO.userError "expected no fill on line"
  | _ => throw <| IO.userError "expected prim/core/path"

/-!
# Layer 2: Diagram tree structure
-/

def testDiagTree_compose : IO Unit := do
  let a : Diagram SVG := Diagram.rect 1 1
  let b : Diagram SVG := Diagram.circle 1
  let d := Diagram.compose a b
  match d with
  | .compose _ _ => pure ()
  | _ => throw <| IO.userError "expected compose"

def testDiagTree_transform : IO Unit := do
  let d : Diagram SVG := Diagram.transform (Matrix.translate 5 0) (Diagram.rect 1 1)
  match d with
  | .transform _ (.prim _) => pure ()
  | _ => throw <| IO.userError "expected transform wrapping prim"

def testDiagTree_annotate : IO Unit := do
  let d : Diagram SVG := Diagram.tag 42 (Diagram.rect 1 1)
  match d with
  | .tag tag _ => assertTrue (tag == 42) "annotation tag"
  | _ => throw <| IO.userError "expected tag"

def testDiagTree_named : IO Unit := do
  let d : Diagram SVG := Diagram.named `myBox (Diagram.rect 1 1)
  match d with
  | .named n _ => assertTrue (n == `myBox) "name matches"
  | _ => throw <| IO.userError "expected named"

def testDiagTree_nested : IO Unit := do
  let inner : Diagram SVG := Diagram.compose (Diagram.rect 1 1) (Diagram.circle 2)
  let d := Diagram.transform (Matrix.rotate 0.5) (Diagram.named `group inner)
  match d with
  | .transform _ inner2 =>
    match inner2 with
    | .named _ inner3 =>
      match inner3 with
      | .compose _ _ => pure ()
      | _ => throw <| IO.userError "expected compose inside"
    | _ => throw <| IO.userError "expected named inside"
  | _ => throw <| IO.userError "expected transform at top"

/-!
# Layer 3: PathData.bounds
-/

def testBounds_rect : IO Unit := do
  let (lo, hi) := (PathData.rect 4 2).bounds
  assertVec2Eq lo ⟨-2, -1⟩ "rect bounds lo"
  assertVec2Eq hi ⟨2, 1⟩ "rect bounds hi"

def testBounds_line : IO Unit := do
  let (lo, hi) := (PathData.line ⟨1, 2⟩ ⟨5, 8⟩).bounds
  assertVec2Eq lo ⟨1, 2⟩ "line bounds lo"
  assertVec2Eq hi ⟨5, 8⟩ "line bounds hi"

def testBounds_empty : IO Unit := do
  let (lo, hi) := PathData.empty.bounds
  assertVec2Eq lo Vec2.zero "empty bounds lo"
  assertVec2Eq hi Vec2.zero "empty bounds hi"

def testBounds_circle : IO Unit := do
  let (lo, hi) := (PathData.circle 5).bounds
  -- Control points extend to (r, k) where k = 0.5522847498 * r ≈ 2.76
  -- So bounds should be approximately (-5, -5) to (5, 5)
  assertApproxEq lo.x (-5) "circle bounds lo.x" (tol := 0.01)
  assertApproxEq lo.y (-5) "circle bounds lo.y" (tol := 0.01)
  assertApproxEq hi.x 5 "circle bounds hi.x" (tol := 0.01)
  assertApproxEq hi.y 5 "circle bounds hi.y" (tol := 0.01)

def testBounds_negativeLine : IO Unit := do
  let (lo, hi) := (PathData.line ⟨-3, -7⟩ ⟨-1, -2⟩).bounds
  assertVec2Eq lo ⟨-3, -7⟩ "neg line bounds lo"
  assertVec2Eq hi ⟨-1, -2⟩ "neg line bounds hi"

/-!
# Layer 3: Diagram.getEnvelope
-/

def testGetEnv_rect : IO Unit := do
  let d : Diagram SVG := Diagram.rect 6 4
  let env := d.getEnvelope
  assertApproxEq env[Vec2.east] 3.5 "rect env east"
  assertApproxEq env[Vec2.north] 2.5 "rect env north"

def testGetEnv_circle : IO Unit := do
  let d : Diagram SVG := Diagram.circle 5
  let env := d.getEnvelope
  assertApproxEq env[Vec2.east] 5 "circle env east" (tol := 0.01)
  assertApproxEq env[Vec2.north] 5 "circle env north" (tol := 0.01)

def testGetEnv_empty : IO Unit := do
  let d : Diagram SVG := .empty
  let env := d.getEnvelope
  assertApproxEq env[Vec2.east] 0 "empty env east"

def testGetEnv_transformed : IO Unit := do
  let d : Diagram SVG := .transform (Matrix.translate 10 0) (Diagram.rect 2 2)
  let env := d.getEnvelope
  assertApproxEq env[Vec2.east] 11.5 "translated rect env east"
  assertApproxEq env[Vec2.west] (-8.5) "translated rect env west"

def testGetEnv_compose : IO Unit := do
  let a : Diagram SVG := Diagram.rect 4 2
  let b : Diagram SVG := Diagram.rect 2 6
  let d := Diagram.compose a b
  let env := d.getEnvelope
  assertApproxEq env[Vec2.east] 2.5 "compose env east = max(2,1)"
  assertApproxEq env[Vec2.north] 3.5 "compose env north = max(1,3)"

/-!
# Layer 3: hjoin / vjoin / beside
-/

def testHcomp_envelopeWidth : IO Unit := do
  let a : Diagram SVG := Diagram.rect 4 2  -- half-width 2
  let b : Diagram SVG := Diagram.rect 6 2  -- half-width 3
  let d := Diagram.hjoin a b
  let env := d.getEnvelope
  -- total width = 4+6 = 10, centered → east=5, west=5
  assertApproxEq env[Vec2.east] 6 "hjoin east" (tol := 0.01)
  assertApproxEq env[Vec2.west] 6 "hjoin west" (tol := 0.01)

def testHcomp_height : IO Unit := do
  let a : Diagram SVG := Diagram.rect 2 4  -- half-height 2
  let b : Diagram SVG := Diagram.rect 2 8  -- half-height 4
  let d := Diagram.hjoin a b
  let env := d.getEnvelope
  assertApproxEq env[Vec2.north] 4.5 "hjoin north = max height" (tol := 0.01)

def testVcomp_envelopeHeight : IO Unit := do
  let a : Diagram SVG := Diagram.rect 2 4  -- half-height 2
  let b : Diagram SVG := Diagram.rect 2 6  -- half-height 3
  let d := Diagram.vjoin a b
  let env := d.getEnvelope
  -- total height = 4+6 = 10, centered → north=5, south=5
  assertApproxEq env[Vec2.north] 6 "vjoin north" (tol := 0.01)
  assertApproxEq env[Vec2.south] 6 "vjoin south" (tol := 0.01)

def testBeside_withGap : IO Unit := do
  let a : Diagram SVG := Diagram.rect 4 2
  let b : Diagram SVG := Diagram.rect 4 2
  let d := Diagram.beside Vec2.east 5 a b
  let env := d.getEnvelope
  -- a east=2, b west=2, gap=5 → total width=13, centered → east=6.5
  assertApproxEq env[Vec2.east] 7.5 "beside gap east" (tol := 0.01)

def testHcomp_empty : IO Unit := do
  let a : Diagram SVG := Diagram.rect 4 2
  let d := Diagram.hjoin a (Diagram.emptyDiagram : Diagram SVG)
  let env := d.getEnvelope
  assertApproxEq env[Vec2.east] 2.5 "hjoin with empty east" (tol := 0.01)

/-!
# Layer 3: hcat / vcat
-/

def testHcat_three : IO Unit := do
  let boxes : List (Diagram SVG) := [Diagram.rect 2 2, Diagram.rect 2 2, Diagram.rect 2 2]
  let d := Diagram.hcat boxes
  let env := d.getEnvelope
  -- Three 2-wide boxes: total width = 6, centered → east = west = 3.
  assertApproxEq env[Vec2.east] 4.5 "hcat three east" (tol := 0.01)

def testVcat_two : IO Unit := do
  let boxes : List (Diagram SVG) := [Diagram.rect 2 4, Diagram.rect 2 4]
  let d := Diagram.vcat boxes
  let env := d.getEnvelope
  -- Two 4-tall boxes: total height = 8, centered → south = north = 4.
  assertApproxEq env[Vec2.south] 5 "vcat two south" (tol := 0.01)

def testHcat_empty : IO Unit := do
  let d : Diagram SVG := Diagram.hcat []
  let env := d.getEnvelope
  assertApproxEq env[Vec2.east] 0 "hcat empty"

def testVcat_single : IO Unit := do
  let d : Diagram SVG := Diagram.vcat [Diagram.rect 6 4]
  let env := d.getEnvelope
  -- Single element stays at origin. rect 6 4 has half-height 2.
  assertApproxEq env[Vec2.south] 2.5 "vcat single south" (tol := 0.01)

def testHcat_singleBox : IO Unit := do
  let d : Diagram SVG := Diagram.hcat [Diagram.rect 4 2]
  let env := d.getEnvelope
  -- Single element stays at origin. rect 4 2 has half-width 2.
  assertApproxEq env[Vec2.east] 2.5 "hcat single east" (tol := 0.01)

/-!
# Layer 3: grid
-/

def testGrid_2x2 : IO Unit := do
  let cell : Diagram SVG := Diagram.rect 2 2
  let d := Diagram.grid #[#[some cell, some cell], #[some cell, some cell]]
  let env := d.getEnvelope
  -- 2x2 grid of 2×2 cells: total width = 4, centered → east = west = 2.
  assertApproxEq env[Vec2.east] 3 "grid 2x2 east" (tol := 0.1)

def testGrid_empty : IO Unit := do
  let d : Diagram SVG := Diagram.grid #[]
  match d with
  | .empty => pure ()
  | _ => throw <| IO.userError "empty grid should be .empty"

def testGrid_withNone : IO Unit := do
  let cell : Diagram SVG := Diagram.rect 4 4
  let d := Diagram.grid #[#[some cell, none], #[none, some cell]]
  let env := d.getEnvelope
  -- Should still produce a 2x2 grid with uniform cell sizes
  assertTrue (env[Vec2.east] > 0) "grid with holes has positive extent"

def testGrid_singleCell : IO Unit := do
  let cell : Diagram SVG := Diagram.rect 6 4
  let d := Diagram.grid #[#[some cell]]
  let env := d.getEnvelope
  -- 1x1 grid. Cell gets uniform envelope ofRect 3 2. Single element at origin.
  assertApproxEq env[Vec2.east] 3.5 "grid single east" (tol := 0.1)

def testGrid_1x3 : IO Unit := do
  let cell : Diagram SVG := Diagram.rect 2 2
  let d := Diagram.grid #[#[some cell, some cell, some cell]]
  let env := d.getEnvelope
  -- 1×3 grid of 2×2 cells: total width = 6, centered → east = west = 3.
  assertApproxEq env[Vec2.east] 4.5 "grid 1x3 east" (tol := 0.1)

/-!
# Layer 3: anchor / named
-/

def testAnchor_zeroEnvelope : IO Unit := do
  let d : Diagram SVG := Diagram.anchor `myPoint
  let env := d.getEnvelope
  assertApproxEq env[Vec2.east] 0 "anchor zero east"
  assertApproxEq env[Vec2.north] 0 "anchor zero north"

def testAnchor_isNamed : IO Unit := do
  let d : Diagram SVG := Diagram.anchor `pt
  match d with
  | .named n (.empty) => assertTrue (n == `pt) "anchor name"
  | _ => throw <| IO.userError "anchor should be named empty"

def testAnchor_composedEnvelope : IO Unit := do
  let box : Diagram SVG := Diagram.rect 4 2
  let d := Diagram.atop box (Diagram.anchor `center)
  let env := d.getEnvelope
  assertApproxEq env[Vec2.east] 2.5 "anchor in compose doesn't change envelope"

def testNamed_preservesEnvelope : IO Unit := do
  let d : Diagram SVG := .named `box (Diagram.rect 6 4)
  let env := d.getEnvelope
  assertApproxEq env[Vec2.east] 3.5 "named preserves east"
  assertApproxEq env[Vec2.north] 2.5 "named preserves north"

def testNamed_nestedLookup : IO Unit := do
  let inner : Diagram SVG := .named `inner (Diagram.rect 2 2)
  let outer := Diagram.named `outer inner
  match outer with
  | .named `outer (.named `inner _) => pure ()
  | _ => throw <| IO.userError "expected nested named"

/-!
# Layer 3: floating / strut / withEnvelope
-/

def testFloating_zeroEnvelope : IO Unit := do
  let d : Diagram SVG := Diagram.floating (Diagram.rect 10 10)
  let env := d.getEnvelope
  assertApproxEq env[Vec2.east] 0 "floating zero east"
  assertApproxEq env[Vec2.north] 0 "floating zero north"

def testStrut_envelope : IO Unit := do
  let d : Diagram SVG := Diagram.strut (Envelope.ofRect 5 3)
  let env := d.getEnvelope
  assertApproxEq env[Vec2.east] 5 "strut east"
  assertApproxEq env[Vec2.north] 3 "strut north"

def testStrut_invisible : IO Unit := do
  let d : Diagram SVG := Diagram.strut (Envelope.ofRect 5 3)
  match d with
  | .withEnv _ .empty => pure ()
  | _ => throw <| IO.userError "strut should be withEnv over empty"

def testWithEnvelope_override : IO Unit := do
  let d : Diagram SVG := Diagram.withEnvelope (Envelope.ofRect 100 100) (Diagram.rect 2 2)
  let env := d.getEnvelope
  assertApproxEq env[Vec2.east] 100 "withEnvelope override east"

def testFloating_inCompose : IO Unit := do
  let box : Diagram SVG := Diagram.rect 4 2
  let floatingBox : Diagram SVG := Diagram.floating (Diagram.rect 100 100)
  let d := Diagram.hjoin box floatingBox
  let env := d.getEnvelope
  -- floating has zero envelope, so hjoin should place it at east edge of box
  -- dist = 2 + 0 = 2, floating at (2,0). Union: east = max(2, 2+0) = 2
  assertApproxEq env[Vec2.east] 2.5 "floating doesn't extend hjoin" (tol := 0.01)

/-!
# Layer 3: padding
-/

def testPad_uniform : IO Unit := do
  let d : Diagram SVG := Diagram.pad 3 (Diagram.rect 4 2)
  let env := d.getEnvelope
  assertApproxEq env[Vec2.east] 5.5 "pad east 2+3"
  assertApproxEq env[Vec2.north] 4.5 "pad north 1+3"

def testPadRight_only : IO Unit := do
  let d : Diagram SVG := Diagram.padRight 5 (Diagram.rect 4 2)
  let env := d.getEnvelope
  assertApproxEq env[Vec2.east] 7.5 "padRight east 2+5"
  assertApproxEq env[Vec2.west] 2.5 "padRight west unchanged"

def testPadLRTB : IO Unit := do
  let d : Diagram SVG := Diagram.padLRTB 1 2 3 4 (Diagram.rect 4 2)
  let env := d.getEnvelope
  assertApproxEq env[Vec2.west] 3.5 "padLRTB west 2+1"
  assertApproxEq env[Vec2.east] 4.5 "padLRTB east 2+2"
  assertApproxEq env[Vec2.north] 4.5 "padLRTB north 1+3"
  assertApproxEq env[Vec2.south] 5.5 "padLRTB south 1+4"

def testPadXY : IO Unit := do
  let d : Diagram SVG := Diagram.padXY 2 3 (Diagram.rect 4 2)
  let env := d.getEnvelope
  assertApproxEq env[Vec2.east] 4.5 "padXY east 2+2"
  assertApproxEq env[Vec2.north] 4.5 "padXY north 1+3"

def testPad_zero : IO Unit := do
  let d : Diagram SVG := Diagram.pad 0 (Diagram.rect 4 2)
  let env := d.getEnvelope
  assertApproxEq env[Vec2.east] 2.5 "pad zero east unchanged"

/-!
# Layer 3: setEnvelope / hGap / vGap
-/

def testSetEnvelopeRight : IO Unit := do
  let d : Diagram SVG := Diagram.setEnvelopeRight 10 (Diagram.rect 4 2)
  let env := d.getEnvelope
  assertApproxEq env[Vec2.east] 10 "setEnvelopeRight east"
  assertApproxEq env[Vec2.west] 2.5 "setEnvelopeRight west unchanged"

def testSetEnvelopeTop : IO Unit := do
  let d : Diagram SVG := Diagram.setEnvelopeTop 20 (Diagram.rect 4 2)
  let env := d.getEnvelope
  assertApproxEq env[Vec2.north] 20 "setEnvelopeTop north"

def testHGap : IO Unit := do
  let d : Diagram SVG := .hgap 8
  let env := d.getEnvelope
  assertApproxEq env[Vec2.east] 4 "hGap east = half width"
  assertApproxEq env[Vec2.north] 0 "hGap zero height"

def testVGap : IO Unit := do
  let d : Diagram SVG := .vgap 6
  let env := d.getEnvelope
  assertApproxEq env[Vec2.north] 3 "vGap north = half height"
  assertApproxEq env[Vec2.east] 0 "vGap zero width"

def testHGap_inCompose : IO Unit := do
  let a : Diagram SVG := Diagram.rect 4 2
  let gap : Diagram SVG := .hgap 6
  let b : Diagram SVG := Diagram.rect 4 2
  let d := Diagram.hcat [a, gap, b]
  let env := d.getEnvelope
  -- rect4 + gap6 + rect4 = total width 14, centered → east = west = 7.
  assertApproxEq env[Vec2.east] 8 "hGap in compose east" (tol := 0.01)

/-!
# Layer 3: hAppendAlign
-/

def testAlignFraction_bottom : IO Unit := do
  let a : Diagram SVG := Diagram.rect 2 4  -- half-height 2
  let b : Diagram SVG := Diagram.rect 2 8  -- half-height 4
  let d := Diagram.hAppendAlign (.fraction 0) a b
  let env := d.getEnvelope
  -- fraction 0 = align bottoms. a bottom at -2, b bottom at -4.
  -- dy = aY - bY = (-2) - (-4) = 2. b shifted up by 2.
  -- b north at 2+4=6, b south at 2-4=-2
  assertApproxEq env[Vec2.north] 6.5 "align bottom north" (tol := 0.01)
  assertApproxEq env[Vec2.south] 2.5 "align bottom south" (tol := 0.01)

def testAlignFraction_top : IO Unit := do
  let a : Diagram SVG := Diagram.rect 2 4  -- half-height 2
  let b : Diagram SVG := Diagram.rect 2 8  -- half-height 4
  let d := Diagram.hAppendAlign (.fraction 1) a b
  let env := d.getEnvelope
  -- fraction 1 = align tops. a top at 2, b top at 4.
  -- dy = 2 - 4 = -2. b shifted down by 2.
  -- b north at -2+4=2, b south at -2-4=-6
  assertApproxEq env[Vec2.north] 2.5 "align top north" (tol := 0.01)
  assertApproxEq env[Vec2.south] 6.5 "align top south" (tol := 0.01)

def testAlignFraction_center : IO Unit := do
  let a : Diagram SVG := Diagram.rect 2 4  -- half-height 2
  let b : Diagram SVG := Diagram.rect 2 8  -- half-height 4
  let d := Diagram.hAppendAlign (.fraction 0.5) a b
  let env := d.getEnvelope
  -- fraction 0.5 = align centers. Both centered at 0.
  -- dy = 0 - 0 = 0. Same as normal hAppend.
  assertApproxEq env[Vec2.north] 4.5 "align center north" (tol := 0.01)
  assertApproxEq env[Vec2.south] 4.5 "align center south" (tol := 0.01)

def testAlignFraction_sameHeight : IO Unit := do
  let a : Diagram SVG := Diagram.rect 2 4
  let b : Diagram SVG := Diagram.rect 2 4
  let d := Diagram.hAppendAlign (.fraction 0) a b
  let env := d.getEnvelope
  -- Same height, any alignment should give same result
  assertApproxEq env[Vec2.north] 2.5 "align same height north" (tol := 0.01)
  assertApproxEq env[Vec2.south] 2.5 "align same height south" (tol := 0.01)

def testAlignAnchor_fallback : IO Unit := do
  let a : Diagram SVG := Diagram.rect 2 4
  let b : Diagram SVG := Diagram.rect 2 8
  -- anchor alignment falls back to center for now
  let d := Diagram.hAppendAlign (.anchor `baseline) a b
  let env := d.getEnvelope
  assertApproxEq env[Vec2.north] 4.5 "anchor fallback north" (tol := 0.01)

/-!
# HorizontalAlignment / VerticalAlignment
-/

def testHcat_topAlign : IO Unit := do
  let a : Diagram SVG := Diagram.rect 2 4  -- half-height 2
  let b : Diagram SVG := Diagram.rect 2 8  -- half-height 4
  let d := Diagram.hcat [a, b] (align := .top)
  let env := d.getEnvelope
  -- Top-aligned, then centered: north=2, south=6 → both = 4.
  assertApproxEq env[Vec2.north] 4.5 "hcat top north" (tol := 0.01)
  assertApproxEq env[Vec2.south] 4.5 "hcat top south" (tol := 0.01)

def testHcat_bottomAlign : IO Unit := do
  let a : Diagram SVG := Diagram.rect 2 4  -- half-height 2
  let b : Diagram SVG := Diagram.rect 2 8  -- half-height 4
  let d := Diagram.hcat [a, b] (align := .bottom)
  let env := d.getEnvelope
  -- Bottom-aligned, then centered: north=6, south=2 → both = 4.
  assertApproxEq env[Vec2.north] 4.5 "hcat bottom north" (tol := 0.01)
  assertApproxEq env[Vec2.south] 4.5 "hcat bottom south" (tol := 0.01)

def testVcat_leftAlign : IO Unit := do
  let a : Diagram SVG := Diagram.rect 4 2  -- half-width 2
  let b : Diagram SVG := Diagram.rect 8 2  -- half-width 4
  let d := Diagram.vcat [a, b] (align := .left)
  let env := d.getEnvelope
  -- Left-aligned, then centered: east=6, west=2 → both = 4.
  assertApproxEq env[Vec2.east] 4.5 "vcat left east" (tol := 0.01)
  assertApproxEq env[Vec2.west] 4.5 "vcat left west" (tol := 0.01)

def testVcat_rightAlign : IO Unit := do
  let a : Diagram SVG := Diagram.rect 4 2  -- half-width 2
  let b : Diagram SVG := Diagram.rect 8 2  -- half-width 4
  let d := Diagram.vcat [a, b] (align := .right)
  let env := d.getEnvelope
  -- Right-aligned, then centered: east=2, west=6 → both = 4.
  assertApproxEq env[Vec2.east] 4.5 "vcat right east" (tol := 0.01)
  assertApproxEq env[Vec2.west] 4.5 "vcat right west" (tol := 0.01)

def testHsep_topAlign : IO Unit := do
  let a : Diagram SVG := Diagram.rect 2 4  -- half-height 2
  let b : Diagram SVG := Diagram.rect 2 8  -- half-height 4
  let d := Diagram.hsep 5 [a, b] (align := .top)
  let env := d.getEnvelope
  -- Same vertical alignment as hcat top, centered → north = south = 4.
  assertApproxEq env[Vec2.north] 4.5 "hsep top north" (tol := 0.01)
  assertApproxEq env[Vec2.south] 4.5 "hsep top south" (tol := 0.01)

def testVsep_leftAlign : IO Unit := do
  let a : Diagram SVG := Diagram.rect 4 2  -- half-width 2
  let b : Diagram SVG := Diagram.rect 8 2  -- half-width 4
  let d := Diagram.vsep 5 [a, b] (align := .left)
  let env := d.getEnvelope
  -- Same horizontal alignment as vcat left, centered → east = west = 4.
  assertApproxEq env[Vec2.east] 4.5 "vsep left east" (tol := 0.01)
  assertApproxEq env[Vec2.west] 4.5 "vsep left west" (tol := 0.01)

/-!
# Envelope convexity
-/

def testConvex_rect : IO Unit := do
  let env := (Diagram.rect 6 4 : Diagram SVG).getEnvelope
  assertEnvelopeConvex env "rect"

def testConvex_circle : IO Unit := do
  let env := (Diagram.circle 5 : Diagram SVG).getEnvelope
  assertEnvelopeConvex env "circle"

def testConvex_union : IO Unit := do
  let a := (Diagram.rect 10 2 : Diagram SVG).getEnvelope
  let b := (Diagram.rect 2 10 : Diagram SVG).getEnvelope
  assertEnvelopeConvex (Envelope.union a b) "union of cross rects"

def testConvex_transformed : IO Unit := do
  let env := (Diagram.rotate (pi / 6) (Diagram.rect 8 3) : Diagram SVG).getEnvelope
  assertEnvelopeConvex env "rotated rect"

def testConvex_multilineText : IO Unit := do
  let env := (Diagram.text "short\nabcdefghij\nhi" : Diagram SVG).getEnvelope
  assertEnvelopeConvex env "multiline text"

def diagramTests : List (String × IO Unit) := [
    -- CorePrimitive (5)
    ("CorePrimitive/path", testCorePrim_path),
    ("CorePrimitive/text", testCorePrim_text),

    ("CorePrimitive/image", testCorePrim_image),
    ("CorePrimitive/beq", testCorePrim_beq),
    -- Diagram smart constructors (5)
    ("Diagram/empty", testDiag_empty),
    ("Diagram/rect", testDiag_rect),
    ("Diagram/circle", testDiag_circle),
    ("Diagram/text", testDiag_text),
    ("Diagram/line", testDiag_line),
    -- Diagram tree structure (5)
    ("DiagramTree/compose", testDiagTree_compose),
    ("DiagramTree/transform", testDiagTree_transform),
    ("DiagramTree/annotate", testDiagTree_annotate),
    ("DiagramTree/named", testDiagTree_named),
    ("DiagramTree/nested", testDiagTree_nested),
    -- PathData.bounds (5)
    ("PathData.bounds/rect", testBounds_rect),
    ("PathData.bounds/line", testBounds_line),
    ("PathData.bounds/empty", testBounds_empty),
    ("PathData.bounds/circle", testBounds_circle),
    ("PathData.bounds/negativeLine", testBounds_negativeLine),
    -- Diagram.getEnvelope (5)
    ("Diagram.getEnvelope/rect", testGetEnv_rect),
    ("Diagram.getEnvelope/circle", testGetEnv_circle),
    ("Diagram.getEnvelope/empty", testGetEnv_empty),
    ("Diagram.getEnvelope/transformed", testGetEnv_transformed),
    ("Diagram.getEnvelope/compose", testGetEnv_compose),
    -- hAppend / beside (5)
    ("hjoin/envelopeWidth", testHcomp_envelopeWidth),
    ("hjoin/height", testHcomp_height),
    ("vjoin/envelopeHeight", testVcomp_envelopeHeight),
    ("beside/withGap", testBeside_withGap),
    ("hjoin/empty", testHcomp_empty),
    -- hcat / vcat (5)
    ("hcat/three", testHcat_three),
    ("vcat/two", testVcat_two),
    ("hcat/empty", testHcat_empty),
    ("vcat/single", testVcat_single),
    ("hcat/singleBox", testHcat_singleBox),
    -- grid (5)
    ("grid/2x2", testGrid_2x2),
    ("grid/empty", testGrid_empty),
    ("grid/withNone", testGrid_withNone),
    ("grid/singleCell", testGrid_singleCell),
    ("grid/1x3", testGrid_1x3),
    -- anchor / named (5)
    ("anchor/zeroEnvelope", testAnchor_zeroEnvelope),
    ("anchor/isNamed", testAnchor_isNamed),
    ("anchor/composedEnvelope", testAnchor_composedEnvelope),
    ("named/preservesEnvelope", testNamed_preservesEnvelope),
    ("named/nestedLookup", testNamed_nestedLookup),
    -- floating / strut / withEnvelope (5)
    ("floating/zeroEnvelope", testFloating_zeroEnvelope),
    ("strut/envelope", testStrut_envelope),
    ("strut/invisible", testStrut_invisible),
    ("withEnvelope/override", testWithEnvelope_override),
    ("floating/inCompose", testFloating_inCompose),
    -- padding (5)
    ("pad/uniform", testPad_uniform),
    ("padRight/only", testPadRight_only),
    ("padLRTB", testPadLRTB),
    ("padXY", testPadXY),
    ("pad/zero", testPad_zero),
    -- setEnvelope / gaps (5)
    ("setEnvelopeRight", testSetEnvelopeRight),
    ("setEnvelopeTop", testSetEnvelopeTop),
    ("hGap", testHGap),
    ("vGap", testVGap),
    ("hGap/inCompose", testHGap_inCompose),
    -- hAppendAlign (5)
    ("hAppendAlign/bottom", testAlignFraction_bottom),
    ("hAppendAlign/top", testAlignFraction_top),
    ("hAppendAlign/center", testAlignFraction_center),
    ("hAppendAlign/sameHeight", testAlignFraction_sameHeight),
    ("hAppendAlign/anchorFallback", testAlignAnchor_fallback),
    -- HorizontalAlignment / VerticalAlignment (6)
    ("hcat/topAlign", testHcat_topAlign),
    ("hcat/bottomAlign", testHcat_bottomAlign),
    ("vcat/leftAlign", testVcat_leftAlign),
    ("vcat/rightAlign", testVcat_rightAlign),
    ("hsep/topAlign", testHsep_topAlign),
    ("vsep/leftAlign", testVsep_leftAlign),
    -- Envelope convexity (5)
    ("convex/rect", testConvex_rect),
    ("convex/circle", testConvex_circle),
    ("convex/union", testConvex_union),
    ("convex/transformed", testConvex_transformed),
    ("convex/multilineText", testConvex_multilineText)
  ]
