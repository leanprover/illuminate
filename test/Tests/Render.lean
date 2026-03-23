/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/

import Tests.Helpers

open Illuminate

/-!
# DrawCmd / Compile (5)
-/

def testDrawCmd_empty : IO Unit := do
  let cmds := (Diagram.empty : Diagram SVG).compile
  assertTrue (cmds.size == 0) "empty diagram has no cmds"

def testDrawCmd_rect : IO Unit := do
  let d : Diagram SVG := Diagram.rect 4 4
  let cmds := d.compile
  -- default fill (lightGray, a=1) => fillPath + strokePath
  assertTrue (cmds.size == 2) s!"rect cmds: {cmds.size}"

def testDrawCmd_transform : IO Unit := do
  let d : Diagram SVG := .transform (Matrix.translate 10 20) (Diagram.rect 4 4)
  let cmds := d.compile
  -- pushTransform, fillPath, strokePath, popTransform
  assertTrue (cmds.size == 4) s!"transform cmds: {cmds.size}"
  match cmds[0]? with
  | some (DrawCmd.pushTransform _) => pure ()
  | _ => throw <| IO.userError "expected pushTransform"

def testDrawCmd_compose : IO Unit := do
  let d : Diagram SVG := .compose (Diagram.rect 4 4) (Diagram.rect 2 2)
  let cmds := d.compile
  -- 2 rects × 2 cmds each (fill + stroke) = 4
  assertTrue (cmds.size == 4) s!"compose cmds: {cmds.size}"

def testDrawCmd_annotate : IO Unit := do
  let d : Diagram SVG := .tag 42 (Diagram.rect 4 4)
  let cmds := d.compile
  -- pushAnnotation, fillPath, strokePath, popAnnotation
  assertTrue (cmds.size == 4) s!"annotate cmds: {cmds.size}"
  match cmds[0]? with
  | some (DrawCmd.pushAnnotation 42) => pure ()
  | _ => throw <| IO.userError "expected pushAnnotation 42"

/-!
# SVG output (5)
-/

def testSvg_pathData : IO Unit := do
  let pd := PathData.line ⟨0, 0⟩ ⟨10, 20⟩
  let d := Svg.pathDataToD pd
  assertTrue (d.any (· == 'M')) "path has M"
  assertTrue (d.any (· == 'L')) "path has L"

def testSvg_fillPath : IO Unit := do
  let cmd : DrawCmd SVG := .fillPath (PathData.rect 4 4) (.solid { color := Color.red }) none
  let svg := Svg.renderCmd cmd
  assertContains svg "fill=\"rgb(255,0,0)\"" "fill has red"

def testSvg_strokePath : IO Unit := do
  let cmd : DrawCmd SVG := .strokePath (PathData.rect 4 4) { Stroke.ofWidth 2 with color := Color.blue }
  let svg := Svg.renderCmd cmd
  assertContains svg "stroke=\"rgb(0,0,255)\"" "stroke has blue"
  assertContains svg "stroke-width=\"2\"" "stroke has width"

def testSvg_text : IO Unit := do
  let cmd : DrawCmd SVG := .drawTextRun "hello" default ⟨0, 0⟩
  let svg := Svg.renderCmd cmd
  assertContains svg ">hello</text>" "text content"

def testSvg_viewBox : IO Unit := do
  let svg := Svg.render (β := SVG) #[] { minX := 0, minY := 0, width := 100, height := 100 }
  assertContains svg "viewBox=\"0 0 100 100\"" "viewBox"

/-!
# SVG rendering (5)
-/

def testSvg_rectRender : IO Unit := do
  let d : Diagram SVG := Diagram.rect 4 4
  let cmds := d.compile
  let svg := Svg.render cmds { minX := -5, minY := -5, width := 10, height := 10 }
  assertContains svg "<path" "svg has path element"
  assertContains svg "</svg>" "svg is closed"

def testSvg_circleRender : IO Unit := do
  let d : Diagram SVG := Diagram.circle 5
  let cmds := d.compile
  let svg := Svg.render cmds { minX := -10, minY := -10, width := 20, height := 20 }
  assertContains svg "<path" "svg has path for circle"
  assertContains svg "C" "svg circle has curves"

def testSvg_transformNested : IO Unit := do
  let d : Diagram SVG := .transform (Matrix.translate 10 0) (Diagram.rect 4 4)
  let cmds := d.compile
  let svg := Svg.render cmds { minX := -20, minY := -20, width := 40, height := 40 }
  assertContains svg "<g transform=\"matrix(" "svg has transform group"
  assertContains svg "</g>" "svg has closing group"

def testSvg_annotationId : IO Unit := do
  let d : Diagram SVG := .tag 7 (Diagram.rect 2 2)
  let cmds := d.compile
  let svg := Svg.render cmds { minX := -5, minY := -5, width := 10, height := 10 }
  assertContains svg "data-anno-id=\"7\"" "svg has annotation"

def testSvg_renderDiagram : IO Unit := do
  let d : Diagram SVG := Diagram.rect 10 10
  let svg := d.renderDiagram
  assertContains svg "<svg" "has svg tag"
  assertContains svg "viewBox" "has viewBox"
  assertContains svg "<path" "has path"

/-!
# Gradient rendering (5)
-/

def testGradientStops : Array GradientStop := #[
  { offset := 0, color := Color.red },
  { offset := 1, color := Color.blue }
]

def testSvg_gradientFillPath : IO Unit := do
  let g := Gradient.linear 0 10 0 (-10) testGradientStops
  let cmd : DrawCmd SVG := .fillPath (PathData.rect 20 20) (.gradient g) (some 0)
  let svg := Svg.renderCmd cmd
  assertContains svg "url(#grad0)" "gradient fill references defs"
  assertContains svg "<path" "gradient produces a path element"

def testSvg_gradientDefs : IO Unit := do
  let g := Gradient.linear 0 10 0 (-10) testGradientStops
  let d : Diagram SVG := Diagram.rect 20 20 (fill := .gradient g)
  let svg := d.renderDiagram
  assertContains svg "<defs>" "SVG has defs block"
  assertContains svg "<linearGradient" "defs contains linearGradient"
  assertContains svg "gradientUnits=\"userSpaceOnUse\"" "uses userSpaceOnUse"
  assertContains svg "<stop" "gradient has stops"

def testSvg_radialGradientDefs : IO Unit := do
  let g := Gradient.radialSymmetric 15 testGradientStops
  let d : Diagram SVG := Diagram.circle 15 (fill := .gradient g)
  let svg := d.renderDiagram
  assertContains svg "<radialGradient" "defs contains radialGradient"
  assertContains svg "stop-color=\"rgb(255,0,0)\"" "has red stop"
  assertContains svg "stop-color=\"rgb(0,0,255)\"" "has blue stop"

def testSvg_gradientSpread : IO Unit := do
  let g := Gradient.linear 0 5 0 (-5) testGradientStops (spread := .reflect)
  let d : Diagram SVG := Diagram.rect 20 20 (fill := .gradient g)
  let svg := d.renderDiagram
  assertContains svg "spreadMethod=\"reflect\"" "has reflect spread"

def testSvg_gradientNone : IO Unit := do
  let cmd : DrawCmd SVG := .fillPath (PathData.rect 4 4) .none none
  let svg := Svg.renderCmd cmd
  assertTrue (svg.isEmpty) "none fill still produces empty string"

/-!
# Gradient transforms (3)
-/

/--
Verifies that a translated gradient-filled shape keeps its gradient {lit}`<defs>` inside the
transform group, so {lit}`userSpaceOnUse` coordinates are interpreted in local space.
-/
def testSvg_gradientTranslated : IO Unit := do
  let g := Gradient.horizontal 20 testGradientStops
  let r : Diagram SVG := Diagram.rect 20 10 (fill := .gradient g)
  let d := Diagram.transform (Matrix.translate 100 0) r
  let svg := d.renderDiagram
  -- The gradient def must appear inside the transform group.
  -- If it were hoisted above the <g transform>, the userSpaceOnUse coordinates
  -- would be wrong because they'd be interpreted in the parent coordinate system.
  assertContains svg "<g transform" "has transform group"
  assertContains svg "<linearGradient" "has gradient def"
  -- Gradient coordinates should be local (-10 to 10), not shifted by the translation
  assertContains svg "x1=\"-10\"" "gradient x1 is in local coords"
  assertContains svg "x2=\"10\"" "gradient x2 is in local coords"

/--
Verifies that two gradient-filled shapes composed with {name}`Diagram.hsep` each get their own
gradient definition inside their respective transform groups.
-/
def testSvg_gradientHsep : IO Unit := do
  let g := Gradient.horizontal 20 testGradientStops
  let r : Diagram SVG := Diagram.rect 20 10 (fill := .gradient g)
  let d := Diagram.hsep 10 [r, r]
  let svg := d.renderDiagram
  -- Two gradient rects side-by-side should produce two separate gradient defs
  let gradCount := (svg.splitOn "<linearGradient").length - 1
  assertTrue (gradCount == 2) s!"expected 2 gradient defs, got {gradCount}"
  -- Each should reference its own gradient by different IDs
  let urlCount := (svg.splitOn "url(#grad").length - 1
  assertTrue (urlCount == 2) s!"expected 2 gradient url refs, got {urlCount}"

/--
Verifies that a radial gradient inside a nested transform (rotate + translate)
keeps local coordinates and the defs stay inside the transform groups.
-/
def testSvg_gradientNestedTransform : IO Unit := do
  let g := Gradient.radialSymmetric 15 testGradientStops
  let c : Diagram SVG := Diagram.circle 15 (fill := .gradient g)
  let d := Diagram.transform (Matrix.translate 50 30)
    (Diagram.transform (Matrix.rotate (pi / 4)) c)
  let svg := d.renderDiagram
  assertContains svg "<radialGradient" "has radial gradient"
  -- Gradient center should still be at origin (local coords), not at (50, 30)
  assertContains svg "cx=\"0\"" "radial cx is local"
  assertContains svg "cy=\"0\"" "radial cy is local"
  assertContains svg "r=\"15\"" "radial r is preserved"

/-!
# Smiley face demo (5)
-/

def smileyYellow : Color := { r := 255, g := 220, b := 50 }

def smileyFace : Diagram SVG :=
  -- Face: yellow circle with black outline
  let face := Diagram.circle 50 (fill := .solid { color := smileyYellow })
    (stroke := { color := Color.black, width := (3 : Float) })
  -- Left eye: small black circle at (-18, 15)
  let leftEye := Diagram.circle 5 (fill := .solid { color := Color.black })
    (stroke := { color := Color.black, width := (0 : Float) })
  let leftEye := Diagram.transform (Matrix.translate (-18) 15) leftEye
  -- Right eye: small black circle at (18, 15)
  let rightEye := Diagram.circle 5 (fill := .solid { color := Color.black })
    (stroke := { color := Color.black, width := (0 : Float) })
  let rightEye := Diagram.transform (Matrix.translate 18 15) rightEye
  -- Smile: a curved path (arc from left to right)
  let smile := Diagram.fromStroke
    (PathData.empty
      |>.moveTo ⟨-25, -10⟩
      |>.curveTo ⟨-15, -30⟩ ⟨15, -30⟩ ⟨25, -10⟩)
    { color := Color.black, width := (3 : Float), lineCap := LineCap.round }
  -- Compose all parts
  .compose (.compose (.compose face leftEye) rightEye) smile

def testSmiley_compiles : IO Unit := do
  let cmds := smileyFace.compile
  assertTrue (cmds.size > 0) s!"smiley has cmds: {cmds.size}"

def testSmiley_hasFace : IO Unit := do
  let svg := smileyFace.renderDiagram (padding := 5)
  assertContains svg "rgb(255,220,50)" "smiley has yellow face"

def testSmiley_hasEyes : IO Unit := do
  let svg := smileyFace.renderDiagram (padding := 5)
  assertContains svg "<g transform" "smiley has transform groups for eyes"

def testSmiley_hasSmile : IO Unit := do
  let svg := smileyFace.renderDiagram (padding := 5)
  assertContains svg "C" "smiley has curves"

def testSmiley_writeSvg : IO Unit := do
  let svg := smileyFace.renderDiagram (padding := 5)
  IO.FS.writeFile "smiley.svg" svg
  let contents ← IO.FS.readFile "smiley.svg"
  assertContains contents "<svg" "written file has svg"
  assertContains contents "</svg>" "written file is complete"
  IO.println s!"  → wrote smiley.svg ({svg.length} bytes)"

/-!
# Test registration
-/

def renderTests : List (String × IO Unit) := [
  -- DrawCmd / Compile (5)
  ("DrawCmd/emptyDiagram", testDrawCmd_empty),
  ("DrawCmd/rect", testDrawCmd_rect),
  ("DrawCmd/transform", testDrawCmd_transform),
  ("DrawCmd/compose", testDrawCmd_compose),
  ("DrawCmd/annotate", testDrawCmd_annotate),
  -- SVG output (5)
  ("SVG/pathData", testSvg_pathData),
  ("SVG/fillPath", testSvg_fillPath),
  ("SVG/strokePath", testSvg_strokePath),
  ("SVG/text", testSvg_text),
  ("SVG/viewBox", testSvg_viewBox),
  -- SVG rendering (5)
  ("SVG/rectRender", testSvg_rectRender),
  ("SVG/circleRender", testSvg_circleRender),
  ("SVG/transformNested", testSvg_transformNested),
  ("SVG/annotationId", testSvg_annotationId),
  ("SVG/renderDiagram", testSvg_renderDiagram),
  -- Gradient rendering (5)
  ("SVG/gradientFillPath", testSvg_gradientFillPath),
  ("SVG/gradientDefs", testSvg_gradientDefs),
  ("SVG/radialGradientDefs", testSvg_radialGradientDefs),
  ("SVG/gradientSpread", testSvg_gradientSpread),
  ("SVG/gradientNone", testSvg_gradientNone),
  -- Gradient transforms (3)
  ("SVG/gradientTranslated", testSvg_gradientTranslated),
  ("SVG/gradientHsep", testSvg_gradientHsep),
  ("SVG/gradientNestedTransform", testSvg_gradientNestedTransform),
  -- Smiley face demo (5)
  ("Smiley/compiles", testSmiley_compiles),
  ("Smiley/hasFace", testSmiley_hasFace),
  ("Smiley/hasEyes", testSmiley_hasEyes),
  ("Smiley/hasSmile", testSmiley_hasSmile),
  ("Smiley/writeSvg", testSmiley_writeSvg)
]
