/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/

import Tests.Helpers

open Illuminate

-- ══════════════════════════════════════════════════════════════════
-- DrawCmd / Compile (5)
-- ══════════════════════════════════════════════════════════════════

def testDrawCmd_empty : IO Unit := do
  let cmds := (Diagram.empty : Diagram Empty).compile
  assertTrue (cmds.length == 0) "empty diagram has no cmds"

def testDrawCmd_rect : IO Unit := do
  let d : Diagram Empty := Diagram.rect 4 4
  let cmds := d.compile
  -- rect with default fill (black, a=1) and default stroke (black, width=1, a=1) => fillPath + strokePath
  assertTrue (cmds.length == 2) s!"rect cmds: {cmds.length}"

def testDrawCmd_transform : IO Unit := do
  let d : Diagram Empty := .transform (Matrix.translate 10 20) (Diagram.rect 4 4)
  let cmds := d.compile
  -- pushTransform, fillPath, strokePath, popTransform
  assertTrue (cmds.length == 4) s!"transform cmds: {cmds.length}"
  match cmds.head? with
  | some (.pushTransform _) => pure ()
  | _ => throw <| IO.userError "expected pushTransform"

def testDrawCmd_compose : IO Unit := do
  let d : Diagram Empty := .compose (Diagram.rect 4 4) (Diagram.rect 2 2)
  let cmds := d.compile
  -- 2 rects × 2 cmds each = 4
  assertTrue (cmds.length == 4) s!"compose cmds: {cmds.length}"

def testDrawCmd_annotate : IO Unit := do
  let d : Diagram Empty := .annotate 42 (Diagram.rect 4 4)
  let cmds := d.compile
  -- pushAnnotation, fillPath, strokePath, popAnnotation
  assertTrue (cmds.length == 4) s!"annotate cmds: {cmds.length}"
  match cmds.head? with
  | some (.pushAnnotation 42) => pure ()
  | _ => throw <| IO.userError "expected pushAnnotation 42"

-- ══════════════════════════════════════════════════════════════════
-- SVG output (5)
-- ══════════════════════════════════════════════════════════════════

def testSvg_pathData : IO Unit := do
  let pd := PathData.line ⟨0, 0⟩ ⟨10, 20⟩
  let d := Svg.pathDataToD pd
  assertTrue (d.any (· == 'M')) "path has M"
  assertTrue (d.any (· == 'L')) "path has L"

def testSvg_fillPath : IO Unit := do
  let cmd := DrawCmd.fillPath (PathData.rect 4 4) { color := Color.red }
  let svg := Svg.renderCmd cmd
  assertContains svg "fill=\"rgb(255,0,0)\"" "fill has red"

def testSvg_strokePath : IO Unit := do
  let cmd := DrawCmd.strokePath (PathData.rect 4 4) { FullStroke.ofWidth 2 with color := Color.blue }
  let svg := Svg.renderCmd cmd
  assertContains svg "stroke=\"rgb(0,0,255)\"" "stroke has blue"
  assertContains svg "stroke-width=\"2\"" "stroke has width"

def testSvg_text : IO Unit := do
  let cmd := DrawCmd.drawTextRun "hello" default ⟨0, 0⟩
  let svg := Svg.renderCmd cmd
  assertContains svg ">hello</text>" "text content"

def testSvg_viewBox : IO Unit := do
  let svg := Svg.render [] (0, 0, 100, 100)
  assertContains svg "viewBox=\"0 0 100 100\"" "viewBox"

-- ══════════════════════════════════════════════════════════════════
-- SVG rendering (5)
-- ══════════════════════════════════════════════════════════════════

def testSvg_rectRender : IO Unit := do
  let d : Diagram Empty := Diagram.rect 4 4
  let cmds := d.compile
  let svg := Svg.render cmds (-5, -5, 10, 10)
  assertContains svg "<path" "svg has path element"
  assertContains svg "</svg>" "svg is closed"

def testSvg_circleRender : IO Unit := do
  let d : Diagram Empty := Diagram.circle 5
  let cmds := d.compile
  let svg := Svg.render cmds (-10, -10, 20, 20)
  assertContains svg "<path" "svg has path for circle"
  assertContains svg "C" "svg circle has curves"

def testSvg_transformNested : IO Unit := do
  let d : Diagram Empty := .transform (Matrix.translate 10 0) (Diagram.rect 4 4)
  let cmds := d.compile
  let svg := Svg.render cmds (-20, -20, 40, 40)
  assertContains svg "<g transform=\"matrix(" "svg has transform group"
  assertContains svg "</g>" "svg has closing group"

def testSvg_annotationId : IO Unit := do
  let d : Diagram Empty := .annotate 7 (Diagram.rect 2 2)
  let cmds := d.compile
  let svg := Svg.render cmds (-5, -5, 10, 10)
  assertContains svg "data-anno-id=\"7\"" "svg has annotation"

def testSvg_renderDiagram : IO Unit := do
  let d : Diagram Empty := Diagram.rect 10 10
  let svg := d.renderDiagram
  assertContains svg "<svg" "has svg tag"
  assertContains svg "viewBox" "has viewBox"
  assertContains svg "<path" "has path"

-- ══════════════════════════════════════════════════════════════════
-- Smiley face demo (5)
-- ══════════════════════════════════════════════════════════════════

def smileyYellow : Color := { r := 255, g := 220, b := 50 }

def smileyFace : Diagram Empty :=
  -- Face: yellow circle with black outline
  let face := Diagram.circle 50 (fill := { color := smileyYellow })
    (stroke := { color := Color.black, width := (3 : Float) })
  -- Left eye: small black circle at (-18, 15)
  let leftEye := Diagram.circle 5 (fill := { color := Color.black })
    (stroke := { color := Color.black, width := (0 : Float) })
  let leftEye := Diagram.transform (Matrix.translate (-18) 15) leftEye
  -- Right eye: small black circle at (18, 15)
  let rightEye := Diagram.circle 5 (fill := { color := Color.black })
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
  assertTrue (cmds.length > 0) s!"smiley has cmds: {cmds.length}"

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

-- ══════════════════════════════════════════════════════════════════
-- Test registration
-- ══════════════════════════════════════════════════════════════════

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
  -- Smiley face demo (5)
  ("Smiley/compiles", testSmiley_compiles),
  ("Smiley/hasFace", testSmiley_hasFace),
  ("Smiley/hasEyes", testSmiley_hasEyes),
  ("Smiley/hasSmile", testSmiley_hasSmile),
  ("Smiley/writeSvg", testSmiley_writeSvg)
]
