/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/

import Illuminate.Diagram
import Illuminate.Geometry
import Illuminate.Style.Color
import Illuminate.Backend.SVG


namespace Illuminate

/--
Renders evenly spaced frames of a single-step animation as a grid diagram.

Each cell shows the diagram at a different progress value from 0 to 1 (inclusive).
Cells are separated by thin divider lines. The resulting diagram can be rendered to
SVG and then to PNG using the existing visual test infrastructure.
-/
def filmstrip (render : Float → Diagram SVG)
    (cols : Nat := 4) (rows : Nat := 5)
    (cellPadding : Float := 8) (dividerWidth : Float := 0.5) : Diagram SVG :=
  let n := cols * rows
  -- Sample frames at evenly spaced progress values and pad each cell
  let frames := (List.range n).map fun i =>
    let t := if n <= 1 then 0.0 else i.toFloat / (n - 1).toFloat
    Diagram.pad cellPadding (render t)
  -- Build the grid rows (top-to-bottom, so row 0 is t=0)
  let gridRows : Array (Array (Option (Diagram SVG))) :=
    Array.range rows |>.map fun r =>
      Array.range cols |>.map fun c =>
        frames[r * cols + c]?
  let content := Diagram.grid gridRows
  -- Overlay divider lines using the grid's envelope for sizing
  let env := content.getEnvelope
  let totalW := env[Vec2.east] + env[Vec2.west]
  let totalH := env[Vec2.north] + env[Vec2.south]
  let dividerStroke : Stroke := { color := Color.lightGray, width := dividerWidth }
  let vDividers := (List.range (cols - 1)).foldl (init := Diagram.empty) fun acc i =>
    let x := (i + 1).toFloat * totalW / cols.toFloat - totalW / 2
    let line := Diagram.fromStroke
      (PathData.empty.moveTo ⟨x, -totalH / 2⟩ |>.lineTo ⟨x, totalH / 2⟩)
      dividerStroke
    Diagram.compose acc line
  let hDividers := (List.range (rows - 1)).foldl (init := Diagram.empty) fun acc i =>
    let y := totalH / 2 - (i + 1).toFloat * totalH / rows.toFloat
    let line := Diagram.fromStroke
      (PathData.empty.moveTo ⟨-totalW / 2, y⟩ |>.lineTo ⟨totalW / 2, y⟩)
      dividerStroke
    Diagram.compose acc line
  let border := Diagram.fromStroke (PathData.rect totalW totalH) dividerStroke
  [content, vDividers, hDividers, border].foldl Diagram.compose Diagram.empty
