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
  -- Sample frames at evenly spaced progress values
  let frames := (List.range n).map fun i =>
    let t := if n <= 1 then 0.0 else i.toFloat / (n - 1).toFloat
    (render t).named (.mkSimple s!"frame{i}")
  -- Compute uniform cell size from max envelope across all frames
  let (maxHW, maxHH) := frames.foldl (init := (0.0, 0.0)) fun (mw, mh) d =>
    match d.getEnvelope with
    | .empty => (mw, mh)
    | .nonempty env =>
      let hw := ((env Vec2.east).diag + (env Vec2.west).diag) / 2
      let hh := ((env Vec2.north).diag + (env Vec2.south).diag) / 2
      (max mw hw, max mh hh)
  let cellW := 2 * (maxHW + cellPadding)
  let cellH := 2 * (maxHH + cellPadding)
  let totalW := cols.toFloat * cellW
  let totalH := rows.toFloat * cellH
  -- Place each frame at its cell center
  let content := (List.range n).foldl (init := (Diagram.empty : Diagram SVG)) fun acc idx =>
    let d := (frames[idx]?.getD .empty)
    let col := idx % cols
    let row := idx / cols
    let cx := (col.toFloat + 0.5) * cellW - totalW / 2
    let cy := ((rows - 1 - row).toFloat + 0.5) * cellH - totalH / 2
    Diagram.compose acc (Diagram.translate cx cy d)
  -- Divider lines
  let dividerStroke : Stroke := { color := Color.lightGray, width := dividerWidth }
  let vDividers := (List.range (cols - 1)).foldl (init := Diagram.empty) fun acc i =>
    let x := (i + 1).toFloat * cellW - totalW / 2
    Diagram.compose acc (Diagram.fromStroke
      (PathData.empty.moveTo ⟨x, -totalH / 2⟩ |>.lineTo ⟨x, totalH / 2⟩) dividerStroke)
  let hDividers := (List.range (rows - 1)).foldl (init := Diagram.empty) fun acc i =>
    let y := totalH / 2 - (i + 1).toFloat * cellH
    Diagram.compose acc (Diagram.fromStroke
      (PathData.empty.moveTo ⟨-totalW / 2, y⟩ |>.lineTo ⟨totalW / 2, y⟩) dividerStroke)
  let border := Diagram.fromStroke (PathData.rect totalW totalH) dividerStroke
  [content, vDividers, hDividers, border].foldl Diagram.compose Diagram.empty
