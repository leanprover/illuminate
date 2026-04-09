/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/
module
public import Illuminate.Diagram.Placement
import Illuminate.Geometry.Basic
public section

namespace Illuminate.Diagram

/--
A curly brace spanning {name}`width`, with its tip pointing in the direction given by
{name}`angle` (in radians, counterclockwise from the positive x-axis). The brace is centered
at the origin. When a {name}`label` is provided, it is placed beyond the tip at a distance
determined by the label's envelope in the tip direction, without rotating the label.
-/
def curlyBrace {β : Type} [Backend β] (width : Float) (depth : Float := 0)
    (angle : Float := 0)
    (roundness : Float := 0.25)
    (stroke : Stroke := { color := Color.black, width := (1 : Float) })
    (label : Option (Diagram β) := none)
    (labelGap : Float := 4) : Diagram β :=
  let depth := if depth > 0 then depth else max 10 (width * 0.08)
  let hw := width / 2
  let mid := depth / 2
  -- Quarter-circle bend radius, capped so bends don't overlap
  let r := min mid (hw * roundness)
  -- Shape: vertical drop at ends → horizontal shoulder → vertical drop to center tip
  -- The path points downward (-y); it is rotated to the requested angle below.
  let path := PathData.empty
    |>.moveTo ⟨-hw, 0⟩
    |>.lineTo ⟨-hw, -(mid - r)⟩
    |>.arcTo r r 0 false true ⟨-hw + r, -mid⟩
    -- Left horizontal shoulder
    |>.lineTo ⟨-r, -mid⟩
    -- Bend down into center tip (elliptical: rx=r, ry=mid)
    |>.arcTo r mid 0 false false ⟨0, -depth⟩
    -- Bend up from center tip into right shoulder
    |>.arcTo r mid 0 false false ⟨r, -mid⟩
    -- Right horizontal shoulder
    |>.lineTo ⟨hw - r, -mid⟩
    -- Bend up into right end vertical
    |>.arcTo r r 0 false true ⟨hw, -(mid - r)⟩
    |>.lineTo ⟨hw, 0⟩
  let braceShape : Diagram β := fromStroke path stroke
  -- The path points down (-y). Rotate so that angle 0 points right (+x).
  let rotation := angle + pi / 2
  let braceShape := braceShape.rotate rotation
  match label with
  | none => braceShape
  | some lbl =>
    let tipDir : Vec2 := ⟨Float.cos angle, Float.sin angle⟩
    beside tipDir labelGap braceShape lbl

/--
Places a curly brace along the envelope edge of {name}`d` in the direction given by {name}`angle`
(radians, counterclockwise from the positive x-axis), with a label beyond the tip.

The brace spans the diagram's extent perpendicular to the tip direction.
-/
def braceBy {β : Type} [Backend β] (d : Diagram β) (angle : Float) (label : Diagram β)
    (gap : Float := 3) (depth : Float := 8) : Diagram β :=
  let tipDir : Vec2 := ⟨Float.cos angle, Float.sin angle⟩
  -- Measure the diagram's extent perpendicular to the tip direction
  let perpDir : Vec2 := ⟨-tipDir.y, tipDir.x⟩
  let env := d.getEnvelope
  let span := env[perpDir] + env[-perpDir]
  let brace := curlyBrace span (depth := depth) (angle := angle) (label := some label)
  beside tipDir gap d brace

/-- Places a curly brace spanning the full width of a diagram below it, with a label. -/
def braceBelow {β : Type} [Backend β] (d : Diagram β) (label : Diagram β)
    (gap : Float := 3) (depth : Float := 8) : Diagram β :=
  braceBy d (-pi / 2) label (gap := gap) (depth := depth)

/-- Places a curly brace spanning the full width of a diagram above it, with a label. -/
def braceAbove {β : Type} [Backend β] (d : Diagram β) (label : Diagram β)
    (gap : Float := 3) (depth : Float := 8) : Diagram β :=
  braceBy d (pi / 2) label (gap := gap) (depth := depth)

/-- Places a curly brace spanning the full height of a diagram to its left, with a label. -/
def braceLeftOf {β : Type} [Backend β] (d : Diagram β) (label : Diagram β)
    (gap : Float := 3) (depth : Float := 8) : Diagram β :=
  braceBy d pi label (gap := gap) (depth := depth)

/-- Places a curly brace spanning the full height of a diagram to its right, with a label. -/
def braceRightOf {β : Type} [Backend β] (d : Diagram β) (label : Diagram β)
    (gap : Float := 3) (depth : Float := 8) : Diagram β :=
  braceBy d 0 label (gap := gap) (depth := depth)
