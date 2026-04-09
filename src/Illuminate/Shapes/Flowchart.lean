/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/
module
public import Illuminate.Diagram.Placement
import Lean.DocString.Syntax
public section

namespace Illuminate

/--
Computes the label extent in each cardinal direction, returning {lit}`(halfWidth, halfHeight)`.
Returns {lit}`(0, 0)` if the label has no envelope.
-/
private def labelExtent {β : Type} [Backend β] (label : Diagram β) : Float × Float :=
  let env := label.getEnvelope
  let hw := match env[Vec2.east]? with | some v => v | none => 0
  let hh := match env[Vec2.north]? with | some v => v | none => 0
  (hw, hh)

namespace Diagram

/-!
# Flowchart shapes

Standard flowchart node shapes, each with optional label support.
When a label is provided, the shape grows to ensure the label fits inside.
-/

variable {β : Type} [Backend β]

/--
A diamond (rhombus) centered at the origin, the standard flowchart decision node.
Cardinal anchors are placed at the four tips.

When a label is provided, the diamond grows so the label's envelope fits
inside the inscribed rectangle (which has half the diamond's width and height).
-/
def diamond (width height : Float) (fill : Fill := default) (stroke : Stroke := {})
    (label : Option (Diagram β) := none)
    (name : Option Lean.Name := none) : Diagram β :=
  let (w, h) := match label with
    | none => (width, height)
    | some lbl =>
      let (lw, lh) := labelExtent lbl
      (max width (2 * lw + stroke.width), max height (2 * lh + stroke.width))
  let hw := w / 2
  let hh := h / 2
  let path : PathData := ⟨#[
    .moveTo ⟨0, hh⟩,
    .lineTo ⟨hw, 0⟩,
    .lineTo ⟨0, -hh⟩,
    .lineTo ⟨-hw, 0⟩,
    .closePath
  ]⟩
  let shape : Diagram β := fromPath path fill stroke
  let d := match label with
    | none => shape
    | some lbl => Diagram.atop lbl shape
  let halfStroke := stroke.width / 2
  match name with
  | none => d
  | some n =>
    withNameAndAnchors d n [
      { name := `north, offset := ⟨0, hh + halfStroke⟩ },
      { name := `south, offset := ⟨0, -(hh + halfStroke)⟩ },
      { name := `east, offset := ⟨hw + halfStroke, 0⟩ },
      { name := `west, offset := ⟨-(hw + halfStroke), 0⟩ }
    ]

/--
A parallelogram centered at the origin, the standard flowchart I/O node.
The top edge is shifted right by {name}`skew`.
-/
def parallelogram (width height : Float) (skew : Float := 0)
    (fill : Fill := default) (stroke : Stroke := {})
    (label : Option (Diagram β) := none)
    (name : Option Lean.Name := none) : Diagram β :=
  let skew := if skew == 0 then width * 0.2 else skew
  let (w, h) := match label with
    | none => (width, height)
    | some lbl =>
      let (lw, lh) := labelExtent lbl
      (max width (2 * lw + skew), max height (2 * lh))
  let hw := w / 2
  let hh := h / 2
  let path : PathData := ⟨#[
    .moveTo ⟨-hw, -hh⟩,
    .lineTo ⟨hw, -hh⟩,
    .lineTo ⟨hw + skew, hh⟩,
    .lineTo ⟨-hw + skew, hh⟩,
    .closePath
  ]⟩
  let shape : Diagram β := fromPath path fill stroke
  let d := match label with
    | none => shape
    | some lbl =>
      let labelShift := skew / 2
      Diagram.atop (Diagram.transform (Matrix.translate labelShift 0) lbl) shape
  let halfStroke := stroke.width / 2
  match name with
  | none => d
  | some n =>
    let cx := skew / 2
    withNameAndAnchors d n [
      { name := `north, offset := ⟨cx, hh + halfStroke⟩ },
      { name := `south, offset := ⟨0, -(hh + halfStroke)⟩ },
      { name := `east, offset := ⟨hw + skew / 2 + halfStroke, 0⟩ },
      { name := `west, offset := ⟨-(hw - skew / 2 + halfStroke), 0⟩ },
      { name := `northeast, offset := ⟨hw + skew + halfStroke, hh + halfStroke⟩ },
      { name := `northwest, offset := ⟨-hw + skew - halfStroke, hh + halfStroke⟩ },
      { name := `southeast, offset := ⟨hw + halfStroke, -(hh + halfStroke)⟩ },
      { name := `southwest, offset := ⟨-(hw + halfStroke), -(hh + halfStroke)⟩ }
    ]

/--
A trapezoid centered at the origin with independent top and bottom widths.
The standard flowchart manual operation node.
-/
def trapezoid (topWidth bottomWidth height : Float)
    (fill : Fill := default) (stroke : Stroke := {})
    (label : Option (Diagram β) := none)
    (name : Option Lean.Name := none) : Diagram β :=
  let minW := min topWidth bottomWidth
  let (tw, bw, h) := match label with
    | none => (topWidth, bottomWidth, height)
    | some lbl =>
      let (lw, lh) := labelExtent lbl
      let neededW := 2 * lw
      let scale := if minW > 0 then max 1 (neededW / minW) else 1
      (topWidth * scale, bottomWidth * scale, max height (2 * lh))
  let htw := tw / 2
  let hbw := bw / 2
  let hh := h / 2
  let path : PathData := ⟨#[
    .moveTo ⟨-hbw, -hh⟩,
    .lineTo ⟨hbw, -hh⟩,
    .lineTo ⟨htw, hh⟩,
    .lineTo ⟨-htw, hh⟩,
    .closePath
  ]⟩
  let shape : Diagram β := fromPath path fill stroke
  let d := match label with
    | none => shape
    | some lbl => Diagram.atop lbl shape
  let halfStroke := stroke.width / 2
  let maxHW := max htw hbw
  match name with
  | none => d
  | some n =>
    withNameAndAnchors d n [
      { name := `north, offset := ⟨0, hh + halfStroke⟩ },
      { name := `south, offset := ⟨0, -(hh + halfStroke)⟩ },
      { name := `east, offset := ⟨maxHW + halfStroke, 0⟩ },
      { name := `west, offset := ⟨-(maxHW + halfStroke), 0⟩ },
      { name := `northeast, offset := ⟨htw + halfStroke, hh + halfStroke⟩ },
      { name := `northwest, offset := ⟨-(htw + halfStroke), hh + halfStroke⟩ },
      { name := `southeast, offset := ⟨hbw + halfStroke, -(hh + halfStroke)⟩ },
      { name := `southwest, offset := ⟨-(hbw + halfStroke), -(hh + halfStroke)⟩ }
    ]

/--
A document shape centered at the origin: a rectangle with a wavy (S-curve) bottom edge.
The standard flowchart document symbol.
-/
def document (width height : Float) (waveDepth : Float := 0)
    (fill : Fill := default) (stroke : Stroke := {})
    (label : Option (Diagram β) := none)
    (name : Option Lean.Name := none) : Diagram β :=
  let wd := if waveDepth == 0 then height * 0.1 else waveDepth
  let (w, h) := match label with
    | none => (width, height)
    | some lbl =>
      let (lw, lh) := labelExtent lbl
      (max width (2 * lw), max height (2 * lh + wd))
  let hw := w / 2
  let hh := h / 2
  -- Bottom edge is a cubic Bézier S-curve: first half dips down, second half rises up
  let path : PathData := ⟨#[
    .moveTo ⟨-hw, hh⟩,
    .lineTo ⟨hw, hh⟩,
    .lineTo ⟨hw, -hh + wd⟩,
    .curveTo ⟨hw / 2, -hh - wd⟩ ⟨-hw / 2, -hh + 3 * wd⟩ ⟨-hw, -hh + wd⟩,
    .closePath
  ]⟩
  let shape : Diagram β := fromPath path fill stroke
  let d := match label with
    | none => shape
    | some lbl =>
      let labelShift := wd / 2
      Diagram.atop (Diagram.transform (Matrix.translate 0 labelShift) lbl) shape
  let halfStroke := stroke.width / 2
  match name with
  | none => d
  | some n =>
    withNameAndAnchors d n [
      { name := `north, offset := ⟨0, hh + halfStroke⟩ },
      { name := `south, offset := ⟨0, -(hh - wd + halfStroke)⟩ },
      { name := `east, offset := ⟨hw + halfStroke, 0⟩ },
      { name := `west, offset := ⟨-(hw + halfStroke), 0⟩ },
      { name := `northeast, offset := ⟨hw + halfStroke, hh + halfStroke⟩ },
      { name := `northwest, offset := ⟨-(hw + halfStroke), hh + halfStroke⟩ },
      { name := `southeast, offset := ⟨hw + halfStroke, -(hh - wd + halfStroke)⟩ },
      { name := `southwest, offset := ⟨-(hw + halfStroke), -(hh - wd + halfStroke)⟩ }
    ]
