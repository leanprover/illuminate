/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/
module
public import Illuminate.Diagram.Types
public import Illuminate.Style.Color
import Illuminate.Diagram.Basic
import Lean.DocString.Syntax
public section

namespace Illuminate

namespace Diagram

/-!
# Arrow and chevron shapes

Block arrows, double arrows, chevrons, and bent arrows for flow diagrams.
-/

variable {β : Type} [Backend β]

/--
A block arrow pointing right, centered at the origin.
The shaft is a rectangle with a triangular arrowhead on the right.
-/
def blockArrow (width height : Float) (headWidth : Float := 0) (headDepth : Float := 0)
    (fill : Fill := default) (stroke : Stroke := {})
    (name : Option Lean.Name := none) : Diagram β :=
  let hw := headWidth |> fun v => if v == 0 then height * 1.4 else v
  let hd := headDepth |> fun v => if v == 0 then width * 0.3 else v
  let halfW := width / 2
  let halfH := height / 2
  let halfHW := hw / 2
  let path : PathData := ⟨#[
    .moveTo ⟨-halfW, halfH⟩,
    .lineTo ⟨halfW - hd, halfH⟩,
    .lineTo ⟨halfW - hd, halfHW⟩,
    .lineTo ⟨halfW, 0⟩,
    .lineTo ⟨halfW - hd, -halfHW⟩,
    .lineTo ⟨halfW - hd, -halfH⟩,
    .lineTo ⟨-halfW, -halfH⟩,
    .closePath
  ]⟩
  let d : Diagram β := fromPath path fill stroke
  let halfStroke := stroke.width / 2
  match name with
  | none => d
  | some n =>
    withNameAndAnchors d n [
      { name := `north, offset := ⟨0, halfHW + halfStroke⟩ },
      { name := `south, offset := ⟨0, -(halfHW + halfStroke)⟩ },
      { name := `east, offset := ⟨halfW + halfStroke, 0⟩ },
      { name := `west, offset := ⟨-(halfW + halfStroke), 0⟩ },
      { name := `tip, offset := ⟨halfW + halfStroke, 0⟩ }
    ]

/--
A bidirectional block arrow centered at the origin, with arrowheads on both ends.
-/
def doubleArrow (width height : Float) (headWidth : Float := 0) (headDepth : Float := 0)
    (fill : Fill := default) (stroke : Stroke := {})
    (name : Option Lean.Name := none) : Diagram β :=
  let hw := headWidth |> fun v => if v == 0 then height * 1.4 else v
  let hd := headDepth |> fun v => if v == 0 then width * 0.2 else v
  let halfW := width / 2
  let halfH := height / 2
  let halfHW := hw / 2
  let path : PathData := ⟨#[
    -- Top edge from left notch to right notch
    .moveTo ⟨-halfW + hd, halfH⟩,
    .lineTo ⟨halfW - hd, halfH⟩,
    -- Right arrowhead
    .lineTo ⟨halfW - hd, halfHW⟩,
    .lineTo ⟨halfW, 0⟩,
    .lineTo ⟨halfW - hd, -halfHW⟩,
    -- Bottom edge
    .lineTo ⟨halfW - hd, -halfH⟩,
    .lineTo ⟨-halfW + hd, -halfH⟩,
    -- Left arrowhead
    .lineTo ⟨-halfW + hd, -halfHW⟩,
    .lineTo ⟨-halfW, 0⟩,
    .lineTo ⟨-halfW + hd, halfHW⟩,
    .closePath
  ]⟩
  let d : Diagram β := fromPath path fill stroke
  let halfStroke := stroke.width / 2
  match name with
  | none => d
  | some n =>
    withNameAndAnchors d n [
      { name := `north, offset := ⟨0, halfHW + halfStroke⟩ },
      { name := `south, offset := ⟨0, -(halfHW + halfStroke)⟩ },
      { name := `tipRight, offset := ⟨halfW + halfStroke, 0⟩ },
      { name := `tipLeft, offset := ⟨-(halfW + halfStroke), 0⟩ }
    ]

/--
A chevron shape centered at the origin: pointed right with a V-notch on the left.
Used for pipeline/process step indicators that chain together visually.
-/
def chevron (width height : Float) (notchDepth : Float := 0)
    (fill : Fill := default) (stroke : Stroke := {})
    (name : Option Lean.Name := none) : Diagram β :=
  let nd := if notchDepth == 0 then width * 0.2 else notchDepth
  let halfW := width / 2
  let halfH := height / 2
  let path : PathData := ⟨#[
    .moveTo ⟨-halfW, halfH⟩,
    .lineTo ⟨halfW - nd, halfH⟩,
    .lineTo ⟨halfW, 0⟩,
    .lineTo ⟨halfW - nd, -halfH⟩,
    .lineTo ⟨-halfW, -halfH⟩,
    .lineTo ⟨-halfW + nd, 0⟩,
    .closePath
  ]⟩
  let d : Diagram β := fromPath path fill stroke
  let halfStroke := stroke.width / 2
  match name with
  | none => d
  | some n =>
    withNameAndAnchors d n [
      { name := `north, offset := ⟨0, halfH + halfStroke⟩ },
      { name := `south, offset := ⟨0, -(halfH + halfStroke)⟩ },
      { name := `east, offset := ⟨halfW + halfStroke, 0⟩ },
      { name := `west, offset := ⟨-(halfW + halfStroke), 0⟩ },
      { name := `tip, offset := ⟨halfW + halfStroke, 0⟩ },
      { name := `notch, offset := ⟨-(halfW - nd), 0⟩ }
    ]

/--
A bent arrow making a 180° U-turn. The shaft goes up on the left,
curves over with a semicircular arc, and comes back down on the right
with an arrowhead at the end.
-/
def bentArrow180 (width height shaftWidth : Float)
    (headWidth : Float := 0) (headLength : Float := 0)
    (fill : Fill := default) (stroke : Stroke := {})
    (name : Option Lean.Name := none) : Diagram β :=
  let halfW := width / 2
  let halfH := height / 2
  let halfSW := shaftWidth / 2
  let arcCenterY := halfH - halfW  -- top minus the arc radius
  let outerR := halfW
  let innerR := halfW - shaftWidth
  let headDepth := if headLength == 0 then shaftWidth * 1.5 else headLength
  let headWidth := if headWidth == 0 then shaftWidth * 2 else headWidth
  let halfHdW := headWidth / 2
  let arrowBottom := -halfH
  let path : PathData := ⟨#[
    -- Start at bottom-left of left shaft (outer edge)
    .moveTo ⟨-(halfW), -halfH⟩,
    -- Up the left side (outer)
    .lineTo ⟨-(halfW), arcCenterY⟩,
    -- Outer semicircular arc from left to right
    .arcTo outerR outerR 0 false false ⟨halfW, arcCenterY⟩,
    -- Down the right side (outer) to arrowhead
    .lineTo ⟨halfW, arrowBottom + headDepth⟩,
    -- Arrowhead right wing
    .lineTo ⟨halfW + halfHdW - halfSW, arrowBottom + headDepth⟩,
    -- Arrowhead tip
    .lineTo ⟨halfW - halfSW, arrowBottom⟩,
    -- Arrowhead left wing
    .lineTo ⟨halfW - halfSW - halfHdW, arrowBottom + headDepth⟩,
    -- Back up right side inner edge
    .lineTo ⟨halfW - shaftWidth, arrowBottom + headDepth⟩,
    .lineTo ⟨halfW - shaftWidth, arcCenterY⟩,
    -- Inner semicircular arc from right to left
    .arcTo innerR innerR 0 false true ⟨-(halfW) + shaftWidth, arcCenterY⟩,
    -- Down the left side (inner)
    .lineTo ⟨-(halfW) + shaftWidth, -halfH⟩,
    .closePath
  ]⟩
  let d : Diagram β := fromPath path fill stroke
  match name with
  | none => d
  | some n =>
    withNameAndAnchors d n [
      { name := `start, offset := ⟨-(halfW) + halfSW, -halfH⟩ },
      { name := `tip, offset := ⟨halfW - halfSW, arrowBottom⟩ }
    ]

/--
A bent arrow making a 90° turn with a rounded corner.
Goes right from the start, curves 90° upward, then continues up with an arrowhead.
-/
def bentArrow90 (width height shaftWidth : Float) (cornerRadius : Float := 0)
    (headWidth : Float := 0) (headLength : Float := 0)
    (fill : Fill := default) (stroke : Stroke := {})
    (name : Option Lean.Name := none) : Diagram β :=
  let halfW := width / 2
  let halfH := height / 2
  let halfSW := shaftWidth / 2
  let cr := if cornerRadius == 0 then shaftWidth * 2 else cornerRadius
  let headDepth := if headLength == 0 then shaftWidth * 1.5 else headLength
  let headWidth := if headWidth == 0 then shaftWidth * 2 else headWidth
  let halfHdW := headWidth / 2
  -- Corner is at bottom-right area
  let cornerX := halfW - halfSW - cr
  let cornerY := -halfH + halfSW + cr
  let outerR := cr + halfSW
  let innerR := max 0.1 (cr - halfSW)
  let shaftCenterX := halfW - halfSW
  let path : PathData := ⟨#[
    -- Start at left end of horizontal shaft (bottom edge)
    .moveTo ⟨-halfW, -halfH⟩,
    -- Bottom of horizontal shaft to corner
    .lineTo ⟨cornerX, -halfH⟩,
    -- Outer 90° arc (counterclockwise from bottom to right)
    .arcTo outerR outerR 0 false true ⟨halfW, cornerY⟩,
    -- Up to arrowhead
    .lineTo ⟨halfW, halfH - headDepth⟩,
    -- Arrowhead centered on shaft
    .lineTo ⟨shaftCenterX + halfHdW, halfH - headDepth⟩,
    .lineTo ⟨shaftCenterX, halfH⟩,
    .lineTo ⟨shaftCenterX - halfHdW, halfH - headDepth⟩,
    -- Inner edge going down
    .lineTo ⟨halfW - shaftWidth, halfH - headDepth⟩,
    .lineTo ⟨halfW - shaftWidth, cornerY⟩,
    -- Inner 90° arc (clockwise from right to bottom)
    .arcTo innerR innerR 0 false false ⟨cornerX, -halfH + shaftWidth⟩,
    -- Top of horizontal shaft back to start
    .lineTo ⟨-halfW, -halfH + shaftWidth⟩,
    .closePath
  ]⟩
  let d : Diagram β := fromPath path fill stroke
  match name with
  | none => d
  | some n =>
    withNameAndAnchors d n [
      { name := `start, offset := ⟨-halfW, -halfH + halfSW⟩ },
      { name := `tip, offset := ⟨shaftCenterX, halfH⟩ },
      { name := `corner, offset := ⟨cornerX + cr, cornerY - cr⟩ }
    ]

/--
A bent arrow making a sharp 90° right-angle turn (no rounded corner).
Goes right from the start, turns sharply upward, then continues up with an arrowhead.
-/
def squareBentArrow90 (width height shaftWidth : Float)
    (headWidth : Float := 0) (headLength : Float := 0)
    (fill : Fill := default) (stroke : Stroke := {})
    (name : Option Lean.Name := none) : Diagram β :=
  let halfW := width / 2
  let halfH := height / 2
  let halfSW := shaftWidth / 2
  let headDepth := if headLength == 0 then shaftWidth * 1.5 else headLength
  let headWidth := if headWidth == 0 then shaftWidth * 2 else headWidth
  let halfHdW := headWidth / 2
  let shaftCenterX := halfW - halfSW
  let path : PathData := ⟨#[
    -- Start at left end of horizontal shaft (bottom)
    .moveTo ⟨-halfW, -halfH⟩,
    -- Bottom of horizontal shaft
    .lineTo ⟨halfW, -halfH⟩,
    -- Up the right side to arrowhead
    .lineTo ⟨halfW, halfH - headDepth⟩,
    -- Arrowhead centered on shaft
    .lineTo ⟨shaftCenterX + halfHdW, halfH - headDepth⟩,
    .lineTo ⟨shaftCenterX, halfH⟩,
    .lineTo ⟨shaftCenterX - halfHdW, halfH - headDepth⟩,
    -- Inner edge going down
    .lineTo ⟨halfW - shaftWidth, halfH - headDepth⟩,
    .lineTo ⟨halfW - shaftWidth, -halfH + shaftWidth⟩,
    -- Top of horizontal shaft back to start
    .lineTo ⟨-halfW, -halfH + shaftWidth⟩,
    .closePath
  ]⟩
  let d : Diagram β := fromPath path fill stroke
  match name with
  | none => d
  | some n =>
    withNameAndAnchors d n [
      { name := `start, offset := ⟨-halfW, -halfH + halfSW⟩ },
      { name := `tip, offset := ⟨shaftCenterX, halfH⟩ },
      { name := `corner, offset := ⟨shaftCenterX, -halfH + halfSW⟩ }
    ]
