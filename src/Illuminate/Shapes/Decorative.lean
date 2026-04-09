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

namespace Diagram

/-!
# Decorative shapes

Heart, plus (n-armed), stadium, cylinder, and cloud shapes.
-/

variable {β : Type} [Backend β]

/--
A heart shape centered at the origin, constructed using cubic Bézier curves.
The top has two rounded bumps meeting at a center cleft, and the bottom
comes to a point.

The {name}`cleft` parameter (0 to 1) controls how deep the center indentation
is between the two lobes: 0 means flush with the lobe tops, 1 means the cleft
reaches all the way down to the bottom point.
-/
def heart (width : Float := 0) (height : Float := 0) (cleft : Float := 0.3)
    (fill : Fill := default) (stroke : Stroke := {})
    (name : Option Lean.Name := none) : Diagram β :=
  -- Default: width slightly larger than height for a natural heart shape
  let h := if height == 0 then (if width == 0 then 40 else width * 0.9) else height
  let w := if width == 0 then h * 1.1 else width
  let hw := w / 2
  let cl := max 0 (min 1 cleft)
  let hh := h / 2
  -- Key y-levels: top of lobes, waist (cleft & widest), bottom point
  let topY := hh
  let bottomY := -hh
  let waistY := hh - cl * h   -- cleft=0 → flush with top, cleft=1 → at bottom
  -- Proportional y-levels below the waist (from the reference heart SVG)
  let belowWaist := waistY - bottomY
  let midCtrlY := waistY - belowWaist * (4.0 / 14.0)
  let lowerY := waistY - belowWaist * (8.0 / 14.0)
  let nearBotCtrlY := waistY - belowWaist * (12.0 / 14.0)
  -- Lobe top x-position (halfway between center and edge)
  let lobeX := hw * 0.5
  -- Precomputed values for quadratic-to-cubic control point conversion
  let yA := (waistY + 2 * topY) / 3
  let yB := (waistY + 2 * midCtrlY) / 3
  let yC := (lowerY + 2 * midCtrlY) / 3
  let yD := (lowerY + 2 * nearBotCtrlY) / 3
  let yE := (bottomY + 2 * nearBotCtrlY) / 3
  let xA := lobeX / 3
  let xB := (lobeX + 2 * hw) / 3
  -- 8 quadratic Bézier segments (from reference SVG) converted to cubic
  let path : PathData := ⟨#[
    .moveTo ⟨0, waistY⟩,
    -- Seg 1: cleft → right lobe top
    .curveTo ⟨0, yA⟩ ⟨xA, topY⟩ ⟨lobeX, topY⟩,
    -- Seg 2: right lobe top → right widest
    .curveTo ⟨xB, topY⟩ ⟨hw, yA⟩ ⟨hw, waistY⟩,
    -- Seg 3: right widest → lower right
    .curveTo ⟨hw, yB⟩ ⟨xB, yC⟩ ⟨lobeX, lowerY⟩,
    -- Seg 4: lower right → bottom point
    .curveTo ⟨xA, yD⟩ ⟨0, yE⟩ ⟨0, bottomY⟩,
    -- Seg 5: bottom point → lower left
    .curveTo ⟨0, yE⟩ ⟨-xA, yD⟩ ⟨-lobeX, lowerY⟩,
    -- Seg 6: lower left → left widest
    .curveTo ⟨-xB, yC⟩ ⟨-hw, yB⟩ ⟨-hw, waistY⟩,
    -- Seg 7: left widest → left lobe top
    .curveTo ⟨-hw, yA⟩ ⟨-xB, topY⟩ ⟨-lobeX, topY⟩,
    -- Seg 8: left lobe top → cleft
    .curveTo ⟨-xA, topY⟩ ⟨0, yA⟩ ⟨0, waistY⟩,
    .closePath
  ]⟩
  let d : Diagram β := fromPath path fill stroke
  let halfStroke := stroke.width / 2
  match name with
  | none => d
  | some n =>
    withNameAndAnchors d n [
      { name := `north, offset := ⟨0, topY + halfStroke⟩ },
      { name := `south, offset := ⟨0, bottomY - halfStroke⟩ },
      { name := `east, offset := ⟨hw + halfStroke, 0⟩ },
      { name := `west, offset := ⟨-(hw + halfStroke), 0⟩ }
    ]

/--
An n-armed plus/cross shape centered at the origin.

For {lean}`arms ≤ 2`, degenerates to a rectangle. For {lean}`arms ≥ 3`, creates a regular polygon
at the center (with each side = {name}`armWidth`) and rectangular arms extending outward.
Numbered anchors {lean}`` `tip0 ``, {lean}`` `tip1 ``, etc. are placed at each arm tip.
The first arm points north.
-/
def plus (armLength armWidth : Float) (arms : Nat := 4)
    (fill : Fill := default) (stroke : Stroke := {})
    (name : Option Lean.Name := none) : Diagram β :=
  if arms <= 2 then
    let d : Diagram β := fromPath (PathData.rect armLength armWidth) fill stroke
    match name with
    | none => d
    | some n =>
      withNameAndAnchors d n [
        { name := `tip0, offset := ⟨0, armWidth / 2⟩ },
        { name := `tip1, offset := ⟨0, -(armWidth / 2)⟩ }
      ]
  else
    -- Circumradius of a regular polygon with n sides of length armWidth
    let n := arms
    let sideLen := armWidth
    let circumR := sideLen / (2 * Float.sin (pi / n.toFloat))
    let step := 2 * pi / n.toFloat
    -- Build vertices: for each arm, we need 4 vertices (two at polygon edge, two at arm tip)
    let vertices := List.range n |>.flatMap fun i =>
      let θ := pi / 2 + i.toFloat * step  -- center angle of this arm
      let halfStep := step / 2
      let θLeft := θ + halfStep
      let θRight := θ - halfStep
      -- Polygon vertices at this side's endpoints
      let pLeft := Vec2.mk (circumR * Float.cos θLeft) (circumR * Float.sin θLeft)
      let pRight := Vec2.mk (circumR * Float.cos θRight) (circumR * Float.sin θRight)
      -- Arm tip vertices, extending outward from the polygon edge
      let dir := Vec2.mk (Float.cos θ) (Float.sin θ)
      let tipR := Vec2.mk (pRight.x + armLength * dir.x) (pRight.y + armLength * dir.y)
      let tipL := Vec2.mk (pLeft.x + armLength * dir.x) (pLeft.y + armLength * dir.y)
      [pRight, tipR, tipL, pLeft]
    let path := match vertices with
      | [] => PathData.empty
      | p :: rest =>
        let pd := PathData.empty |>.moveTo p
        let pd := rest.foldl (fun acc pt => acc.lineTo pt) pd
        pd.close
    let d : Diagram β := fromPath path fill stroke
    let tipCenters := List.range n |>.map fun i =>
      let θ := pi / 2 + i.toFloat * step
      let dir := Vec2.mk (Float.cos θ) (Float.sin θ)
      let tipDist := circumR + armLength
      Vec2.mk (tipDist * dir.x) (tipDist * dir.y)
    match name with
    | none => d
    | some n' =>
      let anchors := tipCenters.mapIdx fun i p =>
        { name := Lean.Name.mkSimple s!"tip{i}", offset := p : AnchorPos }
      withNameAndAnchors d n' anchors

/--
A stadium (pill/capsule) shape centered at the origin: a rectangle with
fully semicircular ends. This is a convenience wrapper around {name}`Diagram.roundedRect`
with corner radius equal to half the height.
-/
def stadium (width height : Float) (fill : Fill := default) (stroke : Stroke := {})
    (name : Option Lean.Name := none) : Diagram β :=
  Diagram.roundedRect width height (height / 2) fill stroke name

/--
A cylinder (database symbol) centered at the origin, with separate fills for the
top cap and the side body. The top cap is drawn as a full ellipse on top,
creating a 3D appearance.
-/
def cylinder (width height : Float) (capRy : Float := 0)
    (topFill : Fill := default) (sideFill : Fill := default) (stroke : Stroke := {})
    (name : Option Lean.Name := none) : Diagram β :=
  let capRy := if capRy == 0 then height * 0.15 else capRy
  let hw := width / 2
  let hh := height / 2
  let rx := hw
  let topCenterY := hh - capRy
  let bottomCenterY := -hh + capRy
  -- Body path: right side of top ellipse front arc, down, bottom front arc, up
  let bodyPath : PathData := ⟨#[
    .moveTo ⟨-rx, topCenterY⟩,
    -- Front arc of top ellipse (dips down, going right)
    .arcTo rx capRy 0 false true ⟨rx, topCenterY⟩,
    -- Right side down
    .lineTo ⟨rx, bottomCenterY⟩,
    -- Front arc of bottom ellipse (curves down, going left)
    .arcTo rx capRy 0 false false ⟨-rx, bottomCenterY⟩,
    -- Left side up
    .closePath
  ]⟩
  -- Top cap: full ellipse
  let topCapPath : PathData := ⟨#[
    .moveTo ⟨rx, topCenterY⟩,
    .arcTo rx capRy 0 false true ⟨-rx, topCenterY⟩,
    .arcTo rx capRy 0 false true ⟨rx, topCenterY⟩,
    .closePath
  ]⟩
  let body : Diagram β := fromPath bodyPath sideFill stroke
  let topCap : Diagram β := fromPath topCapPath topFill stroke
  let d := Diagram.atop topCap body
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
A cloud shape centered at the origin, made of overlapping circular arcs (puffs)
arranged around an elliptical perimeter.

The {name}`puffSize` parameter (0 to 1) controls how far the puffs bulge outward.
0 means nearly flat, 1 means semicircular (maximum bulge). Default is 0.7.

When a label is provided, the cloud grows to fit it inside the usable interior.
-/
def cloud (width height : Float) (puffs : Nat := 8) (puffSize : Float := 0.85)
    (fill : Fill := default) (stroke : Stroke := {})
    (label : Option (Diagram β) := none)
    (name : Option Lean.Name := none) : Diagram β :=
  let n := max 4 puffs
  let (w, h) := match label with
    | none => (width, height)
    | some lbl =>
      let env := lbl.getEnvelope
      let lw := match env[Vec2.east]? with | some v => v | none => 0
      let lh := match env[Vec2.north]? with | some v => v | none => 0
      -- Cloud interior is roughly 60% of total size
      (max width (2 * lw / 0.6), max height (2 * lh / 0.6))
  let hw := w / 2
  let hh := h / 2
  let step := 2 * pi / n.toFloat
  -- Midpoints between adjacent puff centers on the ellipse
  let midpoints := List.range n |>.map fun i =>
    let θ := i.toFloat * step
    let θNext := (i.toFloat + 1) * step
    let θMid := (θ + θNext) / 2
    Vec2.mk (hw * Float.cos θMid) (hh * Float.sin θMid)
  -- Puff centers on the ellipse
  let centers := List.range n |>.map fun i =>
    let θ := i.toFloat * step
    Vec2.mk (hw * Float.cos θ) (hh * Float.sin θ)
  -- Build the cloud path: arc from each midpoint to the next, bulging outward through the center
  let path := match midpoints with
    | [] => PathData.empty
    | first :: _ =>
      let pd := PathData.empty |>.moveTo first
      let pd := List.range n |>.foldl (fun acc i =>
        let center := centers[i]!
        let nextMid := midpoints[(i + 1) % n]!
        -- Distance from midpoint to center = arc chord half-length
        let mid := midpoints[i]!
        let dx := nextMid.x - mid.x
        let dy := nextMid.y - mid.y
        let chordLen := (dx * dx + dy * dy).sqrt
        -- Bulge radius: puffSize 0→very large (flat), 1→chord/2 (semicircle)
        let ps := max 0.01 (min 1 puffSize)
        let puffRadius := chordLen * (0.5 / ps)
        -- Direction from center to this puff center (outward)
        let cx := center.x
        let cy := center.y
        let dist := (cx * cx + cy * cy).sqrt
        let _ := if dist > 0 then dist else 1
        acc.arcTo puffRadius puffRadius 0 false true nextMid
      ) pd
      pd.close
  let shape : Diagram β := fromPath path fill stroke
  let d := match label with
    | none => shape
    | some lbl => Diagram.atop lbl shape
  let halfStroke := stroke.width / 2
  match name with
  | none => d
  | some n' =>
    withNameAndAnchors d n' [
      { name := `north, offset := ⟨0, hh + halfStroke⟩ },
      { name := `south, offset := ⟨0, -(hh + halfStroke)⟩ },
      { name := `east, offset := ⟨hw + halfStroke, 0⟩ },
      { name := `west, offset := ⟨-(hw + halfStroke), 0⟩ },
      { name := `northeast, offset := ⟨hw * 0.7 + halfStroke, hh * 0.7 + halfStroke⟩ },
      { name := `northwest, offset := ⟨-(hw * 0.7 + halfStroke), hh * 0.7 + halfStroke⟩ },
      { name := `southeast, offset := ⟨hw * 0.7 + halfStroke, -(hh * 0.7 + halfStroke)⟩ },
      { name := `southwest, offset := ⟨-(hw * 0.7 + halfStroke), -(hh * 0.7 + halfStroke)⟩ }
    ]
