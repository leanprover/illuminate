/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/
module
public import Illuminate.Diagram.Types
public import Illuminate.Style.Color
import Illuminate.Diagram.Placement
import Lean.DocString.Syntax
public section

namespace Illuminate

/--
Cardinal direction for positioning bubble tails.
-/
inductive TailSide where
  /-- Tail extends downward from the bottom edge. -/
  | south
  /-- Tail extends upward from the top edge. -/
  | north
  /-- Tail extends rightward from the right edge. -/
  | east
  /-- Tail extends leftward from the left edge. -/
  | west
deriving Repr, BEq, Inhabited

namespace Diagram

/-!
# Bubble shapes

Speech bubbles, thought bubbles, and operators for placing them over diagram anchors.
-/

variable {β : Type} [Backend β]

/--
A speech bubble centered at the origin: a rounded rectangle with a triangular tail.

The {name}`tailPosition` parameter (0 to 1) controls where along the edge the tail
attaches, and {name}`tailSide` controls which edge the tail extends from.
-/
def speechBubble (width height : Float) (cornerRadius : Float := 0)
    (tailWidth : Float := 0) (tailHeight : Float := 0)
    (tailPosition : Float := 0.3) (tailSide : TailSide := .south)
    (fill : Fill := default) (stroke : Stroke := {})
    (name : Option Lean.Name := none) : Diagram β :=
  let cr := if cornerRadius == 0 then min (width / 4) (height / 4) else cornerRadius
  let tw := if tailWidth == 0 then width * 0.15 else tailWidth
  let th := if tailHeight == 0 then height * 0.3 else tailHeight
  let hw := width / 2
  let hh := height / 2
  let r := min cr (min hw hh)
  -- Clamp tail position
  let tp := max 0.05 (min 0.95 tailPosition)
  -- Build path based on tail side
  let path := match tailSide with
  | .south =>
    let tailX := -hw + r + tp * (width - 2 * r)
    PathData.empty
      -- Start at top-left after corner
      |>.moveTo ⟨-hw + r, hh⟩
      -- Top edge
      |>.lineTo ⟨hw - r, hh⟩
      |>.arcTo r r 0 false false ⟨hw, hh - r⟩
      -- Right edge
      |>.lineTo ⟨hw, -hh + r⟩
      |>.arcTo r r 0 false false ⟨hw - r, -hh⟩
      -- Bottom edge with tail
      |>.lineTo ⟨tailX + tw / 2, -hh⟩
      |>.lineTo ⟨tailX, -hh - th⟩
      |>.lineTo ⟨tailX - tw / 2, -hh⟩
      -- Continue bottom edge
      |>.lineTo ⟨-hw + r, -hh⟩
      |>.arcTo r r 0 false false ⟨-hw, -hh + r⟩
      -- Left edge
      |>.lineTo ⟨-hw, hh - r⟩
      |>.arcTo r r 0 false false ⟨-hw + r, hh⟩
      |>.close
  | .north =>
    let tailX := -hw + r + tp * (width - 2 * r)
    PathData.empty
      |>.moveTo ⟨-hw + r, hh⟩
      -- Top edge with tail
      |>.lineTo ⟨tailX - tw / 2, hh⟩
      |>.lineTo ⟨tailX, hh + th⟩
      |>.lineTo ⟨tailX + tw / 2, hh⟩
      |>.lineTo ⟨hw - r, hh⟩
      |>.arcTo r r 0 false false ⟨hw, hh - r⟩
      -- Right edge
      |>.lineTo ⟨hw, -hh + r⟩
      |>.arcTo r r 0 false false ⟨hw - r, -hh⟩
      -- Bottom edge
      |>.lineTo ⟨-hw + r, -hh⟩
      |>.arcTo r r 0 false false ⟨-hw, -hh + r⟩
      -- Left edge
      |>.lineTo ⟨-hw, hh - r⟩
      |>.arcTo r r 0 false false ⟨-hw + r, hh⟩
      |>.close
  | .east =>
    let tailY := -hh + r + tp * (height - 2 * r)
    PathData.empty
      |>.moveTo ⟨-hw + r, hh⟩
      |>.lineTo ⟨hw - r, hh⟩
      |>.arcTo r r 0 false false ⟨hw, hh - r⟩
      -- Right edge with tail
      |>.lineTo ⟨hw, tailY + tw / 2⟩
      |>.lineTo ⟨hw + th, tailY⟩
      |>.lineTo ⟨hw, tailY - tw / 2⟩
      |>.lineTo ⟨hw, -hh + r⟩
      |>.arcTo r r 0 false false ⟨hw - r, -hh⟩
      |>.lineTo ⟨-hw + r, -hh⟩
      |>.arcTo r r 0 false false ⟨-hw, -hh + r⟩
      |>.lineTo ⟨-hw, hh - r⟩
      |>.arcTo r r 0 false false ⟨-hw + r, hh⟩
      |>.close
  | .west =>
    let tailY := -hh + r + tp * (height - 2 * r)
    PathData.empty
      |>.moveTo ⟨-hw + r, hh⟩
      |>.lineTo ⟨hw - r, hh⟩
      |>.arcTo r r 0 false false ⟨hw, hh - r⟩
      |>.lineTo ⟨hw, -hh + r⟩
      |>.arcTo r r 0 false false ⟨hw - r, -hh⟩
      |>.lineTo ⟨-hw + r, -hh⟩
      |>.arcTo r r 0 false false ⟨-hw, -hh + r⟩
      -- Left edge with tail
      |>.lineTo ⟨-hw, tailY - tw / 2⟩
      |>.lineTo ⟨-hw - th, tailY⟩
      |>.lineTo ⟨-hw, tailY + tw / 2⟩
      |>.lineTo ⟨-hw, hh - r⟩
      |>.arcTo r r 0 false false ⟨-hw + r, hh⟩
      |>.close
  let d : Diagram β := fromPath path fill stroke
  let halfStroke := stroke.width / 2
  let tailTipOffset := match tailSide with
    | .south =>
      let tailX := -hw + r + tp * (width - 2 * r)
      Vec2.mk tailX (-(hh + th + halfStroke))
    | .north =>
      let tailX := -hw + r + tp * (width - 2 * r)
      Vec2.mk tailX (hh + th + halfStroke)
    | .east =>
      let tailY := -hh + r + tp * (height - 2 * r)
      Vec2.mk (hw + th + halfStroke) tailY
    | .west =>
      let tailY := -hh + r + tp * (height - 2 * r)
      Vec2.mk (-(hw + th + halfStroke)) tailY
  match name with
  | none => d
  | some n =>
    withNameAndAnchors d n [
      { name := `north, offset := ⟨0, hh + halfStroke⟩ },
      { name := `south, offset := ⟨0, -(hh + halfStroke)⟩ },
      { name := `east, offset := ⟨hw + halfStroke, 0⟩ },
      { name := `west, offset := ⟨-(hw + halfStroke), 0⟩ },
      { name := `northeast, offset := ⟨hw + halfStroke, hh + halfStroke⟩ },
      { name := `northwest, offset := ⟨-(hw + halfStroke), hh + halfStroke⟩ },
      { name := `southeast, offset := ⟨hw + halfStroke, -(hh + halfStroke)⟩ },
      { name := `southwest, offset := ⟨-(hw + halfStroke), -(hh + halfStroke)⟩ },
      { name := `tailTip, offset := tailTipOffset }
    ]

/--
A thought bubble centered at the origin: an elliptical body with trailing
circles that decrease in size toward the tail direction.
-/
def thoughtBubble (width height : Float)
    (tailSide : TailSide := .south)
    (fill : Fill := default) (stroke : Stroke := {})
    (name : Option Lean.Name := none) : Diagram β :=
  let hw := width / 2
  let hh := height / 2
  -- Main body: an ellipse
  let body : Diagram β := Diagram.ellipse hw hh fill stroke
  -- Trailing circles: 3 progressively smaller circles
  let (dir, perpOffset) := match tailSide with
    | .south => (Vec2.mk 0 (-1), Vec2.mk (-1) 0)
    | .north => (Vec2.mk 0 1, Vec2.mk 1 0)
    | .east => (Vec2.mk 1 0, Vec2.mk 0 (-1))
    | .west => (Vec2.mk (-1) 0, Vec2.mk 0 1)
  let baseR := min hw hh * 0.15
  let baseDist := (match tailSide with
    | .south | .north => hh
    | .east | .west => hw) + baseR * 1.5
  let circles := List.range 3 |>.map fun i =>
    let scale := 1.0 - i.toFloat * 0.3
    let r := baseR * scale
    let dist := baseDist + i.toFloat * baseR * 2.0
    let wobble := if i == 1 then perpOffset.x * baseR * 0.5 else 0
    let wobbleY := if i == 1 then perpOffset.y * baseR * 0.5 else 0
    let cx := dir.x * dist + wobble
    let cy := dir.y * dist + wobbleY
    Diagram.transform (Matrix.translate cx cy) (Diagram.circle r fill stroke : Diagram β)
  let trailingDots := circles.foldl (fun acc c => Diagram.atop c acc) (Diagram.emptyDiagram : Diagram β)
  let d := Diagram.atop body trailingDots
  let tailDist := baseDist + 2 * baseR * 2.0
  let tailTipOffset := Vec2.mk (dir.x * tailDist) (dir.y * tailDist)
  let halfStroke := stroke.width / 2
  match name with
  | none => d
  | some n =>
    withNameAndAnchors d n [
      { name := `north, offset := ⟨0, hh + halfStroke⟩ },
      { name := `south, offset := ⟨0, -(hh + halfStroke)⟩ },
      { name := `east, offset := ⟨hw + halfStroke, 0⟩ },
      { name := `west, offset := ⟨-(hw + halfStroke), 0⟩ },
      { name := `northeast, offset := ⟨hw + halfStroke, hh + halfStroke⟩ },
      { name := `northwest, offset := ⟨-(hw + halfStroke), hh + halfStroke⟩ },
      { name := `southeast, offset := ⟨hw + halfStroke, -(hh + halfStroke)⟩ },
      { name := `southwest, offset := ⟨-(hw + halfStroke), -(hh + halfStroke)⟩ },
      { name := `tailTip, offset := tailTipOffset }
    ]

/--
Picks the best tail side and position for a bubble at {name}`bubbleCenter` pointing
at {name}`target`. Returns the side, tail position (0–1), and the offset from the
target to the bubble center. If the target is inside the bubble, returns {lean}`none`.
-/
private def pickTailPlacement (hw hh : Float) (bubbleCenter target : Vec2) :
    Option (TailSide × Float) :=
  let dx := target.x - bubbleCenter.x
  let dy := target.y - bubbleCenter.y
  -- Target is inside the bubble body — no tail
  if dx.abs < hw && dy.abs < hh then none
  else
    -- Pick the side closest to the target direction
    let ratioX := if hw > 0 then dx.abs / hw else 0
    let ratioY := if hh > 0 then dy.abs / hh else 0
    let (side, edgeLen, along) :=
      if ratioX > ratioY then
        -- Horizontal dominates: tail on east/west edge, positioned along height
        if dx > 0 then (TailSide.east, hh, dy)
        else (TailSide.west, hh, dy)
      else
        -- Vertical dominates: tail on north/south edge, positioned along width
        if dy > 0 then (TailSide.north, hw, dx)
        else (TailSide.south, hw, dx)
    -- Compute tail position (0–1) along that edge
    let tp := if edgeLen > 0 then 0.5 + along / (2 * edgeLen) else 0.5
    let tp := max 0.05 (min 0.95 tp)
    some (side, tp)

/--
Places a speech bubble pointing at a named anchor in an existing diagram.
The tail side and position are computed automatically to point at the anchor.

The {name}`position` sets the bubble center as a translation relative to the
diagram's origin.

If the anchor is inside the bubble body, a warning is emitted.
-/
def addSpeechBubble (d : Diagram β) (anchorName : Lean.Name)
    (content : Diagram β) (width height : Float)
    (position : Vec2 := ⟨0, 0⟩)
    (cornerRadius : Float := 0)
    (tailWidth : Float := 0)
    (fill : Fill := default) (stroke : Stroke := {})
    (bubbleName : Option Lean.Name := none) : Diagram β :=
  let anchorPos := (d.find anchorName).origin.toVec2
  let hw := width / 2
  let hh := height / 2
  let cr := if cornerRadius == 0 then min (width / 4) (height / 4) else cornerRadius
  let r := min cr (min hw hh)
  let tw := if tailWidth == 0 then min width height * 0.15 else tailWidth
  let bubbleCenter := position
  -- Anchor position relative to bubble center
  let relX := anchorPos.x - bubbleCenter.x
  let relY := anchorPos.y - bubbleCenter.y
  if relX.abs < hw && relY.abs < hh then
    -- Anchor is inside the bubble
    let bubble := Diagram.roundedRect width height r fill stroke bubbleName
    let bubbleWithContent := Diagram.atop content bubble
    let positioned := Diagram.transform (Matrix.translate bubbleCenter.x bubbleCenter.y)
      bubbleWithContent
    Diagram.atop (.warning s!"addSpeechBubble: anchor '{anchorName}' is inside the bubble" positioned) d
  else
    -- Build a single closed path: rounded rect with tail triangle integrated
    let ratioX := if hw > 0 then relX.abs / hw else 0
    let ratioY := if hh > 0 then relY.abs / hh else 0
    let halfTW := tw / 2
    let tip := Vec2.mk relX relY
    -- Determine which edge the tail exits from and build the path
    -- The path traces the rounded rect, inserting the tail detour on the correct edge
    let path :=
      if ratioX > ratioY then
        -- Tail on east or west edge
        let onEast := relX > 0
        let clampedY := max (-hh + r + halfTW) (min (hh - r - halfTW) relY)
        let b1y := clampedY + halfTW
        let b2y := clampedY - halfTW
        if onEast then
          -- Insert tail on the right edge (between top-right and bottom-right corners)
          PathData.empty
            |>.moveTo ⟨-hw + r, hh⟩
            |>.lineTo ⟨hw - r, hh⟩
            |>.arcTo r r 0 false false ⟨hw, hh - r⟩
            |>.lineTo ⟨hw, b1y⟩ |>.lineTo tip |>.lineTo ⟨hw, b2y⟩
            |>.lineTo ⟨hw, -hh + r⟩
            |>.arcTo r r 0 false false ⟨hw - r, -hh⟩
            |>.lineTo ⟨-hw + r, -hh⟩
            |>.arcTo r r 0 false false ⟨-hw, -hh + r⟩
            |>.lineTo ⟨-hw, hh - r⟩
            |>.arcTo r r 0 false false ⟨-hw + r, hh⟩
            |>.close
        else
          -- Insert tail on the left edge (between bottom-left and top-left corners)
          PathData.empty
            |>.moveTo ⟨-hw + r, hh⟩
            |>.lineTo ⟨hw - r, hh⟩
            |>.arcTo r r 0 false false ⟨hw, hh - r⟩
            |>.lineTo ⟨hw, -hh + r⟩
            |>.arcTo r r 0 false false ⟨hw - r, -hh⟩
            |>.lineTo ⟨-hw + r, -hh⟩
            |>.arcTo r r 0 false false ⟨-hw, -hh + r⟩
            |>.lineTo ⟨-hw, b2y⟩ |>.lineTo tip |>.lineTo ⟨-hw, b1y⟩
            |>.lineTo ⟨-hw, hh - r⟩
            |>.arcTo r r 0 false false ⟨-hw + r, hh⟩
            |>.close
      else
        -- Tail on north or south edge
        let onSouth := relY < 0
        let clampedX := max (-hw + r + halfTW) (min (hw - r - halfTW) relX)
        let b1x := clampedX + halfTW
        let b2x := clampedX - halfTW
        if onSouth then
          -- Insert tail on the bottom edge
          PathData.empty
            |>.moveTo ⟨-hw + r, hh⟩
            |>.lineTo ⟨hw - r, hh⟩
            |>.arcTo r r 0 false false ⟨hw, hh - r⟩
            |>.lineTo ⟨hw, -hh + r⟩
            |>.arcTo r r 0 false false ⟨hw - r, -hh⟩
            |>.lineTo ⟨b1x, -hh⟩ |>.lineTo tip |>.lineTo ⟨b2x, -hh⟩
            |>.lineTo ⟨-hw + r, -hh⟩
            |>.arcTo r r 0 false false ⟨-hw, -hh + r⟩
            |>.lineTo ⟨-hw, hh - r⟩
            |>.arcTo r r 0 false false ⟨-hw + r, hh⟩
            |>.close
        else
          -- Insert tail on the top edge
          PathData.empty
            |>.moveTo ⟨-hw + r, hh⟩
            |>.lineTo ⟨b2x, hh⟩ |>.lineTo tip |>.lineTo ⟨b1x, hh⟩
            |>.lineTo ⟨hw - r, hh⟩
            |>.arcTo r r 0 false false ⟨hw, hh - r⟩
            |>.lineTo ⟨hw, -hh + r⟩
            |>.arcTo r r 0 false false ⟨hw - r, -hh⟩
            |>.lineTo ⟨-hw + r, -hh⟩
            |>.arcTo r r 0 false false ⟨-hw, -hh + r⟩
            |>.lineTo ⟨-hw, hh - r⟩
            |>.arcTo r r 0 false false ⟨-hw + r, hh⟩
            |>.close
    let bubble : Diagram β := fromPath path fill stroke
    let named := match bubbleName with
      | some n =>
        let halfStroke := stroke.width / 2
        withNameAndAnchors bubble n [
          { name := `north, offset := ⟨0, hh + halfStroke⟩ },
          { name := `south, offset := ⟨0, -(hh + halfStroke)⟩ },
          { name := `east, offset := ⟨hw + halfStroke, 0⟩ },
          { name := `west, offset := ⟨-(hw + halfStroke), 0⟩ },
          { name := `tailTip, offset := tip }
        ]
      | none => bubble
    let bubbleWithContent := Diagram.atop content named
    let positioned := Diagram.transform (Matrix.translate bubbleCenter.x bubbleCenter.y)
      bubbleWithContent
    Diagram.atop positioned d

/--
Places a thought bubble pointing at a named anchor in an existing diagram.
The tail side is computed automatically to point at the anchor.

The {name}`position` sets the bubble center as a translation relative to the
diagram's origin.

If the anchor is inside the bubble body, a warning is emitted.
-/
def addThoughtBubble (d : Diagram β) (anchorName : Lean.Name)
    (content : Diagram β) (width height : Float)
    (position : Vec2 := ⟨0, 0⟩)
    (fill : Fill := default) (stroke : Stroke := {})
    (bubbleName : Option Lean.Name := none) : Diagram β :=
  let anchorPos := (d.find anchorName).origin.toVec2
  let hw := width / 2
  let hh := height / 2
  let bubbleCenter := position
  let relX := anchorPos.x - bubbleCenter.x
  let relY := anchorPos.y - bubbleCenter.y
  let adx := relX.abs
  let ady := relY.abs
  if adx < hw && ady < hh then
    -- Anchor is inside the bubble
    let body : Diagram β := Diagram.ellipse hw hh fill stroke bubbleName
    let bubbleWithContent := Diagram.atop content body
    let positioned := Diagram.transform (Matrix.translate bubbleCenter.x bubbleCenter.y)
      bubbleWithContent
    Diagram.atop (.warning s!"addThoughtBubble: anchor '{anchorName}' is inside the bubble" positioned) d
  else
    -- Build ellipse body at bubble center
    let body : Diagram β := Diagram.ellipse hw hh fill stroke bubbleName
    -- Trailing circles from bubble edge toward anchor
    let dist := (relX * relX + relY * relY).sqrt
    let dirX := relX / dist
    let dirY := relY / dist
    -- Start circles from the bubble edge
    let edgeDist := match (if ady * hw >= adx * hh then true else false) with
      | true => hh
      | false => hw
    let baseR := min hw hh * 0.12
    let gap := dist - edgeDist
    let circles := List.range 3 |>.map fun i =>
      let t := (i.toFloat + 1) / 4.0  -- 0.25, 0.5, 0.75 along the gap
      let scale := 1.0 - i.toFloat * 0.3
      let r := baseR * scale
      let cx := dirX * (edgeDist + t * gap)
      let cy := dirY * (edgeDist + t * gap)
      Diagram.transform (Matrix.translate cx cy) (Diagram.circle r fill stroke : Diagram β)
    let trailingDots := circles.foldl (fun acc c => Diagram.atop c acc) (Diagram.emptyDiagram : Diagram β)
    let combined := Diagram.atop body trailingDots
    let bubbleWithContent := Diagram.atop content combined
    let positioned := Diagram.transform (Matrix.translate bubbleCenter.x bubbleCenter.y)
      bubbleWithContent
    Diagram.atop positioned d
