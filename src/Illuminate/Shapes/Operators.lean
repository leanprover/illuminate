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

namespace Diagram

/-!
# Math operator shapes

Filled shapes representing common mathematical operators. All take a {lit}`size`
parameter (the height, matching the height of `<` and `>`) and a {lit}`lineWidth`
parameter (the thickness of the filled "strokes").
-/

variable {β : Type} [Backend β]

/-- The width used for single-bar operators (minus, divide) relative to size. -/
private def opBarWidth (size : Float) : Float := size * 0.8

/-- The width used for double-bar operators (equals, not-equals) relative to size. -/
private def opDoubleBarWidth (size : Float) : Float := size * 1.0

/--
A plus operator (+) shape as a single 12-vertex outline path.
-/
def opPlus (size : Float) (lineWidth : Float)
    (fill : Fill := default) (stroke : Stroke := {})
    (name : Option Lean.Name := none) : Diagram β :=
  let hs := size / 2
  let hlw := lineWidth / 2
  let path : PathData := ⟨#[
    .moveTo ⟨-hlw, hs⟩,
    .lineTo ⟨hlw, hs⟩,
    .lineTo ⟨hlw, hlw⟩,
    .lineTo ⟨hs, hlw⟩,
    .lineTo ⟨hs, -hlw⟩,
    .lineTo ⟨hlw, -hlw⟩,
    .lineTo ⟨hlw, -hs⟩,
    .lineTo ⟨-hlw, -hs⟩,
    .lineTo ⟨-hlw, -hlw⟩,
    .lineTo ⟨-hs, -hlw⟩,
    .lineTo ⟨-hs, hlw⟩,
    .lineTo ⟨-hlw, hlw⟩,
    .closePath
  ]⟩
  let d : Diagram β := fromPath path fill stroke
  let halfStroke := stroke.width / 2
  match name with
  | none => d
  | some n =>
    withNameAndAnchors d n [
      { name := `north, offset := ⟨0, hs + halfStroke⟩ },
      { name := `south, offset := ⟨0, -(hs + halfStroke)⟩ },
      { name := `east, offset := ⟨hs + halfStroke, 0⟩ },
      { name := `west, offset := ⟨-(hs + halfStroke), 0⟩ }
    ]

/--
A minus operator (−) shape: a single filled horizontal rectangle.
-/
def opMinus (size : Float) (lineWidth : Float)
    (fill : Fill := default) (stroke : Stroke := {})
    (name : Option Lean.Name := none) : Diagram β :=
  let w := opBarWidth size
  let d : Diagram β := fromPath (PathData.rect w lineWidth) fill stroke
  let halfStroke := stroke.width / 2
  let hw := w / 2
  let hlw := lineWidth / 2
  match name with
  | none => d
  | some n =>
    withNameAndAnchors d n [
      { name := `north, offset := ⟨0, hlw + halfStroke⟩ },
      { name := `south, offset := ⟨0, -(hlw + halfStroke)⟩ },
      { name := `east, offset := ⟨hw + halfStroke, 0⟩ },
      { name := `west, offset := ⟨-(hw + halfStroke), 0⟩ }
    ]

/--
A times/multiply operator (×) shape as a single outline path (plus shape rotated 45°).
-/
def opTimes (size : Float) (lineWidth : Float)
    (fill : Fill := default) (stroke : Stroke := {})
    (name : Option Lean.Name := none) : Diagram β :=
  let s := size * 0.85
  let hs := s / 2
  let hlw := lineWidth / 2
  -- Build a plus-shaped path, then rotate 45°
  let path : PathData := ⟨#[
    .moveTo ⟨-hlw, hs⟩,
    .lineTo ⟨hlw, hs⟩,
    .lineTo ⟨hlw, hlw⟩,
    .lineTo ⟨hs, hlw⟩,
    .lineTo ⟨hs, -hlw⟩,
    .lineTo ⟨hlw, -hlw⟩,
    .lineTo ⟨hlw, -hs⟩,
    .lineTo ⟨-hlw, -hs⟩,
    .lineTo ⟨-hlw, -hlw⟩,
    .lineTo ⟨-hs, -hlw⟩,
    .lineTo ⟨-hs, hlw⟩,
    .lineTo ⟨-hlw, hlw⟩,
    .closePath
  ]⟩
  let d : Diagram β := Diagram.transform (Matrix.rotate (pi / 4)) (fromPath path fill stroke)
  let extent := size / 2
  let halfStroke := stroke.width / 2
  match name with
  | none => d
  | some n =>
    withNameAndAnchors d n [
      { name := `north, offset := ⟨0, extent + halfStroke⟩ },
      { name := `south, offset := ⟨0, -(extent + halfStroke)⟩ },
      { name := `east, offset := ⟨extent + halfStroke, 0⟩ },
      { name := `west, offset := ⟨-(extent + halfStroke), 0⟩ }
    ]

/--
A divide operator (÷) shape: a horizontal bar with a dot above and below.
-/
def opDivide (size : Float) (lineWidth : Float)
    (fill : Fill := default) (stroke : Stroke := {})
    (name : Option Lean.Name := none) : Diagram β :=
  let w := opBarWidth size
  let dotR := lineWidth * 0.55
  let dotGap := lineWidth * 1.7
  let bar : Diagram β := fromPath (PathData.rect w lineWidth) fill stroke
  let topDot : Diagram β := Diagram.transform (Matrix.translate 0 dotGap)
    (fromPath (PathData.circle dotR) fill stroke)
  let bottomDot : Diagram β := Diagram.transform (Matrix.translate 0 (-dotGap))
    (fromPath (PathData.circle dotR) fill stroke)
  let d := Diagram.atop topDot (Diagram.atop bar bottomDot)
  let halfStroke := stroke.width / 2
  let hh := dotGap + dotR
  let hw := w / 2
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
An equals operator (=) shape: two stacked horizontal rectangles.
-/
def opEquals (size : Float) (lineWidth : Float)
    (fill : Fill := default) (stroke : Stroke := {})
    (name : Option Lean.Name := none) : Diagram β :=
  let w := opDoubleBarWidth size
  let gap := lineWidth * 1.2
  let topBar : Diagram β := Diagram.transform (Matrix.translate 0 gap)
    (fromPath (PathData.rect w lineWidth) fill stroke)
  let bottomBar : Diagram β := Diagram.transform (Matrix.translate 0 (-gap))
    (fromPath (PathData.rect w lineWidth) fill stroke)
  let d := Diagram.atop topBar bottomBar
  let halfStroke := stroke.width / 2
  let hh := gap + lineWidth / 2
  let hw := w / 2
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
A not-equals operator (≠) shape as a single closed path tracing the outline
of two horizontal bars with a diagonal slash through them.
-/
def opNotEquals (size : Float) (lineWidth : Float)
    (fill : Fill := default) (stroke : Stroke := {})
    (name : Option Lean.Name := none) : Diagram β :=
  let w := opDoubleBarWidth size
  let hw := w / 2
  let hlw := lineWidth / 2
  let gap := lineWidth * 1.2
  -- Bar y-edges
  let tTop := gap + hlw      -- top of top bar
  let tBot := gap - hlw      -- bottom of top bar
  let bTop := -gap + hlw     -- top of bottom bar
  let bBot := -gap - hlw     -- bottom of bottom bar
  -- Slash geometry: a tilted band going / (lower-left to upper-right)
  let θ := pi / 4 * 0.8
  let tanθ := -(Float.sin θ / Float.cos θ)  -- negated for / direction
  let slashOff := hlw / Float.cos θ   -- x-offset of slash edge from slash center line
  -- Ensure both slash tips protrude past the bars
  let slashH := (gap + hlw) * 3.2
  let halfSH := slashH / 2
  -- Slash edge intersection with a horizontal line at y=Y:
  --   right edge: x = -Y*tanθ + slashOff
  --   left edge:  x = -Y*tanθ - slashOff
  -- Intersection points (right slash edge 'R', left slash edge 'L' at each bar edge)
  let rAtTTop := -tTop * tanθ + slashOff
  let lAtTTop := -tTop * tanθ - slashOff
  let rAtTBot := -tBot * tanθ + slashOff
  let lAtTBot := -tBot * tanθ - slashOff
  let rAtBTop := -bTop * tanθ + slashOff
  let lAtBTop := -bTop * tanθ - slashOff
  let rAtBBot := -bBot * tanθ + slashOff
  let lAtBBot := -bBot * tanθ - slashOff
  -- Slash tips (top tip: upper-right, bottom tip: lower-left for / direction)
  -- Direction d = (sinθ, cosθ), perpendicular p = (cosθ, -sinθ)
  -- Corner = ±halfSH*d ± hlw*p
  let topTipR := Vec2.mk (halfSH * Float.sin θ + hlw * Float.cos θ)
                          (halfSH * Float.cos θ - hlw * Float.sin θ)
  let topTipL := Vec2.mk (halfSH * Float.sin θ - hlw * Float.cos θ)
                          (halfSH * Float.cos θ + hlw * Float.sin θ)
  let botTipR := Vec2.mk (-halfSH * Float.sin θ + hlw * Float.cos θ)
                          (-halfSH * Float.cos θ - hlw * Float.sin θ)
  let botTipL := Vec2.mk (-halfSH * Float.sin θ - hlw * Float.cos θ)
                          (-halfSH * Float.cos θ + hlw * Float.sin θ)
  -- Trace the outline clockwise (in diagram coords)
  let path : PathData := ⟨#[
    -- Start at top-left corner of top bar
    .moveTo ⟨-hw, tTop⟩,
    -- Top of top bar → left slash edge exits
    .lineTo ⟨lAtTTop, tTop⟩,
    -- Up left slash edge to top tip
    .lineTo topTipL,
    .lineTo topTipR,
    -- Down right slash edge to top of top bar
    .lineTo ⟨rAtTTop, tTop⟩,
    -- Continue top of top bar to right end
    .lineTo ⟨hw, tTop⟩,
    -- Down right end of top bar
    .lineTo ⟨hw, tBot⟩,
    -- Bottom of top bar → right slash edge exits into gap
    .lineTo ⟨rAtTBot, tBot⟩,
    -- Down right slash edge through gap to top of bottom bar
    .lineTo ⟨rAtBTop, bTop⟩,
    -- Top of bottom bar to right end
    .lineTo ⟨hw, bTop⟩,
    -- Down right end of bottom bar
    .lineTo ⟨hw, bBot⟩,
    -- Bottom of bottom bar → right slash edge exits
    .lineTo ⟨rAtBBot, bBot⟩,
    -- Down right slash edge to bottom tip
    .lineTo botTipR,
    .lineTo botTipL,
    -- Up left slash edge to bottom of bottom bar
    .lineTo ⟨lAtBBot, bBot⟩,
    -- Bottom of bottom bar to left end
    .lineTo ⟨-hw, bBot⟩,
    -- Up left end of bottom bar
    .lineTo ⟨-hw, bTop⟩,
    -- Top of bottom bar → left slash edge exits into gap
    .lineTo ⟨lAtBTop, bTop⟩,
    -- Up left slash edge through gap to bottom of top bar
    .lineTo ⟨lAtTBot, tBot⟩,
    -- Bottom of top bar to left end
    .lineTo ⟨-hw, tBot⟩,
    -- Up to start
    .closePath
  ]⟩
  let d : Diagram β := fromPath path fill stroke
  let halfStroke := stroke.width / 2
  let hh := gap + hlw
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
A less-than operator (<) shape as a single closed path with 90° ends.
-/
def opLessThan (size : Float) (lineWidth : Float)
    (fill : Fill := default) (stroke : Stroke := {})
    (name : Option Lean.Name := none) : Diagram β :=
  let hs := size / 2
  let w := size * 0.6
  let hlw := lineWidth / 2
  -- Arm geometry: each arm goes from the tip (0,0) to the right end
  let armLen := (w * w + hs * hs).sqrt
  -- Perpendicular offset components (outward normal for upper arm = CW rotation of arm dir)
  let nx := hs * hlw / armLen   -- x-component of perpendicular shift
  let ny := w * hlw / armLen    -- y-component of perpendicular shift
  -- Inner tip: where the two inner edges meet on the x-axis
  let innerTipX := hlw * armLen / hs
  -- Outer tip: where both outer edges meet (mirror of inner tip)
  let outerTipX := -innerTipX
  -- 7 vertices tracing the outline
  let path : PathData := ⟨#[
    -- Outer top-right (upper arm outer edge at right end, 90° cut)
    .moveTo ⟨w - nx, hs + ny⟩,
    -- Sharp outer tip
    .lineTo ⟨outerTipX, 0⟩,
    -- Outer bottom-right (lower arm outer edge at right end, 90° cut)
    .lineTo ⟨w - nx, -(hs + ny)⟩,
    -- Inner bottom-right (lower arm inner edge at right end)
    .lineTo ⟨w + nx, -(hs - ny)⟩,
    -- Inner tip (where both inner edges meet)
    .lineTo ⟨innerTipX, 0⟩,
    -- Inner top-right (upper arm inner edge at right end)
    .lineTo ⟨w + nx, hs - ny⟩,
    .closePath
  ]⟩
  let d : Diagram β := fromPath path fill stroke
  let halfStroke := stroke.width / 2
  -- Center the shape horizontally
  let shift := w / 2
  let d := Diagram.transform (Matrix.translate (-shift) 0) d
  match name with
  | none => d
  | some n =>
    withNameAndAnchors d n [
      { name := `north, offset := ⟨0, hs + ny + halfStroke⟩ },
      { name := `south, offset := ⟨0, -(hs + ny + halfStroke)⟩ },
      { name := `east, offset := ⟨w / 2 + halfStroke, 0⟩ },
      { name := `west, offset := ⟨-(w / 2 + halfStroke), 0⟩ }
    ]

/--
A greater-than operator (>) shape: mirror of less-than.
-/
def opGreaterThan (size : Float) (lineWidth : Float)
    (fill : Fill := default) (stroke : Stroke := {})
    (name : Option Lean.Name := none) : Diagram β :=
  let lt := opLessThan size lineWidth fill stroke name
  Diagram.transform (Matrix.scale (-1) 1) lt
