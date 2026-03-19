/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/

import Illuminate.Geometry
import Illuminate.Style


namespace Illuminate


/-- Reference to an image resource. -/
structure ImageRef where
  /-- File path or URL of the image. -/
  path : String
  /-- Display width in diagram units. -/
  width : Float
  /-- Display height in diagram units. -/
  height : Float
deriving Repr, BEq, Inhabited

/-- A backend-independent primitive that can be rendered by any backend. -/
inductive CorePrimitive where
  /-- Draws a filled and/or stroked path. `none` fields inherit from config. -/
  | path : PathData → Fill → Stroke → CorePrimitive
  /-- Renders a text string. `none` fields inherit style from config. -/
  | text : String → TextStyle → CorePrimitive
  /-- References an external image resource for raster or vector embedding. -/
  | image : ImageRef → CorePrimitive
deriving Repr, BEq, Inhabited

/--
A rendering primitive parameterized by foreign type `β`.
Core primitives are backend-independent. Foreign primitives carry a
backend-specific value of type `β` with an optional core fallback.
-/
inductive Primitive (β : Type) where
  /-- Wraps a backend-independent core primitive. -/
  | core : CorePrimitive → Primitive β
  /-- Wraps a backend-specific foreign value with an optional core fallback. -/
  | foreign : β → Option CorePrimitive → Primitive β
deriving Inhabited

/--
The core diagram type, parameterized by a foreign primitive type `β`.
Backends can embed their own rendering objects alongside built-in primitives
by instantiating `β`. Pure geometric diagrams use `Empty` for this parameter.
-/
inductive Diagram (β : Type) where
  /-- Represents the empty diagram: renders nothing and has a zero envelope. -/
  | empty : Diagram β
  /-- Wraps a single primitive shape (path, text, image, or foreign) as a leaf node. -/
  | prim : Primitive β → Diagram β
  /-- Attaches a numeric tag to a sub-diagram for hit-testing or interactivity. -/
  | annotate : Nat → Diagram β → Diagram β
  /-- Gives a sub-diagram a hierarchical name with cardinal anchor points derived from its envelope. -/
  | named : Lean.Name → Diagram β → Diagram β
  /-- Applies an affine transformation (translate, rotate, scale) to a sub-diagram. -/
  | transform : Matrix → Diagram β → Diagram β
  /-- Overlays two diagrams, sharing the same origin. -/
  | compose : Diagram β → Diagram β → Diagram β
  /-- Overrides the envelope of a sub-diagram without changing its visual content. -/
  | withEnv : Envelope → Diagram β → Diagram β
  /-- Embeds a validation warning message alongside a sub-diagram. -/
  | warning : String → Diagram β → Diagram β
  /-- Applies a cascading draw configuration to a sub-diagram. -/
  | withConfig : DrawConfig → Diagram β → Diagram β
  /-- Wraps a sub-diagram with a given opacity (0–1). -/
  | cellophane : Float → Diagram β → Diagram β
  /-- Clips a sub-diagram to a path boundary. -/
  | clip : PathData → Diagram β → Diagram β

namespace Diagram

-- ═══════════════════════════════════════════════════════════════
-- Smart constructors
-- ═══════════════════════════════════════════════════════════════

variable {β : Type}

/--
Wraps a diagram with a hierarchical name and named anchor points.
Each anchor is placed at the given offset from the origin.
-/
def withNameAndAnchors (d : Diagram β) (n : Lean.Name)
    (anchors : List (Lean.Name × Vec2)) : Diagram β :=
  let withAnchors := anchors.foldl (fun acc (aName, pos) =>
    .compose acc (.transform (Matrix.translate pos.x pos.y) (.named aName .empty))
  ) d
  .named n withAnchors


/-- The empty diagram — renders nothing, has zero envelope. -/
def emptyDiagram : Diagram β := .empty

/-- A filled and/or stroked path. `none` fields inherit from the draw config. -/
def fromPath (pd : PathData) (fill : Fill := {}) (stroke : Stroke := {})
    : Diagram β :=
  .prim (.core (.path pd fill stroke))

/-- A stroked path with no fill. `none` stroke fields inherit from the draw config. -/
def fromStroke (pd : PathData) (stroke : Stroke := {}) : Diagram β :=
  .prim (.core (.path pd { color := Color.transparent } stroke))

/-- A text node with the given style overrides. `none` fields inherit from the draw config. -/
def text (s : String) (style : TextStyle := {})
    (name : Option Lean.Name := none) : Diagram β :=
  let d : Diagram β := .prim (.core (.text s style))
  match name with
  | none => d
  | some n =>
    let fs := style.fontSize.getD 16
    let lines := s.splitOn "\n"
    let totalW := lines.foldl (fun acc line => Max.max acc (estimateTextWidth fs line)) 0
    let lineHeight := fs * 1.2
    let nLines := Max.max 1 lines.length
    let h := if nLines == 1 then fs / 2
             else lineHeight * nLines.toFloat / 2
    let (left, right) : Float × Float := match style.anchor.getD .middle with
      | .start  => (0, totalW)
      | .«end» => (-totalW, 0)
      | .middle => (-totalW / 2, totalW / 2)
    withNameAndAnchors d n [
      (`north, ⟨(left + right) / 2, h⟩),
      (`south, ⟨(left + right) / 2, -h⟩),
      (`east, ⟨right, 0⟩), (`west, ⟨left, 0⟩),
      (`northeast, ⟨right, h⟩), (`northwest, ⟨left, h⟩),
      (`southeast, ⟨right, -h⟩), (`southwest, ⟨left, -h⟩)
    ]

/-- A line segment from `a` to `b`. -/
def line (a b : Vec2) (stroke : Stroke := {}) : Diagram β :=
  fromStroke (PathData.line a b) stroke

/-- A filled rectangle centered at the origin. -/
def rect (width height : Float) (fill : Fill := {}) (stroke : Stroke := {})
    (name : Option Lean.Name := none) : Diagram β :=
  let d : Diagram β := fromPath (PathData.rect width height) fill stroke
  match name with
  | none => d
  | some n =>
    let sw := stroke.width.getD 0 / 2
    let hw := width / 2 + sw
    let hh := height / 2 + sw
    withNameAndAnchors d n [
      (`north, ⟨0, hh⟩), (`south, ⟨0, -hh⟩),
      (`east, ⟨hw, 0⟩), (`west, ⟨-hw, 0⟩),
      (`northeast, ⟨hw, hh⟩), (`northwest, ⟨-hw, hh⟩),
      (`southeast, ⟨hw, -hh⟩), (`southwest, ⟨-hw, -hh⟩)
    ]

/-- A filled rounded rectangle centered at the origin. -/
def roundedRect (width height : Float) (cornerRadius : Float)
    (fill : Fill := {}) (stroke : Stroke := {})
    (name : Option Lean.Name := none) : Diagram β :=
  let d : Diagram β := fromPath (PathData.roundedRect width height cornerRadius) fill stroke
  match name with
  | none => d
  | some n =>
    let sw := stroke.width.getD 0 / 2
    let hw := width / 2 + sw
    let hh := height / 2 + sw
    withNameAndAnchors d n [
      (`north, ⟨0, hh⟩), (`south, ⟨0, -hh⟩),
      (`east, ⟨hw, 0⟩), (`west, ⟨-hw, 0⟩),
      (`northeast, ⟨hw, hh⟩), (`northwest, ⟨-hw, hh⟩),
      (`southeast, ⟨hw, -hh⟩), (`southwest, ⟨-hw, -hh⟩)
    ]

/-- A filled circle centered at the origin. -/
def circle (radius : Float) (fill : Fill := {}) (stroke : Stroke := {})
    (name : Option Lean.Name := none) : Diagram β :=
  let d : Diagram β := .withEnv (Envelope.ofCircle radius)
    (fromPath (PathData.circle radius) fill stroke)
  match name with
  | none => d
  | some n =>
    let sw := stroke.width.getD 0 / 2
    let r := radius + sw
    withNameAndAnchors d n [
      (`north, ⟨0, r⟩), (`south, ⟨0, -r⟩),
      (`east, ⟨r, 0⟩), (`west, ⟨-r, 0⟩)
    ]

/-- A filled ellipse centered at the origin with the given half-widths. -/
def ellipse (rx ry : Float) (fill : Fill := {}) (stroke : Stroke := {})
    (name : Option Lean.Name := none) : Diagram β :=
  let d : Diagram β := .withEnv (Envelope.ofRect rx ry)
    (fromPath (PathData.ellipse rx ry) fill stroke)
  match name with
  | none => d
  | some n =>
    withNameAndAnchors d n [
      (`north, ⟨0, ry⟩), (`south, ⟨0, -ry⟩),
      (`east, ⟨rx, 0⟩), (`west, ⟨-rx, 0⟩)
    ]

/-- A regular polygon centered at the origin with the given number of sides and circumradius.
    If `sides` is less than 3, uses 3 and attaches a validation warning. -/
def polygon (sides : Nat) (radius : Float)
    (fill : Fill := {})
    (stroke : Stroke := {}) : Diagram β :=
  let (n, warn) := if sides < 3 then (3, true) else (sides, false)
  let step := 2 * pi / n.toFloat
  -- First vertex points up (90°)
  let vertices := List.range n |>.map fun i =>
    let θ := pi / 2 + i.toFloat * step
    Vec2.mk (radius * Float.cos θ) (radius * Float.sin θ)
  let path := match vertices with
    | [] => PathData.empty
    | p :: rest =>
      let pd := PathData.empty |>.moveTo p
      let pd := rest.foldl (fun acc pt => acc.lineTo pt) pd
      pd.close
  let d : Diagram β := fromPath path fill stroke
  if warn then .warning s!"polygon: sides={sides} is less than 3, using 3" d
  else d

/--
A star centered at the origin with alternating outer and inner vertices.
The first outer vertex points straight up (north).

Special cases for low point counts:
- 1 point: an equilateral triangle (inner radius is ignored).
- 2 points: a vertical lozenge (diamond) whose half-width equals `innerRadius`.
- 3+ points: a star with `points` outer tips and `points` inner valleys.
-/
def star (points : Nat) (outerRadius innerRadius : Float)
    (fill : Fill := {})
    (stroke : Stroke := {})
    (name : Option Lean.Name := none) : Diagram β :=
  if points == 0 then
    .warning "star: 0 points, rendering nothing" .empty
  else if points == 1 then
    -- Equilateral triangle, first vertex north
    let step := 2 * pi / 3
    let outerPts := List.range 3 |>.map fun i =>
      let θ := pi / 2 + i.toFloat * step
      Vec2.mk (outerRadius * Float.cos θ) (outerRadius * Float.sin θ)
    let d := polygon 3 outerRadius fill stroke
    addPointAnchors d name outerPts
  else if points == 2 then
    -- Vertical lozenge: top/bottom at outerRadius, left/right at innerRadius
    let outerPts : List Vec2 := [⟨0, outerRadius⟩, ⟨0, -outerRadius⟩]
    let path := PathData.empty
      |>.moveTo ⟨0, outerRadius⟩
      |>.lineTo ⟨innerRadius, 0⟩
      |>.lineTo ⟨0, -outerRadius⟩
      |>.lineTo ⟨-innerRadius, 0⟩
      |>.close
    let d := fromPath path fill stroke
    addPointAnchors d name outerPts
  else
    -- General star: alternate outer/inner vertices
    let n := points
    let step := pi / n.toFloat  -- half-step between outer and inner
    let vertices := List.range (2 * n) |>.map fun i =>
      let r := if i % 2 == 0 then outerRadius else innerRadius
      let θ := pi / 2 + i.toFloat * step
      Vec2.mk (r * Float.cos θ) (r * Float.sin θ)
    let outerPts := List.range n |>.map fun i =>
      let θ := pi / 2 + (2 * i).toFloat * step
      Vec2.mk (outerRadius * Float.cos θ) (outerRadius * Float.sin θ)
    let path := match vertices with
      | [] => PathData.empty
      | p :: rest =>
        let pd := PathData.empty |>.moveTo p
        let pd := rest.foldl (fun acc pt => acc.lineTo pt) pd
        pd.close
    let d := fromPath path fill stroke
    addPointAnchors d name outerPts
where
  /-- Optionally names a diagram and attaches `point0`, `point1`, … anchors. -/
  addPointAnchors (d : Diagram β) (name : Option Lean.Name) (pts : List Vec2)
      : Diagram β :=
    match name with
    | none => d
    | some n =>
      let anchors := pts.mapIdx fun i p =>
        (Lean.Name.mkSimple s!"point{i}", p)
      withNameAndAnchors d n anchors

/--
A horizontal curly brace centered at the origin, pointing downward.
The brace spans `width` horizontally and extends `depth` downward to a central tip.
Optionally attach a `label` diagram below the tip.
-/
def curlyBrace (width : Float) (depth : Float := 0)
    (roundness : Float := 0.25)
    (stroke : Stroke := { color := Color.black, width := (1 : Float) })
    (label : Option (Diagram β) := none) : Diagram β :=
  let depth := if depth > 0 then depth else max 10 (width * 0.08)
  let hw := width / 2
  let mid := depth / 2
  -- Quarter-circle bend radius, capped so bends don't overlap
  let r := min mid (hw * roundness)
  -- Shape: vertical drop at ends → horizontal shoulder → vertical drop to center tip
  -- Left end vertical drop + bend into horizontal
  let path := PathData.empty
    |>.moveTo ⟨-hw, 0⟩
    |>.lineTo ⟨-hw, -(mid - r)⟩
    |>.curveTo ⟨-hw, -mid⟩ ⟨-hw + r, -mid⟩ ⟨-hw + r, -mid⟩
    -- Left horizontal shoulder
    |>.lineTo ⟨-r, -mid⟩
    -- Bend down into center tip
    |>.curveTo ⟨0, -mid⟩ ⟨0, -mid⟩ ⟨0, -depth⟩
    -- Bend up from center tip into right shoulder
    |>.curveTo ⟨0, -mid⟩ ⟨0, -mid⟩ ⟨r, -mid⟩
    -- Right horizontal shoulder
    |>.lineTo ⟨hw - r, -mid⟩
    -- Bend up into right end vertical
    |>.curveTo ⟨hw, -mid⟩ ⟨hw, -mid⟩ ⟨hw, -(mid - r)⟩
    |>.lineTo ⟨hw, 0⟩
  let brace : Diagram β := fromStroke path stroke
  match label with
  | none => brace
  | some lbl =>
    let labelOffset := depth + 10
    Diagram.compose brace (Diagram.transform (Matrix.translate 0 (-labelOffset)) lbl)

/-- An image primitive. -/
def fromImage (ref : ImageRef) : Diagram β :=
  .prim (.core (.image ref))

/-- A foreign primitive with optional core fallback. -/
def fromForeign (val : β) (fallback : Option CorePrimitive := none) : Diagram β :=
  .prim (.foreign val fallback)

-- ═══════════════════════════════════════════════════════════════
-- Per-field config override helpers
-- ═══════════════════════════════════════════════════════════════

/-- Sets the stroke color for a sub-diagram. -/
def withStrokeColor (c : Color) (d : Diagram β) : Diagram β :=
  .withConfig { strokeColor := .set c } d

/-- Sets the stroke width for a sub-diagram. -/
def withStrokeWidth (w : Float) (d : Diagram β) : Diagram β :=
  .withConfig { strokeWidth := .set w } d

/-- Sets the fill color for a sub-diagram. -/
def withFillColor (c : Color) (d : Diagram β) : Diagram β :=
  .withConfig { fillColor := .set c } d

/-- Sets the font size for a sub-diagram. -/
def withFontSize (s : Float) (d : Diagram β) : Diagram β :=
  .withConfig { fontSize := .set s } d

/-- Sets the font family for a sub-diagram. -/
def withFontFamily (f : String) (d : Diagram β) : Diagram β :=
  .withConfig { fontFamily := .set f } d

/-- Sets the bold flag for a sub-diagram. -/
def withFontBold (b : Bool) (d : Diagram β) : Diagram β :=
  .withConfig { fontBold := .set b } d

/-- Sets the italic flag for a sub-diagram. -/
def withFontItalic (i : Bool) (d : Diagram β) : Diagram β :=
  .withConfig { fontItalic := .set i } d

/-- Sets the text color for a sub-diagram. -/
def withTextColor (c : Color) (d : Diagram β) : Diagram β :=
  .withConfig { textColor := .set c } d

/-- Sets the default arrowhead style for a sub-diagram. -/
def withArrowhead (ah : Arrowhead) (d : Diagram β) : Diagram β :=
  .withConfig { arrowhead := .set (some ah) } d

/-- Sets the stroke line cap for a sub-diagram. -/
def withStrokeLineCap (cap : LineCap) (d : Diagram β) : Diagram β :=
  .withConfig { strokeLineCap := .set cap } d

/-- Sets the stroke line join for a sub-diagram. -/
def withStrokeLineJoin (join : LineJoin) (d : Diagram β) : Diagram β :=
  .withConfig { strokeLineJoin := .set join } d

-- ═══════════════════════════════════════════════════════════════
-- Per-field config reset helpers
-- ═══════════════════════════════════════════════════════════════

/-- Resets the stroke color to the global default for a sub-diagram. -/
def resetStrokeColor (d : Diagram β) : Diagram β :=
  .withConfig { strokeColor := .reset } d

/-- Resets the stroke width to the global default for a sub-diagram. -/
def resetStrokeWidth (d : Diagram β) : Diagram β :=
  .withConfig { strokeWidth := .reset } d

/-- Resets the fill color to the global default for a sub-diagram. -/
def resetFillColor (d : Diagram β) : Diagram β :=
  .withConfig { fillColor := .reset } d

/-- Resets the font size to the global default for a sub-diagram. -/
def resetFontSize (d : Diagram β) : Diagram β :=
  .withConfig { fontSize := .reset } d

/-- Resets the font family to the global default for a sub-diagram. -/
def resetFontFamily (d : Diagram β) : Diagram β :=
  .withConfig { fontFamily := .reset } d

/-- Resets the bold flag to the global default for a sub-diagram. -/
def resetFontBold (d : Diagram β) : Diagram β :=
  .withConfig { fontBold := .reset } d

/-- Resets the italic flag to the global default for a sub-diagram. -/
def resetFontItalic (d : Diagram β) : Diagram β :=
  .withConfig { fontItalic := .reset } d

/-- Resets the text color to the global default for a sub-diagram. -/
def resetTextColor (d : Diagram β) : Diagram β :=
  .withConfig { textColor := .reset } d

/-- Resets the arrowhead to the global default for a sub-diagram. -/
def resetArrowhead (d : Diagram β) : Diagram β :=
  .withConfig { arrowhead := .reset } d

/-- Sets arrow labels to stay upright (not rotate to follow the arrow) for a sub-diagram. -/
def withLabelUpright (d : Diagram β) : Diagram β :=
  .withConfig { labelUpright := .set true } d

/-- Resets the label upright flag to the global default for a sub-diagram. -/
def resetLabelUpright (d : Diagram β) : Diagram β :=
  .withConfig { labelUpright := .reset } d

/-- Resets all config fields to their global defaults for a sub-diagram. -/
def resetConfig (d : Diagram β) : Diagram β :=
  .withConfig DrawConfig.resetAll d
