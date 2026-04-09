/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/
module
public import Illuminate.Diagram.Types
import Lean.DocString.Syntax
import Illuminate.Geometry.Basic
import Illuminate.Geometry.Envelope
import Illuminate.Geometry.Matrix
import Illuminate.Geometry.PathData
public import Illuminate.Style.Color
import Illuminate.Style.Text
public section

namespace Illuminate

instance : Coe Lean.Name LineEnd where
  coe point := { point }

instance : Backend Empty where
  envelope e _ := nomatch e
  trace e _ := nomatch e
  strokeTrace e _ := nomatch e
  compile e _ := nomatch e

namespace Diagram

/-!
# Smart constructors
-/

variable {β : Type}



/--
Wraps a diagram with a hierarchical name and named anchor points.
Each anchor is placed at the given diagram-unit offset.
-/
def withNameAndAnchors (d : Diagram β) (n : Lean.Name)
    (anchors : List AnchorPos) : Diagram β :=
  let withAnchors := anchors.foldl (fun acc a =>
    let anchor : Diagram β := .named a.name .empty
    let translated := .transform (Matrix.translate a.offset.x a.offset.y) anchor
    .compose acc translated
  ) d
  .named n withAnchors


/-- The empty diagram — renders nothing, has zero envelope. -/
def emptyDiagram : Diagram β := .empty

/-- A filled and/or stroked path. -/
def fromPath (pd : PathData) (fill : Fill := default) (stroke : Stroke := {}) :
    Diagram β :=
  .prim (.path pd fill stroke)

/-- A stroked path with no fill. -/
def fromStroke (pd : PathData) (stroke : Stroke := {}) : Diagram β :=
  .prim (.path pd .none stroke)

/-- A text node with the given style. -/
def text (s : String) (style : TextStyle := {})
    (name : Option Lean.Name := none) : Diagram β :=
  let d : Diagram β := .prim (.text s style)
  match name with
  | none => d
  | some n =>
    let fs := style.fontSize
    let lines := s.splitOn "\n"
    let totalW := lines.foldl (fun acc line => Max.max acc (estimateTextWidth fs line)) 0
    let lineHeight := fs * 1.2
    let nLines := Max.max 1 lines.length
    let h := if nLines == 1 then fs / 2
             else lineHeight * nLines.toFloat / 2
    let (left, right) : Float × Float := match style.anchor with
      | .start => (0, totalW)
      | .«end» => (-totalW, 0)
      | .middle => (-totalW / 2, totalW / 2)
    withNameAndAnchors d n [
      { name := `north, offset := ⟨(left + right) / 2, h⟩ },
      { name := `south, offset := ⟨(left + right) / 2, -h⟩ },
      { name := `east, offset := ⟨right, 0⟩ },
      { name := `west, offset := ⟨left, 0⟩ },
      { name := `northeast, offset := ⟨right, h⟩ },
      { name := `northwest, offset := ⟨left, h⟩ },
      { name := `southeast, offset := ⟨right, -h⟩ },
      { name := `southwest, offset := ⟨left, -h⟩ }
    ]

/--
Multi-line styled text with per-segment font and color.

Each inner list is a line of styled segments. The {name}`anchor` controls
horizontal alignment of the whole text block.
-/
def styledLines (lines : List (List (FontStyle × String)))
    (anchor : TextAnchor := .middle)
    (name : Option Lean.Name := none) : Diagram β :=
  let linesArr := lines.map (·.toArray) |>.toArray
  let d : Diagram β := .prim (.styledText linesArr anchor)
  match name with
  | none => d
  | some n =>
    let maxW := linesArr.foldl (fun acc segs => max acc (estimateLineWidth segs)) 0
    let nLines := max 1 linesArr.size
    let maxFs := linesArr.foldl (fun acc segs => max acc (lineMaxFontSize segs)) 0
    let h := if nLines == 1 then maxFs / 2
             else maxFs * 1.2 * nLines.toFloat / 2
    let (left, right) : Float × Float := match anchor with
      | .start => (0, maxW)
      | .«end» => (-maxW, 0)
      | .middle => (-maxW / 2, maxW / 2)
    withNameAndAnchors d n [
      { name := `north, offset := ⟨(left + right) / 2, h⟩ },
      { name := `south, offset := ⟨(left + right) / 2, -h⟩ },
      { name := `east, offset := ⟨right, 0⟩ },
      { name := `west, offset := ⟨left, 0⟩ },
      { name := `northeast, offset := ⟨right, h⟩ },
      { name := `northwest, offset := ⟨left, h⟩ },
      { name := `southeast, offset := ⟨right, -h⟩ },
      { name := `southwest, offset := ⟨left, -h⟩ }
    ]

/-- Styled text with an ambient base style. Bare strings inherit the base; use {name}`bold`, {name}`italic`, {name}`family`, or {name}`styled` to modify it for inner fragments. -/
def styledText (text : StyledText)
    (base : FontStyle := {})
    (anchor : TextAnchor := .middle)
    (name : Option Lean.Name := none) : Diagram β :=
  styledLines (text.toLines base) anchor name

/-- A line segment from {name}`a` to {name}`b`. -/
def line (a b : Vec2) (stroke : Stroke := {}) : Diagram β :=
  fromStroke (PathData.line a b) stroke

/-- A filled rectangle centered at the origin. -/
def rect (width height : Float) (fill : Fill := default) (stroke : Stroke := {})
    (name : Option Lean.Name := none) : Diagram β :=
  let d : Diagram β := fromPath (PathData.rect width height) fill stroke
  match name with
  | none => d
  | some n =>
    let halfStroke := stroke.width / 2
    let hw := width / 2 + halfStroke
    let hh := height / 2 + halfStroke
    withNameAndAnchors d n [
      { name := `north, offset := ⟨0, hh⟩ },
      { name := `south, offset := ⟨0, -hh⟩ },
      { name := `east, offset := ⟨hw, 0⟩ },
      { name := `west, offset := ⟨-hw, 0⟩ },
      { name := `northeast, offset := ⟨hw, hh⟩ },
      { name := `northwest, offset := ⟨-hw, hh⟩ },
      { name := `southeast, offset := ⟨hw, -hh⟩ },
      { name := `southwest, offset := ⟨-hw, -hh⟩ }
    ]

/-- A filled rounded rectangle centered at the origin. -/
def roundedRect (width height : Float) (cornerRadius : Float)
    (fill : Fill := default) (stroke : Stroke := {})
    (name : Option Lean.Name := none) : Diagram β :=
  let d : Diagram β := fromPath (PathData.roundedRect width height cornerRadius) fill stroke
  match name with
  | none => d
  | some n =>
    let halfStroke := stroke.width / 2
    let hw := width / 2 + halfStroke
    let hh := height / 2 + halfStroke
    withNameAndAnchors d n [
      { name := `north, offset := ⟨0, hh⟩ },
      { name := `south, offset := ⟨0, -hh⟩ },
      { name := `east, offset := ⟨hw, 0⟩ },
      { name := `west, offset := ⟨-hw, 0⟩ },
      { name := `northeast, offset := ⟨hw, hh⟩ },
      { name := `northwest, offset := ⟨-hw, hh⟩ },
      { name := `southeast, offset := ⟨hw, -hh⟩ },
      { name := `southwest, offset := ⟨-hw, -hh⟩ }
    ]

/-- A filled circle centered at the origin. -/
def circle (radius : Float) (fill : Fill := default) (stroke : Stroke := {})
    (name : Option Lean.Name := none) : Diagram β :=
  let halfStroke := stroke.width / 2
  let circleEnv : Envelope :=
    if halfStroke == 0 then Envelope.ofCircle radius
    else .nonempty fun _ => radius + halfStroke
  let d : Diagram β := .withEnv circleEnv
    (fromPath (PathData.circle radius) fill stroke)
  match name with
  | none => d
  | some n =>
    let half := stroke.width / 2
    let r := radius + half
    withNameAndAnchors d n [
      { name := `north, offset := ⟨0, r⟩ },
      { name := `south, offset := ⟨0, -r⟩ },
      { name := `east, offset := ⟨r, 0⟩ },
      { name := `west, offset := ⟨-r, 0⟩ }
    ]

/-- A filled ellipse centered at the origin with the given half-widths. -/
def ellipse (rx ry : Float) (fill : Fill := default) (stroke : Stroke := {})
    (name : Option Lean.Name := none) : Diagram β :=
  let halfStroke := stroke.width / 2
  let ellipseEnv : Envelope :=
    if halfStroke == 0 then Envelope.ofRect rx ry
    else .nonempty fun v =>
      v.x.abs * (rx + halfStroke) + v.y.abs * (ry + halfStroke)
  let d : Diagram β := .withEnv ellipseEnv
    (fromPath (PathData.ellipse rx ry) fill stroke)
  match name with
  | none => d
  | some n =>
    let half := stroke.width / 2
    withNameAndAnchors d n [
      { name := `north, offset := ⟨0, ry + half⟩ },
      { name := `south, offset := ⟨0, -(ry + half)⟩ },
      { name := `east, offset := ⟨rx + half, 0⟩ },
      { name := `west, offset := ⟨-(rx + half), 0⟩ }
    ]

/--
A wedge (circular sector) centered at the origin, sweeping counterclockwise
from {name}`startAngle` to {name}`endAngle` (in radians) at the given {name}`radius`.
-/
def wedge (startAngle endAngle radius : Float)
    (fill : Fill := default) (stroke : Stroke := {})
    (name : Option Lean.Name := none) : Diagram β :=
  let d : Diagram β := fromPath (PathData.wedge startAngle endAngle radius) fill stroke
  match name with
  | none => d
  | some n =>
    let sw := stroke.width / 2
    let r := radius + sw
    let midAngle := (startAngle + endAngle) / 2
    let p1 := Vec2.mk (r * Float.cos startAngle) (r * Float.sin startAngle)
    let p2 := Vec2.mk (r * Float.cos endAngle) (r * Float.sin endAngle)
    let pm := Vec2.mk (r * Float.cos midAngle) (r * Float.sin midAngle)
    withNameAndAnchors d n [
      { name := `tip, offset := ⟨0, 0⟩ },
      { name := `arcStart, offset := p1 },
      { name := `arcEnd, offset := p2 },
      { name := `arcMid, offset := pm }
    ]

/--
A ring wedge (annular sector) centered at the origin, sweeping counterclockwise
from {name}`startAngle` to {name}`endAngle` (in radians) between {name}`innerRadius`
and {name}`outerRadius`.
-/
def ringWedge (startAngle endAngle innerRadius outerRadius : Float)
    (fill : Fill := default) (stroke : Stroke := {})
    (name : Option Lean.Name := none) : Diagram β :=
  let d : Diagram β :=
    fromPath (PathData.ringWedge startAngle endAngle innerRadius outerRadius) fill stroke
  match name with
  | none => d
  | some n =>
    let sw := stroke.width / 2
    let ro := outerRadius + sw
    let ri := innerRadius - sw
    let midAngle := (startAngle + endAngle) / 2
    let midR := (ro + ri) / 2
    let outerMid := Vec2.mk (ro * Float.cos midAngle) (ro * Float.sin midAngle)
    let innerMid := Vec2.mk (ri * Float.cos midAngle) (ri * Float.sin midAngle)
    let center := Vec2.mk (midR * Float.cos midAngle) (midR * Float.sin midAngle)
    withNameAndAnchors d n [
      { name := `outerMid, offset := outerMid },
      { name := `innerMid, offset := innerMid },
      { name := `center, offset := center }
    ]

/-- A regular polygon centered at the origin with the given number of sides and circumradius.
    If {name}`sides` is less than 3, uses 3 and attaches a validation warning. -/
def polygon (sides : Nat) (radius : Float)
    (fill : Fill := default)
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
- 2 points: a vertical lozenge (diamond) whose half-width equals {name}`innerRadius`.
- 3+ points: a star with {name}`points` outer tips and {name}`points` inner valleys.
-/
def star (points : Nat) (outerRadius innerRadius : Float)
    (fill : Fill := default)
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
  /-- Optionally names a diagram and attaches {lean}`` `point0 ``, {lean}`` `point1 ``, … anchors. -/
  addPointAnchors (d : Diagram β) (name : Option Lean.Name) (pts : List Vec2) :
      Diagram β :=
    match name with
    | none => d
    | some n =>
      let anchors := pts.mapIdx fun i p =>
        { name := Lean.Name.mkSimple s!"point{i}", offset := p : AnchorPos }
      withNameAndAnchors d n anchors

/--
A horizontal curly brace centered at the origin, pointing downward.
The brace spans {name}`width` horizontally and extends {name}`depth` downward to a central tip.
Optionally attach a {name}`label` diagram below the tip.
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
  let brace : Diagram β := fromStroke path stroke
  match label with
  | none => brace
  | some lbl =>
    let labelOffset := depth + 10
    Diagram.compose brace (Diagram.transform (Matrix.translate 0 (-labelOffset)) lbl)

/-- An image primitive. -/
def fromImage (ref : ImageRef) : Diagram β :=
  .prim (.image ref)
