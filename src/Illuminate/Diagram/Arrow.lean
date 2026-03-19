/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/

import Illuminate.Geometry
import Illuminate.Style
import Illuminate.Diagram.Basic
import Illuminate.Diagram.Placement


namespace Illuminate

/-- A label to place on a connection. -/
structure Label (β : Type) where
  /-- The label diagram (typically text). -/
  label : Diagram β
  /-- Position along the curve as a parameter from 0 (start) to 1 (end). -/
  pos : Float := 0.5
  /-- Additional offset after automatic placement. -/
  shift : Vec2 := 0
  /-- When true, the label keeps its original orientation instead of rotating to follow the arrow. -/
  upright : Bool := false

instance {β : Type} : Coe (Diagram β) (Label β) where
  coe label := { label }

/-- One endpoint of a line/arrow, specifying where and how the line departs or arrives. -/
structure LineEnd where
  /-- Named anchor point in the diagram to connect. -/
  point : Lean.Name
  /-- Additional offset applied to the resolved anchor position. -/
  shift : Vec2 := 0
  /-- Departure/arrival angle in radians. If `none`, defaults to the straight-line angle. -/
  angle : Option Float := none
  /-- Controls how far the Bézier control point extends from this endpoint,
      as a fraction of the distance between endpoints. Like Racket pict's `pull`. -/
  pull : Float := 0.25
  /-- Optional arrowhead drawn at this endpoint. -/
  arrowhead : Option Arrowhead := none
deriving Repr, BEq

instance : Coe Lean.Name LineEnd where
  coe point := { point }

-- ═══════════════════════════════════════════════════════════════
-- Arrowhead rendering
-- ═══════════════════════════════════════════════════════════════

namespace ArrowDraw

variable {β : Type}

/-- Default base length of arrowheads (before scaling). -/
private def baseHeadLen : Float := 8.0

/-- Default half-angle of open/stealth arrowheads in radians (~23°). -/
private def headAngle : Float := 0.4

/--
Computes the minimum half-angle so the arrowhead covers the shaft at its end point.
For stealth, the shaft ends at the notch (headLen/2); for triangle, at the base.
-/
private def minHalfAngle (ah : Arrowhead) (strokeWidth : Float) : Float :=
  let headLen := baseHeadLen * ah.length
  match ah.type with
  | .stealth =>
    -- At notch (headLen*0.5 from tip), width = headLen * tan(a). Need >= strokeWidth.
    if headLen > 0.001 then Float.atan (strokeWidth / headLen) else 0
  | .triangle =>
    -- At base, width = 2*headLen*sin(a). Need >= strokeWidth.
    let ratio := strokeWidth / (2 * headLen)
    if headLen > 0.001 && ratio < 1 then Float.asin ratio else 0
  | _ => 0

/--
Computes how much the shaft must be shortened for a given arrowhead type.
Takes the stroke width to match the minimum angle enforcement in `drawArrowhead`.
-/
def arrowheadShorten (ah : Arrowhead) (strokeWidth : Float) : Float :=
  let headLen := baseHeadLen * ah.length
  let halfAngle := max (headAngle * ah.width) (minHalfAngle ah strokeWidth)
  match ah.type with
  | .latex => 0
  | .stealth => headLen * 0.5
  | .triangle => headLen * Float.cos halfAngle
  | .circle => headLen * 0.4 * 2

/--
Draws an arrowhead at `tip` pointing in direction `dir`.
Returns the arrowhead diagram and the amount the shaft should be shortened
to avoid overlapping the head.
-/
def drawArrowhead (ah : Arrowhead) (tip dir : Vec2) (stroke : FullStroke)
    : Diagram β × Float :=
  let headLen := baseHeadLen * ah.length
  -- For filled arrowheads, enforce minimum angle so the arrowhead covers the shaft
  let halfAngle := max (headAngle * ah.width) (minHalfAngle ah stroke.width)
  let n := dir.normalize
  let strokeOverride := stroke.toStroke
  match ah.type with
  | .latex =>
    let cosA := Float.cos halfAngle
    let sinA := Float.sin halfAngle
    let ld : Vec2 := ⟨-(n.x * cosA - n.y * sinA), -(n.y * cosA + n.x * sinA)⟩
    let rd : Vec2 := ⟨-(n.x * cosA + n.y * sinA), -(n.y * cosA - n.x * sinA)⟩
    let path := PathData.empty
      |>.moveTo (tip + headLen • ld)
      |>.lineTo tip
      |>.lineTo (tip + headLen • rd)
    (Diagram.fromStroke path strokeOverride, 0)
  | .stealth =>
    let cosA := Float.cos halfAngle
    let sinA := Float.sin halfAngle
    let ld : Vec2 := ⟨-(n.x * cosA - n.y * sinA), -(n.y * cosA + n.x * sinA)⟩
    let rd : Vec2 := ⟨-(n.x * cosA + n.y * sinA), -(n.y * cosA - n.x * sinA)⟩
    let notch := tip + (headLen * 0.5) • (-n)
    let path := PathData.empty
      |>.moveTo tip
      |>.lineTo (tip + headLen • ld)
      |>.lineTo notch
      |>.lineTo (tip + headLen • rd)
      |>.close
    (Diagram.fromPath path (fill := { color := stroke.color }) (stroke := strokeOverride), headLen * 0.5)
  | .triangle =>
    let cosA := Float.cos halfAngle
    let sinA := Float.sin halfAngle
    let ld : Vec2 := ⟨-(n.x * cosA - n.y * sinA), -(n.y * cosA + n.x * sinA)⟩
    let rd : Vec2 := ⟨-(n.x * cosA + n.y * sinA), -(n.y * cosA - n.x * sinA)⟩
    let path := PathData.empty
      |>.moveTo tip
      |>.lineTo (tip + headLen • ld)
      |>.lineTo (tip + headLen • rd)
      |>.close
    (Diagram.fromPath path (fill := { color := stroke.color }) (stroke := strokeOverride), headLen * cosA)
  | .circle =>
    let r := headLen * 0.4
    let center := tip - r • n
    let d := Diagram.transform (Matrix.translate center.x center.y)
      (Diagram.circle r (fill := { color := stroke.color }) (stroke := strokeOverride))
    (d, r * 2)

-- ═══════════════════════════════════════════════════════════════
-- Bézier curve computation
-- ═══════════════════════════════════════════════════════════════

/-- Evaluates a cubic Bézier curve at parameter `t`. -/
def bezierAt (p0 c1 c2 p3 : Vec2) (t : Float) : Vec2 :=
  let u := 1 - t
  (u * u * u) • p0 + (3 * u * u * t) • c1 + (3 * u * t * t) • c2 + (t * t * t) • p3

/-- Computes the tangent (first derivative) of a cubic Bézier at parameter `t`. -/
def bezierTangent (p0 c1 c2 p3 : Vec2) (t : Float) : Vec2 :=
  let u := 1 - t
  (3 * u * u) • (c1 - p0) + (6 * u * t) • (c2 - c1) + (3 * t * t) • (p3 - c2)


/-- Computes the unit direction vector for a given angle in radians. -/
private def angleDir (θ : Float) : Vec2 := ⟨Float.cos θ, Float.sin θ⟩

/-- Computes the angle (in radians) from `a` to `b`. -/
def angleBetween (a b : Vec2) : Float :=
  Float.atan2 (b.y - a.y) (b.x - a.x)

/--
Computes cubic Bézier control points for a line between two resolved endpoints.

Each endpoint has an angle (the direction the line departs/arrives) and a pull
(how far the control point extends as a fraction of the endpoint distance).
This matches Racket pict's `pin-arrow-line` behaviour:
- `c1 = src + pull × dist × dir(srcAngle)`
- `c2 = tgt - pull × dist × dir(tgtAngle)`
-/
def computeControlPoints (src tgt : Vec2)
    (srcAngle tgtAngle : Float) (srcPull tgtPull : Float)
    : Vec2 × Vec2 :=
  let dist := (tgt - src).length
  let c1 := src + (srcPull * dist) • angleDir srcAngle
  let c2 := tgt - (tgtPull * dist) • angleDir tgtAngle
  (c1, c2)

-- ═══════════════════════════════════════════════════════════════
-- High-level line/arrow drawing
-- ═══════════════════════════════════════════════════════════════

/--
Draws a line or arrow between two resolved points with the given endpoint specs.
This is the low-level function that operates on concrete `Vec2` positions.
-/
def drawLine (srcPos tgtPos : Vec2)
    (srcEnd tgtEnd : LineEnd) (stroke : FullStroke := .defaultArrow)
    : Diagram β :=
  let straightAngle := angleBetween srcPos tgtPos
  let srcAngle := srcEnd.angle.getD straightAngle
  let tgtAngle := tgtEnd.angle.getD straightAngle
  -- Compute control points for the full curve
  let (c1, c2) := computeControlPoints srcPos tgtPos srcAngle tgtAngle srcEnd.pull tgtEnd.pull
  -- Sample the Bézier near each tip to get visual arrowhead directions
  let dist := (tgtPos - srcPos).length
  let srcVisualDir := match srcEnd.arrowhead with
    | some ah =>
      let headLen := baseHeadLen * ah.length
      let δ := min 0.15 (headLen / dist)
      (srcPos - bezierAt srcPos c1 c2 tgtPos δ).normalize
    | none => Vec2.zero
  let tgtVisualDir := match tgtEnd.arrowhead with
    | some ah =>
      let headLen := baseHeadLen * ah.length
      let δ := min 0.15 (headLen / dist)
      (tgtPos - bezierAt srcPos c1 c2 tgtPos (1 - δ)).normalize
    | none => Vec2.zero
  -- Shorten shaft along the visual arrowhead direction so it doesn't poke
  -- through filled heads. For unfilled heads (latex), the shaft meets the tip.
  let srcShorten := match srcEnd.arrowhead with
    | some ah => arrowheadShorten ah stroke.width
    | none => 0.0
  let tgtShorten := match tgtEnd.arrowhead with
    | some ah => arrowheadShorten ah stroke.width
    | none => 0.0
  let shaftSrc := srcPos - srcShorten • srcVisualDir
  let shaftTgt := tgtPos - tgtShorten • tgtVisualDir
  -- Recompute control points for shortened shaft
  let (sc1, sc2) := computeControlPoints shaftSrc shaftTgt srcAngle tgtAngle srcEnd.pull tgtEnd.pull
  -- Draw arrowheads using the visual directions
  let srcHead := match srcEnd.arrowhead with
    | some ah => some (drawArrowhead ah srcPos srcVisualDir stroke).1
    | none => none
  let tgtHead := match tgtEnd.arrowhead with
    | some ah => some (drawArrowhead ah tgtPos tgtVisualDir stroke).1
    | none => none
  -- Determine if this is a straight line (default angles, no pull override making it curved)
  let isStraight := srcEnd.angle.isNone && tgtEnd.angle.isNone &&
    srcEnd.pull == 0.25 && tgtEnd.pull == 0.25
  let strokeOverride := stroke.toStroke
  let shaft :=
    if isStraight then
      Diagram.fromStroke (PathData.line shaftSrc shaftTgt) strokeOverride
    else
      Diagram.fromStroke
        (PathData.empty |>.moveTo shaftSrc |>.curveTo sc1 sc2 shaftTgt) strokeOverride
  -- Compose
  let result := shaft
  let result := match srcHead with | some h => Diagram.atop result h | none => result
  let result := match tgtHead with | some h => Diagram.atop result h | none => result
  result

end ArrowDraw

-- ═══════════════════════════════════════════════════════════════
-- Diagram-level API
-- ═══════════════════════════════════════════════════════════════

/--
Builds the arrow diagram between two concrete points with the given endpoint specs and label.
-/
private def buildArrow {β : Type} (srcPos tgtPos : Vec2)
    (start stop : LineEnd) (stroke : FullStroke)
    (label : Option (Label β) := none)
    (labelUpright : Bool := false) : Diagram β :=
  let arrow := ArrowDraw.drawLine srcPos tgtPos start stop stroke
  match label with
  | none => arrow
  | some lbl =>
    let straightAngle := ArrowDraw.angleBetween srcPos tgtPos
    let srcAngle := start.angle.getD straightAngle
    let tgtAngle := stop.angle.getD straightAngle
    let (c1, c2) := ArrowDraw.computeControlPoints srcPos tgtPos srcAngle tgtAngle start.pull stop.pull
    let mid := ArrowDraw.bezierAt srcPos c1 c2 tgtPos lbl.pos
    let tangent := ArrowDraw.bezierTangent srcPos c1 c2 tgtPos lbl.pos
    let n := tangent.normalize
    let chord := tgtPos - srcPos
    let chordMid := (0.5 : Float) • (srcPos + tgtPos)
    let bulge := mid - chordMid
    let bulgeCross := chord.x * bulge.y - chord.y * bulge.x
    let rawPerp : Vec2 := ⟨-n.y, n.x⟩
    let perp := if bulgeCross.abs < 1e-6 then rawPerp
                else if (rawPerp.x * bulge.x + rawPerp.y * bulge.y) > 0 then rawPerp
                else -rawPerp
    let labelEnv := lbl.label.getEnvelope
    let isUpright := lbl.upright || labelUpright
    let pos :=
      if isUpright then
        let extent := labelEnv (-perp)
        mid + (extent + stroke.width + 6) • perp + lbl.shift
      else
        let labelH := (labelEnv Vec2.north + labelEnv Vec2.south) / 2
        mid + (labelH + stroke.width + 4) • perp + lbl.shift
    let labelXform :=
      if isUpright then
        Matrix.translate pos.x pos.y
      else
        let halfPi := pi / 2
        let angle := let a := Float.atan2 n.y n.x
          if a > halfPi || a < -halfPi then a + pi else a
        Matrix.mul (Matrix.translate pos.x pos.y) (Matrix.rotate angle)
    Diagram.atop arrow (Diagram.transform labelXform lbl.label)

/--
Draws a line or arrow between two points.
-/
def Diagram.connect' {β : Type} (srcPoint tgtPoint : Point) (start stop : LineEnd)
    (stroke : FullStroke := .defaultArrow)
    (arrowhead : Option Arrowhead := none)
    (label : Option (Label β) := none)
    (d : Diagram β) : Diagram β :=
  let rc := d.resolveOuterConfig
  let stop' := { stop with arrowhead := stop.arrowhead <|> arrowhead <|> rc.arrowhead }
  let src := srcPoint.toVec2 + start.shift
  let tgt := tgtPoint.toVec2 + stop.shift
  .compose d (buildArrow src tgt start stop' stroke label rc.labelUpright)

/--
Draws a line or arrow between two named anchor points in a diagram.
Resolves names eagerly using `find` and `origin`. The diagram's outer
`DrawConfig` is consulted for default arrowhead and label orientation.
-/
def Diagram.connect {β : Type} (start stop : LineEnd)
    (stroke : FullStroke := .defaultArrow)
    (arrowhead : Option Arrowhead := none)
    (label : Option (Label β) := none)
    (d : Diagram β) : Diagram β :=
  let rc := d.resolveOuterConfig
  let stop' := { stop with arrowhead := stop.arrowhead <|> arrowhead <|> rc.arrowhead }
  let srcPoint := (d.find start.point).origin
  let tgtPoint := (d.find stop.point).origin
  let src := srcPoint.toVec2 + start.shift
  let tgt := tgtPoint.toVec2 + stop.shift
  .compose d (buildArrow src tgt start stop' stroke label rc.labelUpright)

/-- Direction for the bend in an L-shaped connection. -/
inductive BendDirection where
  /-- The line goes horizontally first, then vertically. -/
  | horizontal
  /-- The line goes vertically first, then horizontally. -/
  | vertical
deriving Repr, BEq, Inhabited

/--
Draws an L-shaped (right-angle) line or arrow between two points.
`bend` controls whether the line goes horizontally or vertically first.
-/
def Diagram.connectL' {β : Type} (srcPoint tgtPoint : Point) (start stop : LineEnd)
    (bend : BendDirection := .vertical)
    (stroke : FullStroke := .defaultArrow)
    (d : Diagram β) : Diagram β :=
  let rc := d.resolveOuterConfig
  let strokeOverride := stroke.toStroke
  let src := srcPoint.toVec2 + start.shift
  let tgt := tgtPoint.toVec2 + stop.shift
  let startAh := start.arrowhead <|> rc.arrowhead
  let stopAh := stop.arrowhead <|> rc.arrowhead
  let mid : Vec2 := match bend with
    | .vertical => ⟨src.x, tgt.y⟩
    | .horizontal => ⟨tgt.x, src.y⟩
  let seg1 := Diagram.fromStroke (PathData.line src mid) strokeOverride
  let seg2 := Diagram.fromStroke (PathData.line mid tgt) strokeOverride
  let heads := match startAh with
    | some ah =>
      let dir := (src - mid).normalize
      (ArrowDraw.drawArrowhead ah src dir stroke).1
    | none => .empty
  let heads := match stopAh with
    | some ah =>
      let dir := (tgt - mid).normalize
      Diagram.compose heads (ArrowDraw.drawArrowhead ah tgt dir stroke).1
    | none => heads
  .compose d ([seg1, seg2, heads].foldl Diagram.compose .empty)

/--
Draws an L-shaped (right-angle) line or arrow between two named anchor points.
`bend` controls whether the line goes horizontally or vertically first.
The diagram's outer `DrawConfig` is consulted for default arrowhead style.
-/
def Diagram.connectL {β : Type} (start stop : LineEnd)
    (bend : BendDirection := .vertical)
    (stroke : FullStroke := .defaultArrow)
    (d : Diagram β) : Diagram β :=
  let srcPoint := (d.find start.point).origin
  let tgtPoint := (d.find stop.point).origin
  d.connectL' srcPoint tgtPoint start stop bend stroke
