/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/
module
import Illuminate.Geometry.Basic
public import Illuminate.Geometry.Types
public section

namespace Illuminate.PathData

/-- Creates an empty path with no commands. -/
def empty : PathData := ⟨#[]⟩

/-- Appends a move-to command to the path. -/
def moveTo (p : Vec2) (pd : PathData) : PathData :=
  ⟨pd.commands.push (.moveTo p)⟩

/-- Appends a line-to command to the path. -/
def lineTo (p : Vec2) (pd : PathData) : PathData :=
  ⟨pd.commands.push (.lineTo p)⟩

/-- Appends a cubic Bézier curve command to the path. -/
def curveTo (c1 c2 ep : Vec2) (pd : PathData) : PathData :=
  ⟨pd.commands.push (.curveTo c1 c2 ep)⟩

/-- Appends an elliptical arc command to the path. -/
def arcTo (rx ry xRotation : Float) (largeArc sweep : Bool) (endpoint : Vec2)
    (pd : PathData) : PathData :=
  ⟨pd.commands.push (.arcTo rx ry xRotation largeArc sweep endpoint)⟩

/-- Appends a close-path command, connecting back to the sub-path start. -/
def close (pd : PathData) : PathData :=
  ⟨pd.commands.push .closePath⟩

/-- Builds a straight line segment from {lean}`a` to {lean}`b`. -/
def line (a b : Vec2) : PathData :=
  ⟨#[.moveTo a, .lineTo b]⟩

/-- Builds a closed rectangle centered at the origin with the given width and height. -/
def rect (width height : Float) : PathData :=
  let hw := width / 2
  let hh := height / 2
  ⟨#[.moveTo ⟨-hw, -hh⟩,
     .lineTo ⟨hw, -hh⟩,
     .lineTo ⟨hw, hh⟩,
     .lineTo ⟨-hw, hh⟩,
     .closePath]⟩

/-- Builds a circle centered at the origin using two semicircular arcs. -/
def circle (radius : Float) : PathData :=
  let r := radius
  ⟨#[.moveTo ⟨r, 0⟩,
     .arcTo r r 0 false true ⟨-r, 0⟩,
     .arcTo r r 0 false true ⟨r, 0⟩,
     .closePath]⟩

/-- Builds a closed rounded rectangle centered at the origin with the given dimensions and corner radius. -/
def roundedRect (width height : Float) (cornerRadius : Float) : PathData :=
  let hw := width / 2
  let hh := height / 2
  let r := Min.min cornerRadius (Min.min hw hh)
  ⟨#[-- Start at top-left, just after the corner
     .moveTo ⟨-hw + r, hh⟩,
     -- Top edge → top-right corner
     .lineTo ⟨hw - r, hh⟩,
     .arcTo r r 0 false false ⟨hw, hh - r⟩,
     -- Right edge → bottom-right corner
     .lineTo ⟨hw, -hh + r⟩,
     .arcTo r r 0 false false ⟨hw - r, -hh⟩,
     -- Bottom edge → bottom-left corner
     .lineTo ⟨-hw + r, -hh⟩,
     .arcTo r r 0 false false ⟨-hw, -hh + r⟩,
     -- Left edge → top-left corner
     .lineTo ⟨-hw, hh - r⟩,
     .arcTo r r 0 false false ⟨-hw + r, hh⟩,
     .closePath]⟩

/-- Builds an ellipse centered at the origin using two semicircular arcs. -/
def ellipse (rx ry : Float) : PathData :=
  ⟨#[.moveTo ⟨rx, 0⟩,
     .arcTo rx ry 0 false true ⟨-rx, 0⟩,
     .arcTo rx ry 0 false true ⟨rx, 0⟩,
     .closePath]⟩

/--
Builds a wedge (circular sector) centered at the origin from {name}`startAngle` to
{name}`endAngle` (in radians, counterclockwise) with the given {name}`radius`.
-/
def wedge (startAngle endAngle radius : Float) : PathData :=
  let p1 := Vec2.mk (radius * Float.cos startAngle) (radius * Float.sin startAngle)
  let p2 := Vec2.mk (radius * Float.cos endAngle) (radius * Float.sin endAngle)
  let sweep := endAngle - startAngle
  let largeArc := sweep.abs > pi
  -- sweep > 0 means counterclockwise
  let sweepFlag := sweep > 0
  ⟨#[.moveTo ⟨0, 0⟩,
     .lineTo p1,
     .arcTo radius radius 0 largeArc sweepFlag p2,
     .lineTo ⟨0, 0⟩,
     .closePath]⟩

/--
Builds a ring wedge (annular sector) centered at the origin from {name}`startAngle` to
{name}`endAngle` (in radians, counterclockwise) between {name}`innerRadius` and
{name}`outerRadius`.
-/
def ringWedge (startAngle endAngle innerRadius outerRadius : Float) : PathData :=
  let cosS := Float.cos startAngle
  let sinS := Float.sin startAngle
  let cosE := Float.cos endAngle
  let sinE := Float.sin endAngle
  let outerStart := Vec2.mk (outerRadius * cosS) (outerRadius * sinS)
  let outerEnd := Vec2.mk (outerRadius * cosE) (outerRadius * sinE)
  let innerEnd := Vec2.mk (innerRadius * cosE) (innerRadius * sinE)
  let innerStart := Vec2.mk (innerRadius * cosS) (innerRadius * sinS)
  let sweep := endAngle - startAngle
  let largeArc := sweep.abs > pi
  let sweepFlag := sweep > 0
  ⟨#[.moveTo outerStart,
     .arcTo outerRadius outerRadius 0 largeArc sweepFlag outerEnd,
     .lineTo innerEnd,
     .arcTo innerRadius innerRadius 0 largeArc (!sweepFlag) innerStart,
     .closePath]⟩

end PathData

/-!
# SVG arc endpoint-to-center conversion

Implements the SVG specification §F.6.5 algorithm for converting arc endpoint
parameterization (two points, radii, flags) to center parameterization (center,
start angle, sweep angle). Used by bounding box computation and ray-arc intersection.
-/

/-- Center parameterization of an elliptical arc. -/
structure ArcCenter where
  /-- X coordinate of the ellipse center. -/
  cx : Float
  /-- Y coordinate of the ellipse center. -/
  cy : Float
  /-- Start angle in radians. -/
  theta1 : Float
  /-- Sweep angle in radians (positive = counterclockwise). -/
  dtheta : Float
deriving Repr, BEq, Inhabited

/--
Converts SVG arc endpoint parameterization to center parameterization.

Follows the SVG specification §F.6.5 algorithm. Adjusts radii upward when they are
too small to connect the endpoints, and handles degenerate cases.
-/
def endpointToCenter (x1 y1 rx' ry' phi : Float) (fA fS : Bool) (x2 y2 : Float) :
    ArcCenter :=
  -- Step 1: Compute (x1', y1') in rotated coordinates
  let cosPhi := Float.cos phi
  let sinPhi := Float.sin phi
  let dx := (x1 - x2) / 2
  let dy := (y1 - y2) / 2
  let x1p := cosPhi * dx + sinPhi * dy
  let y1p := -sinPhi * dx + cosPhi * dy
  -- Step 2: Compute (cx', cy') — adjust radii if too small
  let x1pSq := x1p * x1p
  let y1pSq := y1p * y1p
  let rx0 := rx'.abs
  let ry0 := ry'.abs
  let lambda := x1pSq / (rx0 * rx0) + y1pSq / (ry0 * ry0)
  let (rx, ry) := if lambda > 1 then
    let sqLambda := lambda.sqrt
    (sqLambda * rx0, sqLambda * ry0)
  else (rx0, ry0)
  let rxSq := rx * rx
  let rySq := ry * ry
  let num := rxSq * rySq - rxSq * y1pSq - rySq * x1pSq
  let denom := rxSq * y1pSq + rySq * x1pSq
  let sq := if denom == 0 then 0 else (max 0 (num / denom)).sqrt
  let sign := if fA == fS then -1 else 1
  let cxp := sign * sq * (rx * y1p / ry)
  let cyp := sign * sq * (-(ry * x1p / rx))
  -- Step 3: Compute (cx, cy)
  let cx := cosPhi * cxp - sinPhi * cyp + (x1 + x2) / 2
  let cy := sinPhi * cxp + cosPhi * cyp + (y1 + y2) / 2
  -- Step 4: Compute theta1 and dtheta
  let ux := (x1p - cxp) / rx
  let uy := (y1p - cyp) / ry
  let vx := (-x1p - cxp) / rx
  let vy := (-y1p - cyp) / ry
  let angleBetween (ax ay bx by_ : Float) : Float :=
    let dot := ax * bx + ay * by_
    let cross := ax * by_ - ay * bx
    let cosA := min 1 (max (-1) (dot / ((ax * ax + ay * ay).sqrt * (bx * bx + by_ * by_).sqrt)))
    let a := Float.acos cosA
    if cross < 0 then -a else a
  let theta1 := angleBetween 1 0 ux uy
  let dtheta0 := angleBetween ux uy vx vy
  -- Clamp sweep to match the sweep flag
  let dtheta :=
    if fS && dtheta0 < 0 then dtheta0 + 2 * pi
    else if !fS && dtheta0 > 0 then dtheta0 - 2 * pi
    else dtheta0
  { cx, cy, theta1, dtheta }

/--
Computes the extremal points of an elliptical arc for bounding box purposes.

Returns the two endpoints plus any axis-aligned extrema (angles 0, π/2, π, 3π/2)
that fall within the arc's angular range.
-/
def arcExtrema (cur : Vec2) (rx ry xRotation : Float) (largeArc sweep : Bool)
    (ep : Vec2) : List Vec2 :=
  let ac := endpointToCenter cur.x cur.y rx ry xRotation largeArc sweep ep.x ep.y
  let cosPhi := Float.cos xRotation
  let sinPhi := Float.sin xRotation
  -- Point on the ellipse at parameter angle theta
  let pointAt (theta : Float) : Vec2 :=
    let ct := Float.cos theta
    let st := Float.sin theta
    ⟨ac.cx + cosPhi * rx * ct - sinPhi * ry * st,
     ac.cy + sinPhi * rx * ct + cosPhi * ry * st⟩
  -- Normalize angle to [0, 2π)
  let twoPi := 2 * pi
  let normalize (a : Float) : Float :=
    let r := a - twoPi * (a / twoPi).floor
    if r < 0 then r + twoPi else r
  -- Check whether angle theta is within the arc
  let inArc (theta : Float) : Bool :=
    if ac.dtheta > 0 then
      let t := normalize (theta - ac.theta1)
      t <= ac.dtheta + 1e-9
    else
      let t := normalize (ac.theta1 - theta)
      t <= (-ac.dtheta) + 1e-9
  -- Axis-aligned extrema angles for a rotated ellipse:
  -- x-extrema at theta = -atan(ry * tan(phi) / rx) + k*pi
  -- y-extrema at theta = atan(ry / (rx * tan(phi))) + k*pi
  -- For phi = 0, these simplify to 0, pi/2, pi, 3*pi/2
  let extremaAngles : List Float :=
    if nearZero xRotation then
      [0, pi / 2, pi, 3 * pi / 2]
    else
      let txBase := Float.atan2 (-ry * Float.sin xRotation) (rx * Float.cos xRotation)
      let tyBase := Float.atan2 (ry * Float.cos xRotation) (rx * Float.sin xRotation)
      [txBase, txBase + pi, tyBase, tyBase + pi]
  let extraPts := extremaAngles.filterMap fun theta =>
    if inArc theta then some (pointAt theta) else none
  [cur, ep] ++ extraPts

namespace PathData
