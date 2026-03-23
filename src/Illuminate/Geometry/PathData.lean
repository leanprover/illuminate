/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/

import Illuminate.Geometry.Vec2


namespace Illuminate

/-- A single drawing command within a path. -/
inductive PathCmd where
  /-- Moves the pen to the given point without drawing. -/
  | moveTo : Vec2 → PathCmd
  /-- Draws a straight line from the current point to the given point. -/
  | lineTo : Vec2 → PathCmd
  /-- Draws a cubic Bézier curve through two control points to an endpoint. -/
  | curveTo : Vec2 → Vec2 → Vec2 → PathCmd
  /-- Closes the current sub-path by drawing a line back to its start. -/
  | closePath : PathCmd
deriving Repr, BEq, Inhabited, Hashable

/-- An ordered sequence of path commands describing a shape outline. -/
structure PathData where
  /-- The sequence of drawing commands. -/
  commands : Array PathCmd
deriving Repr, BEq, Inhabited, Hashable

namespace PathData

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

/-- Builds a circle approximation using four cubic Bézier curves. -/
def circle (radius : Float) : PathData :=
  -- κ ≈ 0.5522847498 is the standard Bézier approximation constant for quarter arcs
  let k := 0.5522847498 * radius
  let r := radius
  ⟨#[.moveTo ⟨r, 0⟩,
     .curveTo ⟨r, k⟩ ⟨k, r⟩ ⟨0, r⟩,
     .curveTo ⟨-k, r⟩ ⟨-r, k⟩ ⟨-r, 0⟩,
     .curveTo ⟨-r, -k⟩ ⟨-k, -r⟩ ⟨0, -r⟩,
     .curveTo ⟨k, -r⟩ ⟨r, -k⟩ ⟨r, 0⟩,
     .closePath]⟩

/-- Builds a closed rounded rectangle centered at the origin with the given dimensions and corner radius. -/
def roundedRect (width height : Float) (cornerRadius : Float) : PathData :=
  let hw := width / 2
  let hh := height / 2
  let r := Min.min cornerRadius (Min.min hw hh)
  let k := 0.5522847498 * r
  ⟨#[-- Start at top-left, just after the corner
     .moveTo ⟨-hw + r, hh⟩,
     -- Top edge → top-right corner
     .lineTo ⟨hw - r, hh⟩,
     .curveTo ⟨hw - r + k, hh⟩ ⟨hw, hh - r + k⟩ ⟨hw, hh - r⟩,
     -- Right edge → bottom-right corner
     .lineTo ⟨hw, -hh + r⟩,
     .curveTo ⟨hw, -hh + r - k⟩ ⟨hw - r + k, -hh⟩ ⟨hw - r, -hh⟩,
     -- Bottom edge → bottom-left corner
     .lineTo ⟨-hw + r, -hh⟩,
     .curveTo ⟨-hw + r - k, -hh⟩ ⟨-hw, -hh + r - k⟩ ⟨-hw, -hh + r⟩,
     -- Left edge → top-left corner
     .lineTo ⟨-hw, hh - r⟩,
     .curveTo ⟨-hw, hh - r + k⟩ ⟨-hw + r - k, hh⟩ ⟨-hw + r, hh⟩,
     .closePath]⟩

/-- Builds an ellipse centered at the origin using four cubic Bézier curves. -/
def ellipse (rx ry : Float) : PathData :=
  let kx := 0.5522847498 * rx
  let ky := 0.5522847498 * ry
  ⟨#[.moveTo ⟨rx, 0⟩,
     .curveTo ⟨rx, ky⟩ ⟨kx, ry⟩ ⟨0, ry⟩,
     .curveTo ⟨-kx, ry⟩ ⟨-rx, ky⟩ ⟨-rx, 0⟩,
     .curveTo ⟨-rx, -ky⟩ ⟨-kx, -ry⟩ ⟨0, -ry⟩,
     .curveTo ⟨kx, -ry⟩ ⟨rx, -ky⟩ ⟨rx, 0⟩,
     .closePath]⟩
