/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/
module

public section

namespace Illuminate

instance : Hashable Float where
  hash f :=
    if f != f then 0            -- all NaNs hash the same
    else if f == 0 then hash (0.0 : Float).toBits  -- +0.0 and -0.0 hash the same
    else hash f.toBits

instance : Hashable Empty where
  hash e := nomatch e

/-- A 2D vector with {name (full := Vec2.x)}`x` and {name (full := Vec2.y)}`y` components. -/
structure Vec2 where
  /-- Horizontal component. -/
  x : Float
  /-- Vertical component. -/
  y : Float
deriving Repr, BEq, Inhabited, Hashable

/-- A 2D point representing a position in space, distinct from {name}`Vec2` which represents a direction or offset. -/
structure Point where
  /-- Horizontal coordinate. -/
  x : Float
  /-- Vertical coordinate. -/
  y : Float
deriving Repr, BEq, Inhabited, Hashable


/--
A 3×3 homogeneous matrix for 2D affine transforms.
Stored as six values (the third row is always {lit}`[0, 0, 1]`):
```
⎡ a  b  tx ⎤
⎢ c  d  ty ⎥
⎣ 0  0   1 ⎦
```
-/
structure Matrix where
  /-- First-row x-scale / rotation component. -/
  a : Float
  /-- First-row y-shear / rotation component. -/
  b : Float
  /-- Horizontal translation. -/
  tx : Float
  /-- Second-row x-shear / rotation component. -/
  c : Float
  /-- Second-row y-scale / rotation component. -/
  d : Float
  /-- Vertical translation. -/
  ty : Float
deriving Repr, BEq, Inhabited, Hashable


/--
An envelope maps a unit direction vector to a scalar extent of a shape
in that direction. Given direction $`v`, $`envelope(v)` returns the scalar $`t`
such that the shape fits within the half-plane $`{ p | p · v ≤ t }`.
-/
inductive Envelope where
  /-- The empty envelope takes up no space. -/
  | empty
  /-- A nonempty envelope. -/
  | nonempty (env : Vec2 → Float)


/-- A single drawing command within a path. -/
inductive PathCmd where
  /-- Moves the pen to the given point without drawing. -/
  | moveTo : Vec2 → PathCmd
  /-- Draws a straight line from the current point to the given point. -/
  | lineTo : Vec2 → PathCmd
  /-- Draws a cubic Bézier curve through two control points to an endpoint. -/
  | curveTo : Vec2 → Vec2 → Vec2 → PathCmd
  /-- Draws an elliptical arc to the given endpoint (SVG {lit}`A` command). -/
  | arcTo (rx ry xRotation : Float) (largeArc sweep : Bool) (endpoint : Vec2) : PathCmd
  /-- Closes the current sub-path by drawing a line back to its start. -/
  | closePath : PathCmd
deriving Repr, BEq, Inhabited, Hashable

/-- An ordered sequence of path commands describing a shape outline. -/
structure PathData where
  /-- The sequence of drawing commands. -/
  commands : Array PathCmd
deriving Repr, BEq, Inhabited, Hashable
