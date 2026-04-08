/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/
module
public import Illuminate.Geometry.Types
public section

namespace Illuminate.Point

/-- The origin point {lit}`(0, 0)`. -/
def origin : Point := ⟨0, 0⟩

/-- Translates a point by a vector offset. -/
def translate (p : Point) (v : Vec2) : Point := ⟨p.x + v.x, p.y + v.y⟩

/-- Computes the vector from point {lean}`a` to point {lean}`b`. -/
def vecTo (a b : Point) : Vec2 := ⟨b.x - a.x, b.y - a.y⟩

/-- Converts a point to a {name}`Vec2` (interpreting the position as an offset from the origin). -/
def toVec2 (p : Point) : Vec2 := ⟨p.x, p.y⟩

/-- Constructs a point from a {name}`Vec2` (interpreting the offset as a position). -/
def ofVec2 (v : Vec2) : Point := ⟨v.x, v.y⟩

instance : HAdd Point Vec2 Point := ⟨translate⟩
