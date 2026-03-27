/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/

import Illuminate.Diagram.Placement


namespace Illuminate

/-- The result of a point hit test against a diagram. -/
inductive Click where
  /-- The point did not hit anything. -/
  | nothing
  /-- The point hit an untagged primitive. -/
  | something
  /-- The point hit a primitive inside a tag annotation with the given ID. -/
  | tag : Nat → Click
deriving Repr, BEq, Inhabited

namespace Click

/-- Returns {lean}`true` if the click hit something (either tagged or untagged). -/
def isHit : Click → Bool
  | .nothing => false
  | .something => true
  | .tag _ => true

instance : ToString Click where
  toString
    | .nothing => "nothing"
    | .something => "something"
    | .tag n => s!"tag {n}"

end Click

/-!
# Point-in-path (ray casting)
-/

namespace PathData

/--
Tests whether a point lies inside a closed path using the ray-casting algorithm.
Casts a horizontal ray to the right from {name}`p` and counts boundary crossings;
an odd count means the point is inside.
-/
def contains (pd : PathData) (p : Point) : Bool :=
  let hits := (Trace.ofPathData pd.commands).query p Vec2.east
  hits.size % 2 == 1

end PathData

/-!
# Point-on-stroke check
-/

/--
Tests whether a point lies within the stroke band of a path.
Casts rays in several directions from the point; if any ray's closest
stroke hit has $`edge ≤ 0`, the point is inside the stroke band.
-/
private def pointOnStroke (pd : PathData) (strokeWidth : Float) (p : Point) : Bool :=
  if strokeWidth <= 0 then false
  else
    let st := StrokeTrace.ofPathData pd.commands strokeWidth
    let dirs : List Vec2 := [Vec2.east, Vec2.north, Vec2.west, Vec2.south,
      (Vec2.mk 1 1).normalize, (Vec2.mk 1 (-1)).normalize,
      (Vec2.mk (-1) 1).normalize, (Vec2.mk (-1) (-1)).normalize]
    dirs.any fun dir =>
      match st.closest p dir with
      | some hit => hit.edge <= 0
      | none => false

/-!
# Hit testing
-/

/--
Tests whether a point lies inside a text bounding box, accounting for anchor alignment.
-/
private def textContains (s : String) (style : TextStyle) (p : Point) : Bool :=
  let fontSize := style.fontSize
  let lines := s.splitOn "\n"
  let nLines := Max.max 1 lines.length
  let totalW := lines.foldl (fun acc line => Max.max acc (estimateTextWidth fontSize line)) 0
  let h := if nLines == 1 then fontSize / 2
           else fontSize * 1.2 * nLines.toFloat / 2
  let hw := totalW / 2
  let (left, right) : Float × Float := match style.anchor with
    | .start => (0, totalW)
    | .«end» => (-totalW, 0)
    | .middle => (-hw, hw)
  p.x >= left && p.x <= right && p.y >= -h && p.y <= h

namespace Diagram

variable {β : Type} [Backend β]

/--
Hit-tests a diagram at the given point, returning the topmost hit.

Respects front-to-back ordering: given diagrams {given}`a, b`, in {lean}`Diagram.compose a b`, {name}`b` (drawn on top) is
tested first. A {name}`ResolvedFill.solid` interior is hittable even if transparent. A
{name}`ResolvedFill.none` interior is not hittable — only its stroke boundary is.
Clicking within the border stroke of a shape counts as clicking the shape.
-/
def hitTest (d : Diagram β) (p : Point) : Click :=
  match d with
  | .empty => .nothing
  | .prim cp =>
    match cp with
    | .path pd fill stroke =>
      let fillHit := match fill with
        | .none => false
        | _ => pd.contains p
      let strokeHit := (stroke.width.diag > 0 || stroke.width.px > 0) &&
        stroke.color.a > 0 && pointOnStroke pd stroke.width.diag p
      if fillHit || strokeHit then .something else .nothing
    | .text s style =>
      if textContains s style p then .something else .nothing
    | .image ref =>
      let hw := ref.width / 2
      let hh := ref.height / 2
      if p.x >= -hw && p.x <= hw && p.y >= -hh && p.y <= hh then .something
      else .nothing
  | .foreign _ d => hitTest d p
  | .tag n d =>
    if (hitTest d p).isHit then .tag n else .nothing
  | .named _ d => hitTest d p
  | .transform m d =>
    match Matrix.inverse m with
    | none => .nothing
    | some mInv => hitTest d (Matrix.applyPoint mInv p)
  | .compose a b =>
    let bHit := hitTest b p
    if bHit.isHit then bHit else hitTest a p
  | .withEnv _ d => hitTest d p
  | .warning _ d => hitTest d p
  | .cellophane _ d => hitTest d p
  | .clip pd d =>
    if pd.contains p then hitTest d p else .nothing
  | .arrow _ _ _ _ d => hitTest d p
  | .pxTranslate _ d => hitTest d p
