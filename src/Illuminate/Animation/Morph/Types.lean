/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/

import Illuminate.Diagram.Basic
import Illuminate.Animation.Morph.CubicSeg


namespace Illuminate

/-- An axis-aligned bounding box for text clip regions. -/
structure BBox where
  /-- Half-width (extent in the x direction from center). -/
  hw : Float
  /-- Half-height (extent in the y direction from center). -/
  hh : Float
deriving Repr, BEq, Inhabited

/--
A pre-computed morph plan node.

Each variant encodes a specific interpolation strategy with all expensive
decisions (normalization, matching, alignment) already resolved. The
evaluate function walks this tree doing only cheap lerps.
-/
inductive MorphNode (β : Type) where
  /-- Both sides are empty. -/
  | empty
  /-- Interpolates matched path primitives with pre-equalized cubic segments. -/
  | path (segsA segsB : Array CubicSeg) (closed : Bool)
      (fillA fillB : ResolvedFill) (strokeA strokeB : Stroke)
  /-- Cross-fades text inside a lerped bounding-box clip region. -/
  | text (textA : String) (styleA : TextStyle) (textB : String) (styleB : TextStyle)
      (bboxA bboxB : BBox)
  /-- Interpolates a transform wrapper around a child morph node. -/
  | transform (matA matB : Matrix) (child : MorphNode β)
  /-- Overlays two child morph nodes (structural match of {name}`Diagram.compose`). -/
  | compose (left right : MorphNode β)
  /-- Interpolates cellophane (opacity) around a child morph node. -/
  | cellophane (opA opB : Float) (child : MorphNode β)
  /-- Interpolates a clip region around a child morph node. -/
  | clip (segsA segsB : Array CubicSeg) (closed : Bool) (child : MorphNode β)
  /-- Matched named subdiagrams: lerps the extracted transforms, recurses on content. -/
  | matched (name : Lean.Name) (xformA xformB : Matrix) (child : MorphNode β)
  /-- Fades in a diagram that only exists in the target. -/
  | fadeIn (d : Diagram β)
  /-- Fades out a diagram that only exists in the source. -/
  | fadeOut (d : Diagram β)
  /-- Interpolates an arrow by re-resolving endpoints from the morphed child at each t. -/
  | arrow (startA startB : LineEnd) (stopA stopB : LineEnd)
      (strokeA strokeB : Stroke) (useTraceA useTraceB : Bool) (child : MorphNode β)
  /-- A positioned name placeholder for arrow endpoint resolution. -/
  | nameRef (name : Lean.Name)
  /-- Cross-fades two structurally incompatible subtrees. -/
  | crossFade (a b : Diagram β)

instance {β : Type} : Inhabited (MorphNode β) := ⟨.empty⟩

/-- A pre-computed morph plan between two diagrams. -/
structure Morph (β : Type) where
  /-- The root of the morph plan tree. -/
  node : MorphNode β
  /-- The source diagram, returned at {lit}`t = 0` for pixel-perfect keyframes. -/
  source : Diagram β
  /-- The target diagram, returned at {lit}`t = 1` for pixel-perfect keyframes. -/
  target : Diagram β
