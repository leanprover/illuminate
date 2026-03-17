/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/

import Illuminate.Geometry
import Illuminate.Style


namespace Illuminate

/-- The display list: backend-agnostic drawing commands. -/
inductive DrawCmd where
  /-- Fills a path with the given fill style. -/
  | fillPath : PathData → FullFill → DrawCmd
  /-- Strokes a path with the given stroke style. -/
  | strokePath : PathData → FullStroke → DrawCmd
  /-- Draws a text string at the given position. -/
  | drawTextRun : String → FullTextStyle → Vec2 → DrawCmd
  /-- Pushes an affine transform onto the graphics state stack. -/
  | pushTransform : Matrix → DrawCmd
  /-- Pops the most recent transform from the graphics state stack. -/
  | popTransform : DrawCmd
  /-- Pushes an annotation group with the given tag. -/
  | pushAnnotation : Nat → DrawCmd
  /-- Pops the most recent annotation group. -/
  | popAnnotation : DrawCmd
  /-- Pushes a group with the given opacity (0–1). -/
  | pushOpacity : Float → DrawCmd
  /-- Pops the most recent opacity group. -/
  | popOpacity : DrawCmd
  /-- Pushes a clip region defined by a path, with a unique ID. -/
  | pushClip : PathData → Nat → DrawCmd
  /-- Pops the most recent clip region. -/
  | popClip : DrawCmd
deriving Repr, BEq, Inhabited
