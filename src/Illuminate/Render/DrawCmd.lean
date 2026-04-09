/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/
module
import Lean.DocString.Syntax
public import Illuminate.Style.Types
meta import Lean.Parser.Term
public section


namespace Illuminate

/-- The display list: drawing commands parameterized by a backend-specific foreign type. -/
inductive DrawCmd (β : Type) where
  /--
  Fills a path with the given fill style. The optional {name}`Nat` is the gradient index assigned
  during compilation. It ties this command to its corresponding {name (full := DrawCmd.defGradient)}`defGradient` so that
  {name (scope := "Illuminate.Render.Svg")}`drawCmdAttrs` can emit the correct {lit}`url(#...)` fill
  without relying on command ordering.
  -/
  | fillPath : PathData → ResolvedFill → Option Nat → DrawCmd β
  /-- Strokes a path with the given stroke style. -/
  | strokePath : PathData → Stroke → DrawCmd β
  /-- Draws a text string at the given position. -/
  | drawTextRun : String → TextStyle → Vec2 → DrawCmd β
  /-- Draws styled text with per-span font and color at the given position. -/
  | drawStyledText : Array (Array (FontStyle × String)) → TextAnchor → Vec2 → DrawCmd β
  /-- Pushes an affine transform onto the graphics state stack. -/
  | pushTransform : Matrix → DrawCmd β
  /-- Pops the most recent transform from the graphics state stack. -/
  | popTransform : DrawCmd β
  /-- Pushes an annotation group with the given tag. -/
  | pushAnnotation : Nat → DrawCmd β
  /-- Pops the most recent annotation group. -/
  | popAnnotation : DrawCmd β
  /-- Pushes a group with the given opacity (0–1). -/
  | pushOpacity : Float → DrawCmd β
  /-- Pops the most recent opacity group. -/
  | popOpacity : DrawCmd β
  /-- Pushes a clip region defined by a path, with a unique ID. -/
  | pushClip : PathData → Nat → DrawCmd β
  /-- Pops the most recent clip region. -/
  | popClip : DrawCmd β
  /-- Pushes a backend-specific foreign group. -/
  | pushForeign : β → DrawCmd β
  /-- Pops the most recent foreign group. -/
  | popForeign : β → DrawCmd β
  /-- Defines a gradient in the SVG {lit}`<defs>` block, referenced by a positional ID. -/
  | defGradient : Nat → Gradient → DrawCmd β

instance {β : Type} : Inhabited (DrawCmd β) := ⟨.popTransform⟩
