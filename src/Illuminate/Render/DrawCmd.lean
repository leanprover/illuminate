import Illuminate.Geometry
import Illuminate.Style


namespace Illuminate

/-- The display list: backend-agnostic drawing commands. -/
inductive DrawCmd where
  /-- Fills a path with the given fill style. -/
  | fillPath : PathData → Fill → DrawCmd
  /-- Strokes a path with the given stroke style. -/
  | strokePath : PathData → Stroke → DrawCmd
  /-- Draws a text string at the given position. -/
  | drawTextRun : String → TextStyle → Vec2 → DrawCmd
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

