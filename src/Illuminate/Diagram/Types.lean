/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/
module
import Illuminate.Geometry.Envelope
public import Illuminate.Geometry.Trace
public import Illuminate.Style.Arrow
public import Illuminate.Render.DrawCmd
import Lean.DocString.Syntax
meta import Lean.Parser.Term
public section

namespace Illuminate

/-- Reference to an image resource. -/
structure ImageRef where
  /-- File path or URL of the image. -/
  path : String
  /-- Display width in diagram units. -/
  width : Float
  /-- Display height in diagram units. -/
  height : Float
deriving Repr, BEq, Inhabited, Hashable

/-- A backend-independent primitive that can be rendered by any backend. -/
inductive CorePrimitive where
  /-- Draws a filled and/or stroked path. {name}`none` fields inherit from config. -/
  | path : PathData → Fill → Stroke → CorePrimitive
  /-- Renders a text string. {name}`none` fields inherit style from config. -/
  | text : String → TextStyle → CorePrimitive
  /-- Renders styled text with per-span font and color. The outer array is lines; each inner array is spans within a line. -/
  | styledText : Array (Array (FontStyle × String)) → TextAnchor → CorePrimitive
  /-- References an external image resource for raster or vector embedding. -/
  | image : ImageRef → CorePrimitive
deriving Repr, BEq, Inhabited, Hashable

/-- One endpoint of an arrow, specifying where and how the line departs or arrives. -/
structure LineEnd where
  /-- Named anchor point in the diagram to connect. -/
  point : Lean.Name
  /-- Additional offset applied to the resolved anchor position. -/
  shift : Vec2 := 0
  /-- Departure/arrival angle in radians. Defaults to the straight-line angle. -/
  angle : Option Float := none
  /-- Controls how far the Bézier control point extends from this endpoint. -/
  pull : Float := 0.25
  /-- Optional arrowhead drawn at this endpoint. -/
  arrowhead : Option Arrowhead := none
deriving Repr, BEq, Hashable


/--
The core diagram type, parameterized by a backend-specific foreign type {name}`β`.
Backends can embed their own rendering objects alongside built-in primitives
by instantiating {name}`β`. Pure geometric diagrams use {name}`Empty` for this parameter.
-/
inductive Diagram (β : Type) where
  /-- Represents the empty diagram: renders nothing and has a zero envelope. -/
  | empty : Diagram β
  /-- Wraps a single core primitive (path, text, or image) as a leaf node. -/
  | prim : CorePrimitive → Diagram β
  /-- Wraps a sub-diagram with a backend-specific foreign value. -/
  | foreign : β → Diagram β → Diagram β
  /-- Attaches a numeric tag to a sub-diagram for hit-testing or interactivity. -/
  | tag : Nat → Diagram β → Diagram β
  /-- Gives a sub-diagram a hierarchical name with cardinal anchor points derived from its envelope. -/
  | named : Lean.Name → Diagram β → Diagram β
  /-- Applies an affine transformation (translate, rotate, scale) to a sub-diagram. -/
  | transform : Matrix → Diagram β → Diagram β
  /-- Overlays two diagrams, sharing the same origin. -/
  | compose : Diagram β → Diagram β → Diagram β
  /-- Overrides the envelope of a sub-diagram without changing its visual content. -/
  | withEnv : Envelope → Diagram β → Diagram β
  /-- Embeds a validation warning message alongside a sub-diagram. -/
  | warning : String → Diagram β → Diagram β
  /-- Wraps a sub-diagram with a given opacity (0–1). -/
  | cellophane : Float → Diagram β → Diagram β
  /-- Clips a sub-diagram to a path boundary. -/
  | clip : PathData → Diagram β → Diagram β
  /-- Draws an arrow between two named anchors in a sub-diagram. When {name}`useTrace` is true, endpoints are resolved via trace-based boundary detection instead of named anchor positions. -/
  | arrow (start stop : LineEnd) (stroke : Stroke) (useTrace : Bool) (child : Diagram β) : Diagram β
  /-- Overlays a translucent envelope-boundary polygon, resolved at compile time when the scale is known. -/
  | showEnv : Nat → Color → Float → Diagram β → Diagram β
deriving Hashable

/--
Controls how a backend-specific foreign value interacts with the diagram pipeline.
The {name (full := Backend.envelope)}`envelope` and {name (full := Backend.trace)}`trace` methods adjust the inner diagram's geometry.
The {name (full := Backend.compile)}`compile` method wraps or replaces the compiled inner drawing commands.
-/
class Backend (β : Type) where
  /-- Adjusts the inner diagram's envelope. Receives the inner envelope, returns the final one. -/
  envelope : β → Envelope → Envelope
  /-- Adjusts the inner diagram's trace. Receives the inner trace, returns the final one. -/
  trace : β → Trace → Trace
  /-- Adjusts the inner diagram's stroke trace. Receives the inner stroke trace, returns the final one. -/
  strokeTrace : β → StrokeTrace → StrokeTrace
  /-- Wraps or replaces the compiled inner drawing commands. -/
  compile : β → Array (DrawCmd β) → Array (DrawCmd β)

/-- The result of a point hit test against a diagram. -/
inductive Click where
  /-- The point did not hit anything. -/
  | nothing
  /-- The point hit an untagged primitive. -/
  | something
  /-- The point hit a primitive inside a tag annotation with the given ID. -/
  | tag : Nat → Click
deriving Repr, BEq, Inhabited

namespace Diagram

/-- An anchor point with a name and diagram-unit offset. -/
structure AnchorPos where
  /-- Hierarchical name for the anchor. -/
  name : Lean.Name
  /-- Offset from the origin in diagram units. -/
  offset : Vec2

end Diagram

/-- Structural errors detected during validation. -/
inductive DiagramError where
  /-- Indicates two sub-diagrams share the same hierarchical name. -/
  | duplicateName : Lean.Name → DiagramError
  /-- Indicates a path primitive with invalid or empty command data. -/
  | malformedPath : String → DiagramError
deriving Repr, BEq
