/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/


namespace Illuminate

/-- A single step in an animation timeline. -/
structure Step where
  /-- Duration of this step in seconds. Zero or negative durations are skipped. -/
  duration : Float
  /-- Wraps progress around for looping effects (spinners, pulses). -/
  loop : Bool := false
  /-- Pauses playback until user interaction (click/tap/keypress). -/
  pause : Bool := false
deriving Repr, BEq, Inhabited

/-- Maps a parameter index to a specific SVG DOM element and attribute. -/
structure ParamBinding where
  /-- Index of the SVG DOM element in depth-first order (skipping close tags). -/
  elemIdx : Nat
  /-- SVG attribute name to update (e.g., "fill", "transform", "d"). -/
  attr : String
deriving Repr, BEq, Inhabited

/-- A segment of consecutive frames with identical draw list structure. -/
structure Segment where
  /-- Index of the first frame in this segment. -/
  startFrame : Nat
  /-- Number of frames in this segment. -/
  frameCount : Nat
  /-- Full SVG string for the first frame (sync frame for random access). -/
  syncFrame : String
  /-- Maps each parameter index to an SVG element index and attribute name. -/
  paramMap : Array ParamBinding
  /-- Per-frame parameter values; {lit}`params[i][j]` is the {lit}`j`-th parameter for frame {lit}`i`. -/
  params : Array (Array String)
deriving Repr, Inhabited

/-- Step metadata in a compiled animation, mapping steps to frame indices. -/
structure StepInfo where
  /-- Frame index where this step begins. -/
  frame : Nat
  /-- Whether this step pauses for user interaction. -/
  pause : Bool
  /-- Whether this step loops continuously. -/
  loop : Bool
deriving Repr, BEq, Inhabited

/-- A fully compiled animation ready for HTML rendering. -/
structure CompiledAnimation where
  /-- Frames per second used during compilation. -/
  fps : Nat
  /-- Total number of frames. -/
  totalFrames : Nat
  /-- Segments of structurally identical consecutive frames. -/
  segments : Array Segment
  /-- Step boundary metadata for pause-driven navigation. -/
  steps : Array StepInfo
deriving Repr, Inhabited
