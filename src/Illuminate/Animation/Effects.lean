/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/

import Illuminate.Diagram.Basic
import Illuminate.Geometry.Matrix
import Illuminate.Geometry.Vec2
import Illuminate.Animation.Interpolate


namespace Illuminate

/-- Fades a diagram in from transparent to opaque. -/
def fadeIn {β : Type} (d : Diagram β) (t : Float) : Diagram β :=
  .cellophane t d

/-- Fades a diagram out from opaque to transparent. -/
def fadeOut {β : Type} (d : Diagram β) (t : Float) : Diagram β :=
  .cellophane (1 - t) d

/-- Cross-fades from one diagram to another. -/
def crossFade {β : Type} (a b : Diagram β) (t : Float) : Diagram β :=
  .compose (.cellophane (1 - t) a) (.cellophane t b)

/-- Slides a diagram from position {lit}`start` to position {lit}`finish`. -/
def slide {β : Type} (d : Diagram β) (start finish : Vec2) (t : Float) : Diagram β :=
  let pos := Interpolate.interpolate start finish t
  .transform (Matrix.translate pos.x pos.y) d

/-- Scales a diagram from factor {lit}`s0` to factor {lit}`s1`. -/
def animScale {β : Type} (d : Diagram β) (s0 s1 : Float) (t : Float) : Diagram β :=
  let s := Interpolate.interpolate s0 s1 t
  .transform (Matrix.uniformScale s) d

/-- Rotates a diagram from angle {lit}`a0` to angle {lit}`a1` (radians). -/
def animRotate {β : Type} (d : Diagram β) (a0 a1 : Float) (t : Float) : Diagram β :=
  let a := Interpolate.interpolate a0 a1 t
  .transform (Matrix.rotate a) d
