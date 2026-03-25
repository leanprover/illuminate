/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/

import Illuminate.Diagram.Placement
import Illuminate.Render.DrawCmd
import Illuminate.Render.Svg

namespace Illuminate

namespace Diagram

variable {β : Type} [Backend β]

/-- Compiles a diagram tree into a flat display list of drawing commands. -/
def compile (d : Diagram β) : Array (DrawCmd β) :=
  (go d #[] 0 0).1
where
  go (d : Diagram β) (acc : Array (DrawCmd β)) (gi ci : Nat) :
      Array (DrawCmd β) × Nat × Nat :=
    match d with
    | .empty => (acc, gi, ci)
    | .prim cp =>
      match cp with
      | .path pd fill stroke =>
        let resolved : ResolvedFill := fill.resolve cp.toEnvelope
        let (acc, gi) :=
          match resolved with
          | .solid fs =>
            if fs.color.a > 0 then
              (acc.push (.fillPath pd resolved none), gi)
            else (acc, gi)
          | .gradient g =>
            let acc := acc.push (.defGradient gi g)
            (acc.push (.fillPath pd resolved (some gi)), gi + 1)
          | .none => (acc, gi)
        if stroke.width > 0 && stroke.color.a > 0 then
          (acc.push (.strokePath pd stroke), gi, ci)
        else
          (acc, gi, ci)
      | .text s style =>
        (acc.push (.drawTextRun s style ⟨0, 0⟩), gi, ci)
      | .image _ => (acc, gi, ci)
    | .foreign val d =>
      let (inner, gi, ci) := go d #[] gi ci
      (acc ++ Backend.compile val inner, gi, ci)
    | .tag n d =>
      let (inner, gi, ci) := go d #[] gi ci
      ((acc.push (.pushAnnotation n) ++ inner).push .popAnnotation, gi, ci)
    | .named _ d => go d acc gi ci
    | .transform m d =>
      let (inner, gi, ci) := go d #[] gi ci
      ((acc.push (.pushTransform m) ++ inner).push .popTransform, gi, ci)
    | .compose a b =>
      let (acc, gi, ci) := go a acc gi ci
      go b acc gi ci
    | .withEnv _ d => go d acc gi ci
    | .warning _ d => go d acc gi ci
    | .cellophane α d =>
      let (inner, gi, ci) := go d #[] gi ci
      ((acc.push (.pushOpacity α) ++ inner).push .popOpacity, gi, ci)
    | .clip pd d =>
      let (inner, gi, ci) := go d #[] gi (ci + 1)
      ((acc.push (.pushClip pd ci) ++ inner).push .popClip, gi, ci)

/-- Renders a diagram to an SVG string. -/
def renderDiagram [BackendRender β] [Hashable β] (d : Diagram β) (padding : Float := 2) : String :=
  let pfx := s!"{(hash d).toNat % 65536}_"
  if let .nonempty env := d.getEnvelope then
    let east := env Vec2.east
    let west := env Vec2.west
    let north := env Vec2.north
    let south := env Vec2.south
    let minX := -(west + padding)
    let minY := -(north + padding)
    let width := west + east + 2 * padding
    let height := north + south + 2 * padding
    let cmds := d.compile
    Svg.render cmds { minX, minY, width, height } pfx
  else
    let cmds := d.warning "Diagram has no envelope, defaulting to 640x480" |>.compile
    Svg.render cmds ViewBox.fallback pfx
