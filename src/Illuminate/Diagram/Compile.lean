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
  go d #[]
where
  go (d : Diagram β) (acc : Array (DrawCmd β)) : Array (DrawCmd β) :=
    match d with
    | .empty => acc
    | .prim cp =>
      match cp with
      | .path pd fill stroke =>
        let acc :=
          match fill with
          | .solid fs => if fs.color.a > 0 then acc.push (.fillPath pd fill) else acc
          | .none => acc
        if stroke.width > 0 && stroke.color.a > 0 then acc.push (.strokePath pd stroke)
        else acc
      | .text s style =>
        acc.push (.drawTextRun s style ⟨0, 0⟩)
      | .image _ => acc
    | .foreign val d =>
      let inner := go d #[]
      acc ++ Backend.compile val inner
    | .tag n d =>
      let inner := go d #[]
      (acc.push (.pushAnnotation n) ++ inner).push .popAnnotation
    | .named _ d => go d acc
    | .transform m d =>
      let inner := go d #[]
      (acc.push (.pushTransform m) ++ inner).push .popTransform
    | .compose a b =>
      let acc := go a acc
      go b acc
    | .withEnv _ d => go d acc
    | .warning _ d => go d acc
    | .cellophane α d =>
      let inner := go d #[]
      (acc.push (.pushOpacity α) ++ inner).push .popOpacity
    | .clip pd d =>
      let clipId := acc.size
      let inner := go d #[]
      (acc.push (.pushClip pd clipId) ++ inner).push .popClip

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
