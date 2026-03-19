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

variable {β : Type}

/-- Compiles a diagram tree into a flat display list of drawing commands. -/
def compile (d : Diagram β) : List DrawCmd :=
  go d ResolvedConfig.defaults []
where
  go (d : Diagram β) (rc : ResolvedConfig) (acc : List DrawCmd) : List DrawCmd :=
    match d with
    | .empty => acc
    | .prim p =>
      match p with
      | .core (.path pd fill stroke) =>
        let fullFill := fill.resolve rc
        let fullStroke := stroke.resolve rc
        let acc := if fullFill.color.a > 0 then acc ++ [.fillPath pd fullFill] else acc
        if fullStroke.width > 0 && fullStroke.color.a > 0 then acc ++ [.strokePath pd fullStroke]
        else acc
      | .core (.text s style) =>
        let fullStyle := style.resolve rc
        acc ++ [.drawTextRun s fullStyle ⟨0, 0⟩]
      | .core (.image _) => acc
      | .foreign _ (some cp) =>
        go (.prim (.core cp)) rc acc
      | .foreign _ none => acc
    | .annotate tag d =>
      let inner := go d rc []
      acc ++ [.pushAnnotation tag] ++ inner ++ [.popAnnotation]
    | .named _ d => go d rc acc
    | .transform m d =>
      let inner := go d rc []
      acc ++ [.pushTransform m] ++ inner ++ [.popTransform]
    | .compose a b =>
      let acc := go a rc acc
      go b rc acc
    | .withEnv _ d => go d rc acc
    | .warning _ d => go d rc acc
    | .withConfig cfg d =>
      let rc' := cfg.resolve rc
      go d rc' acc
    | .cellophane α d =>
      let inner := go d rc []
      acc ++ [.pushOpacity α] ++ inner ++ [.popOpacity]
    | .clip pd d =>
      let clipId := acc.length
      let inner := go d rc []
      acc ++ [.pushClip pd clipId] ++ inner ++ [.popClip]

/-- Renders a diagram to an SVG string. -/
def renderDiagram (d : Diagram β) (padding : Float := 2) : String :=
  let env := d.getEnvelope
  let east := env Vec2.east
  let west := env Vec2.west
  let north := env Vec2.north
  let south := env Vec2.south
  let minX := -(west + padding)
  let minY := -(north + padding)
  let w := west + east + 2 * padding
  let h := north + south + 2 * padding
  let cmds := d.compile
  Svg.render cmds (minX, minY, w, h)
