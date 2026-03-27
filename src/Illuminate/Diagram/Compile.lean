/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/

import Illuminate.Diagram.Placement
import Illuminate.Diagram.Arrow
import Illuminate.Render.DrawCmd
import Illuminate.Render.Svg

namespace Illuminate

namespace Diagram

variable {β : Type} [Backend β]

/-- Compiles a diagram tree into a flat display list of drawing commands. -/
partial def compile (d : Diagram β) (scale : Float := 0) : Array (DrawCmd β) :=
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
        if (stroke.width.diag > 0 || stroke.width.px > 0) && stroke.color.a > 0 then
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
    | .pxTranslate pxOffset d =>
      if scale == 0 then go d acc gi ci
      else
        let duOffset := scale • pxOffset
        let m := Matrix.translate duOffset.x duOffset.y
        let (inner, gi, ci) := go d #[] gi ci
        ((acc.push (.pushTransform m) ++ inner).push .popTransform, gi, ci)
    | .arrow start stop stroke useTrace d =>
      let (src, tgt, stop') :=
        if useTrace then
          -- Trace-based boundary detection (connectEdge style)
          let srcSub := d.find start.point
          let tgtSub := d.find stop.point
          let srcCenter := srcSub.origin.toVec2
          let tgtCenter := tgtSub.origin.toVec2
          let defaultDir := (tgtCenter - srcCenter).normalize
          let srcDir := match start.angle with
            | some a => Vec2.mk (Float.cos a) (Float.sin a)
            | none => defaultDir
          let tgtDir := match stop.angle with
            | some a => Vec2.mk (Float.cos a) (Float.sin a)
            | none => -defaultDir
          let srcTrace := srcSub.getStrokeTrace
          let tgtTrace := tgtSub.getStrokeTrace
          -- Offset arrowhead tips: latex (open) heads need miter protrusion offset;
          -- filled heads (stealth/triangle/circle) have no stroke so need no offset.
          let resolvedW := stroke.width.resolve scale
          let tipOffset (ah? : Option Arrowhead) : Float := match ah? with
            | some ah =>
              match ah.type with
              | .latex =>
                let halfAngle := 0.4 * ah.width
                if halfAngle > 0.01 then resolvedW / (2 * Float.sin halfAngle)
                else resolvedW / 2
              | _ => 0
            | none => 0
          let src := match srcTrace.closest (Point.ofVec2 srcCenter) srcDir with
            | some hit => srcCenter + (hit.edge + hit.width + tipOffset start.arrowhead) • srcDir + start.shift
            | none => srcCenter + start.shift
          let tgt := match tgtTrace.closest (Point.ofVec2 tgtCenter) tgtDir with
            | some hit => tgtCenter + (hit.edge + hit.width + tipOffset stop.arrowhead) • tgtDir + stop.shift
            | none => tgtCenter + stop.shift
          -- Flip stop angle for arrival tangent (outward → inward)
          let stop' := match stop.angle with
            | some a => { stop with angle := some (a + pi) }
            | none => stop
          (src, tgt, stop')
        else
          -- Anchor-based resolution (connect style)
          let srcPoint := (d.find start.point).origin
          let tgtPoint := (d.find stop.point).origin
          (srcPoint.toVec2 + start.shift, tgtPoint.toVec2 + stop.shift, stop)
      let arrowDiagram := ArrowDraw.drawLine src tgt start
        { stop' with arrowhead := stop'.arrowhead } stroke
      let (innerCmds, gi, ci) := go d acc gi ci
      let (arrowCmds, gi, ci) := go arrowDiagram #[] gi ci
      (innerCmds ++ arrowCmds, gi, ci)

/--
Renders a diagram to an SVG string.

The {name}`viewBoxPixelWidth` parameter specifies the intended display width in pixels.
This is used to compute the diagram-units-per-pixel scale for resolving pixel-valued
lengths. When 0, pixel components are ignored (backward-compatible default).
-/
def renderDiagram [BackendRender β] [Hashable β] (d : Diagram β)
    (padding : Float := 2) (viewBoxPixelWidth : Float := 0) : String :=
  let pfx := s!"{(hash d).toNat % 65536}_"
  if let .nonempty env := d.getEnvelope then
    -- Compute diag and px extents separately
    let eastL := env Vec2.east
    let westL := env Vec2.west
    let northL := env Vec2.north
    let southL := env Vec2.south
    let diagW := westL.diag + eastL.diag + 2 * padding
    let pxW := westL.px + eastL.px
    -- Closed-form scale: scale = D_diag / (vpw - D_px)
    let scale :=
      if viewBoxPixelWidth > 0 && viewBoxPixelWidth > pxW then
        diagW / (viewBoxPixelWidth - pxW)
      else 0
    -- Resolve envelope extents to diagram units using scale
    let east := eastL.resolve scale
    let west := westL.resolve scale
    let north := northL.resolve scale
    let south := southL.resolve scale
    let minX := -(west + padding)
    let minY := -(north + padding)
    let width := west + east + 2 * padding
    let height := north + south + 2 * padding
    let cmds := d.compile scale
    Svg.render cmds { minX, minY, width, height } pfx false scale
  else
    let cmds := d.warning "Diagram has no envelope, defaulting to 640x480" |>.compile
    Svg.render cmds ViewBox.fallback pfx
