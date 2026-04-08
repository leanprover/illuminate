/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/
module
public import Illuminate.Animation.Morph.Types
public import Illuminate.Animation.Morph.Style
import Illuminate.Animation.Interpolate
import Lean.DocString.Syntax
import Illuminate.Geometry.PathData
meta import Lean.Parser.Term
public section


namespace Illuminate

/-- Lerps an array of pre-equalized cubic segments at parameter {name}`t`. -/
private def lerpSegs (a b : Array CubicSeg) (t : Float) : Array CubicSeg :=
  Array.zipWith (fun (x : CubicSeg) (y : CubicSeg) => CubicSeg.lerp x y t) a b

/-- Lerps two bounding boxes at parameter {name}`t`. -/
private def lerpBBox (a b : BBox) (t : Float) : BBox :=
  { hw := a.hw + t * (b.hw - a.hw)
    hh := a.hh + t * (b.hh - a.hh) }

/-- Converts a {name}`BBox` to a rectangular {name}`PathData` for clipping. -/
private def bboxToPathData (bb : BBox) : PathData :=
  PathData.rect (2 * bb.hw) (2 * bb.hh)

/--
Evaluates a morph plan node at parameter {name}`t`, producing a diagram.

This is the cheap per-frame operation. All expensive work (normalization,
matching, alignment) was done during the prepare phase.
-/
def evalMorphNode {β : Type} [Backend β] (node : MorphNode β) (t : Float) : Diagram β :=
  match node with
  | .empty => .empty
  | .path segsA segsB closed fillA fillB strokeA strokeB =>
    let segs := lerpSegs segsA segsB t
    let pd := cubicsToPathData segs closed
    let fill := match Morph.lerpResolvedFill fillA fillB t with
      | some f => f
      | none =>
        -- Cross-type fills: discrete switch at t=0.5
        if t < 0.5 then fillA else fillB
    let stroke := Morph.lerpStroke strokeA strokeB t
    .prim (.path pd (.resolved fill) stroke)
  | .text textA styleA textB styleB bboxA bboxB =>
    let bb := lerpBBox bboxA bboxB t
    let clipPd := bboxToPathData bb
    let stA := .prim (.text textA styleA)
    let stB := .prim (.text textB styleB)
    .clip clipPd (.compose (.cellophane (1 - t) stA) (.cellophane t stB))
  | .transform matA matB child =>
    .transform (Interpolate.interpolate matA matB t) (evalMorphNode child t)
  | .compose left right =>
    .compose (evalMorphNode left t) (evalMorphNode right t)
  | .cellophane opA opB child =>
    .cellophane (opA + t * (opB - opA)) (evalMorphNode child t)
  | .clip segsA segsB closed child =>
    let segs := lerpSegs segsA segsB t
    .clip (cubicsToPathData segs closed) (evalMorphNode child t)
  | .matched name xformA xformB child =>
    .transform (Interpolate.interpolate xformA xformB t) (.named name (evalMorphNode child t))
  | .nameRef _name =>
    -- The corresponding matched node provides the name and content;
    -- the ref is just a structural placeholder in the residual.
    .empty
  | .arrow startA startB stopA stopB strokeA strokeB useTraceA useTraceB child =>
    let useTrace := useTraceA || useTraceB
    let evalChild := evalMorphNode child t
    let lerpAngle (a b : Option Float) : Option Float :=
      match a, b with
      | some aa, some ab => some (aa + t * (ab - aa))
      | some aa, none => if t < 0.5 then some aa else none
      | none, some ab => if t < 0.5 then none else some ab
      | none, none => none
    let lerpLE (a b : LineEnd) : LineEnd :=
      { point := if t < 0.5 then a.point else b.point
        shift := Interpolate.interpolate a.shift b.shift t
        angle := lerpAngle a.angle b.angle
        pull := a.pull + t * (b.pull - a.pull)
        arrowhead := if t < 0.5 then a.arrowhead else b.arrowhead }
    .arrow (lerpLE startA startB) (lerpLE stopA stopB)
      (Morph.lerpStroke strokeA strokeB t) useTrace evalChild
  | .fadeIn d => .cellophane t d
  | .fadeOut d => .cellophane (1 - t) d
  | .crossFade a b => .compose (.cellophane (1 - t) a) (.cellophane t b)

/-- Evaluates a morph plan at parameter {name}`t`, producing a diagram. At {lit}`t = 0` and {lit}`t = 1`, returns the original source and target diagrams for pixel-perfect keyframes. -/
def Morph.evaluate {β : Type} [Backend β] (m : Morph β) (t : Float) : Diagram β :=
  if t <= 0.0 then m.source
  else if t >= 1.0 then m.target
  else evalMorphNode m.node t
