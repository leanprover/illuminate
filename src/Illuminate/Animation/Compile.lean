/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/

import Std.Data.HashSet
import Illuminate.Animation.Types
import Illuminate.Animation.Animate
import Illuminate.Diagram
import Illuminate.Render
import Illuminate.Backend.SVG


namespace Illuminate

-- ═══════════════════════════════════════════════════════════════
-- DrawCmd field serialization
-- ═══════════════════════════════════════════════════════════════

/-- Returns a numeric constructor discriminant for structural comparison. -/
private def drawCmdTag {β : Type} (cmd : DrawCmd β) : UInt8 :=
  match cmd with
  | .fillPath .. => 0
  | .strokePath .. => 1
  | .drawTextRun .. => 2
  | .pushTransform .. => 3
  | .popTransform => 4
  | .pushAnnotation .. => 5
  | .popAnnotation => 6
  | .pushOpacity .. => 7
  | .popOpacity => 8
  | .pushClip .. => 9
  | .popClip => 10
  | .pushForeign .. => 11
  | .popForeign .. => 12

/-- Returns whether a draw command produces an SVG DOM element (vs a close tag or nothing). -/
private def drawCmdProducesElement {β : Type} (cmd : DrawCmd β) : Bool :=
  match cmd with
  | .fillPath _ (.solid _) => true
  | .fillPath _ .none => false
  | .strokePath .. => true
  | .drawTextRun .. => true
  | .pushTransform .. => true
  | .pushAnnotation .. => true
  | .pushOpacity .. => true
  | .pushClip .. => true
  | .pushForeign .. => true
  | .popTransform | .popAnnotation | .popOpacity | .popClip => false
  | .popForeign .. => false

/--
Extracts serializable field values from a draw command, paired with their SVG attribute names.
-/
private def drawCmdFields {β : Type} [BackendRender β]
    (cmd : DrawCmd β) : List (String × String × String) :=
  -- Returns (fieldName, svgAttrName, value)
  match cmd with
  | .fillPath pd fill =>
    let d := Svg.pathDataToD pd
    match fill with
    | .none => [("d", "d", d)]
    | .solid fs =>
      [("d", "d", d),
       ("fill", "fill", s!"rgb({fs.color.r},{fs.color.g},{fs.color.b})"),
       ("fill-opacity", "fill-opacity", toString fs.color.a)]
  | .strokePath pd stroke =>
    let d := Svg.pathDataToD pd
    [("d", "d", d),
     ("stroke", "stroke", s!"rgb({stroke.color.r},{stroke.color.g},{stroke.color.b})"),
     ("stroke-width", "stroke-width", toString stroke.width),
     ("stroke-opacity", "stroke-opacity", toString stroke.color.a)]
  | .drawTextRun s style pos =>
    [("text", "textContent", s),
     ("x", "x", toString pos.x), ("y", "y", toString pos.y),
     ("font-size", "font-size", toString style.fontSize),
     ("fill", "fill", s!"rgb({style.color.r},{style.color.g},{style.color.b})")]
  | .pushTransform m =>
    [("matrix", "transform",
      s!"matrix({m.a},{m.c},{m.b},{m.d},{m.tx},{m.ty})")]
  | .pushOpacity α => [("opacity", "opacity", toString α)]
  | .pushAnnotation tag => [("tag", "data-anno-id", toString tag)]
  | .pushClip pd clipId =>
    [("d", "d", Svg.pathDataToD pd), ("clipId", "id", toString clipId)]
  | .pushForeign f => [("foreign", "data-foreign", BackendRender.renderOpen f)]
  | .popForeign f => [("foreign", "data-foreign", BackendRender.renderClose f)]
  | .popTransform | .popAnnotation | .popOpacity | .popClip => []

-- ═══════════════════════════════════════════════════════════════
-- Structural comparison
-- ═══════════════════════════════════════════════════════════════

/-- Checks whether two draw lists have the same structural tags. -/
private def structurallyIdentical {β : Type}
    (a b : List (DrawCmd β)) : Bool :=
  a.length == b.length &&
  (a.zip b).all fun (ca, cb) => drawCmdTag ca == drawCmdTag cb

-- ═══════════════════════════════════════════════════════════════
-- Template extraction for a single segment
-- ═══════════════════════════════════════════════════════════════

/--
Extracts a param map and per-frame parameter arrays from structurally identical draw lists.

Returns `(paramMap, params)` where:
- `paramMap[i]` maps param `i` to an SVG element index and attribute name
- `params[frame][i]` is the string value of param `i` for that frame
-/
private def extractParams {β : Type} [BackendRender β]
    (frames : Array (List (DrawCmd β))) :
    Array ParamBinding × Array (Array String) :=
  if h : frames.size = 0 then
    (#[], #[])
  else Id.run do
    let cmdCount := frames[0].length
    -- Precompute fields for every (frame, cmdIdx) pair once
    let allFields : Array (Array (List (String × String × String))) :=
      frames.map fun frame =>
        let arr := frame.toArray
        Array.ofFn (n := cmdCount) fun ⟨i, _⟩ =>
          match arr[i]? with
          | some cmd => drawCmdFields cmd
          | none => []
    have : allFields.size > 0 := by grind
    let firstFrameFields := allFields[0]
    let firstArr := frames[0].toArray
    let mut paramMap : Array ParamBinding := #[]
    -- Which (cmdIdx, fieldIndex) slots vary across frames
    let mut varyingSlots : Array (Array (Nat × String)) := #[]

    -- Track SVG element index: only commands that produce elements get one
    let mut elemIdx : Nat := 0
    let mut cmdElemIdx : Array (Option Nat) := #[]

    for cmdIdx in List.range cmdCount do
      let producesElem := match firstArr[cmdIdx]? with
        | some cmd => drawCmdProducesElement cmd
        | none => false
      if producesElem then
        cmdElemIdx := cmdElemIdx.push (some elemIdx)
        elemIdx := elemIdx + 1
      else
        cmdElemIdx := cmdElemIdx.push none

      let firstFields := firstFrameFields[cmdIdx]?.getD []
      let mut cmdVarying : Array (Nat × String) := #[]
      for h : fieldIdx in [:firstFields.length] do
        let (fieldName, svgAttr, firstVal) := firstFields[fieldIdx]
        let varies := allFields.any fun frameFields =>
          match (frameFields[cmdIdx]?.getD []).find? (fun (n, _, _) => n == fieldName) with
          | some (_, _, v) => v != firstVal
          | none => true
        if varies then
          cmdVarying := cmdVarying.push (fieldIdx, fieldName)
          match cmdElemIdx[cmdIdx]? with
          | some (some eidx) =>
            paramMap := paramMap.push { elemIdx := eidx, attr := svgAttr }
          | _ => pure ()
      varyingSlots := varyingSlots.push cmdVarying

    -- Build per-frame parameter arrays from precomputed fields
    let mut allParams : Array (Array String) := #[]
    for frameFields in allFields do
      let mut frameParams : Array String := #[]
      for cmdIdx in List.range cmdCount do
        let fields := frameFields[cmdIdx]?.getD []
        for (_, fieldName) in varyingSlots[cmdIdx]?.getD #[] do
          let val := match fields.find? (fun (n, _, _) => n == fieldName) with
            | some (_, _, v) => v
            | none => ""
          frameParams := frameParams.push val
      allParams := allParams.push frameParams

    (paramMap, allParams)

-- ═══════════════════════════════════════════════════════════════
-- Segmentation and full compilation
-- ═══════════════════════════════════════════════════════════════

/--
Compiles an animation into a `CompiledAnimation`.

Evaluates the render function at every frame time to produce draw lists,
segments them by structural identity, and extracts parameterized templates.
-/
def compileAnimation (steps : List Step)
    (render : Vector Float steps.length → Diagram SVG)
    (fps : Nat := 60) : CompiledAnimation :=
  let dur := totalDuration steps
  let totalFrames := if dur <= 0 then 1 else Nat.max 1 (dur * fps.toFloat).ceil.toUInt64.toNat
  -- Evaluate all frames, accumulating draw lists, viewBox bounds, and clip hash
  let padding : Float := 5
  let (frameDrawLists, unifiedViewBox, clipHash) := Id.run do
    let mut drawLists : Array (List (DrawCmd SVG)) := #[]
    let mut minX : Float := 0
    let mut maxX : Float := 0
    let mut minY : Float := 0
    let mut maxY : Float := 0
    let mut first := true
    let mut h : UInt64 := 0
    for i in List.range totalFrames do
      let t := i.toFloat / fps.toFloat
      let progress := progressAt steps t
      let vec := progressVector progress steps.length
      let d := render vec
      drawLists := drawLists.push d.compile
      h := mixHash h (hash d)
      if let .nonempty env := d.getEnvelope then
        let east := env Vec2.east
        let west := env Vec2.west
        let north := env Vec2.north
        let south := env Vec2.south
        let fMinX := -(west + padding)
        let fMaxX := east + padding
        let fMinY := -(north + padding)
        let fMaxY := south + padding
        if first then
          minX := fMinX; maxX := fMaxX; minY := fMinY; maxY := fMaxY
          first := false
        else
          if fMinX < minX then minX := fMinX
          if fMaxX > maxX then maxX := fMaxX
          if fMinY < minY then minY := fMinY
          if fMaxY > maxY then maxY := fMaxY
    let vb : ViewBox :=
      if first then { minX := -320, minY := -240, width := 640, height := 480 }
      else { minX := minX, minY := minY, width := maxX - minX, height := maxY - minY }
    (drawLists, vb, h)
  -- Render all frames with the unified viewBox
  let clipPfx := s!"{clipHash.toNat % 65536}_"
  let allSvgs := frameDrawLists.map fun cmds => Svg.render cmds unifiedViewBox clipPfx
  -- Compute step boundary frames
  let stepFrames : Array Nat := Id.run do
    let mut arr : Array Nat := #[]
    let mut elapsed : Float := 0
    for s in steps do
      let frame := (elapsed * fps.toFloat).round.toUInt64.toNat |>.min (totalFrames - 1)
      arr := arr.push frame
      elapsed := elapsed + clampNonneg s.duration
    return arr
  let stepBoundaries : Std.HashSet Nat :=
    (0 :: stepFrames.toList).foldl (·.insert ·) {}
  -- Build segments, splitting at step boundaries and structural changes
  let segments : Array Segment := Id.run do
    let mut segs : Array Segment := #[]
    let mut segStart : Nat := 0
    for i in List.range totalFrames do
      let structChanged := match frameDrawLists[segStart]?, frameDrawLists[i]? with
        | some a, some b => !structurallyIdentical a b
        | _, _ => true
      let shouldSplit := i > segStart && (stepBoundaries.contains i || structChanged)
      if shouldSplit then
        let segFrames := frameDrawLists.extract segStart i
        let (pmap, params) := extractParams segFrames
        let syncSvg := allSvgs[segStart]?.getD ""
        let segSvgs := allSvgs.extract segStart i
        segs := segs.push {
          startFrame := segStart
          frameCount := i - segStart
          syncFrame := syncSvg
          frameSvgs := segSvgs
          paramMap := pmap
          params
        }
        segStart := i
    -- Close final segment
    if segStart < totalFrames then
      let segFrames := frameDrawLists.extract segStart totalFrames
      let (pmap, params) := extractParams segFrames
      let syncSvg := allSvgs[segStart]?.getD ""
      let segSvgs := allSvgs.extract segStart totalFrames
      segs := segs.push {
        startFrame := segStart
        frameCount := totalFrames - segStart
        syncFrame := syncSvg
        frameSvgs := segSvgs
        paramMap := pmap
        params
      }
    return segs
  -- Build step info
  let stepInfos : Array StepInfo := Id.run do
    let mut arr : Array StepInfo := #[]
    let mut idx : Nat := 0
    for s in steps do
      let frame := stepFrames[idx]?.getD 0
      arr := arr.push { frame, pause := s.pause, loop := s.loop }
      idx := idx + 1
    return arr
  { fps, totalFrames, segments, steps := stepInfos }
