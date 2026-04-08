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
  | .defGradient .. => 13

/-!
# Structural comparison
-/

/-- Checks whether two draw command arrays have the same structural tags. -/
private def structurallyIdentical {β : Type}
    (a b : Array (DrawCmd β)) : Bool :=
  a.size == b.size &&
  (Array.zip a b).all fun (ca, cb) => drawCmdTag ca == drawCmdTag cb

/-!
# Template extraction for a single segment
-/

/--
Extracts a param map and per-frame parameter arrays from structurally identical draw lists.

{given -show}`paramMap : Array ParamBinding, params : Array (Array String), i : Nat, frame : Nat`
{given -show}`h : i < paramMap.size`
{given -show}`h : frame < params.size`
{given -show}`h : i < params[frame].size`
Returns {lean}`(paramMap, params)` where:
- {lean}`paramMap[i]` maps param {name}`i` to an SVG element index and attribute name
- {lean}`params[frame][i]` is the string value of param {lean}`i` for that frame
-/
private def extractParams {β : Type} [BackendRender β]
    (frames : Array (Array (DrawCmd β))) :
    Array ParamBinding × Array (Array String) :=
  if h : frames.size = 0 then
    (#[], #[])
  else Id.run do
    let cmdCount := frames[0].size
    -- Precompute attrs for every (frame, cmdIdx) pair once via the shared
    -- Svg.drawCmdAttrs, so attribute values are computed in one place.
    let allAttrs : Array (Vector (Array (String × String)) cmdCount) :=
      frames.map fun frame =>
        Vector.ofFn fun (⟨i, _⟩ : Fin cmdCount) =>
          match frame[i]? with
          | some cmd => (Svg.drawCmdAttrs cmd).attrs
          | none => #[]
    let firstFrame := frames[0]
    have : allAttrs.size > 0 := by grind
    let firstFrameAttrs := allAttrs[0]
    let mut paramMap : Array ParamBinding := #[]
    let mut varyingSlots : Vector (Array Nat) cmdCount :=
      Vector.ofFn fun _ => #[]
    let mut elemIdx : Nat := 0

    for hCmd : cmdIdx in 0...cmdCount do
      let producesElem := (Svg.drawCmdAttrs firstFrame[cmdIdx]).producesElement
      let curElemIdx := if producesElem then some elemIdx else none
      if producesElem then elemIdx := elemIdx + 1
      let firstAttrs := firstFrameAttrs[cmdIdx]
      let mut cmdVarying : Array Nat := #[]
      for hF : fieldIdx in 0...firstAttrs.size do
        let (svgAttr, firstVal) := firstAttrs[fieldIdx]
        -- Fields appear in the same order for structurally identical commands,
        -- so we compare by position rather than searching by name.
        let varies := allAttrs.any fun frameAttrs =>
          match frameAttrs[cmdIdx][fieldIdx]? with
          | some (_, v) => v != firstVal
          | none => true
        if varies then
          cmdVarying := cmdVarying.push fieldIdx
          match curElemIdx with
          | some eidx =>
            paramMap := paramMap.push { elemIdx := eidx, attr := svgAttr }
          | none => pure ()
      varyingSlots := varyingSlots.set cmdIdx cmdVarying

    -- Build per-frame parameter arrays from precomputed attrs
    let mut allParams : Array (Array String) := #[]
    for frameAttrs in allAttrs do
      let mut frameParams : Array String := #[]
      for hCmd : cmdIdx in 0...cmdCount do
        have hLt : cmdIdx < cmdCount := by get_elem_tactic
        let attrs := frameAttrs[cmdIdx]
        for fieldIdx in (varyingSlots[cmdIdx]) do
          let val := match attrs[fieldIdx]? with
            | some (_, v) => v
            | none => ""
          frameParams := frameParams.push val
      allParams := allParams.push frameParams

    (paramMap, allParams)

/-!
# Segmentation and full compilation
-/

/--
Compiles an animation into a {name}`CompiledAnimation`.

Evaluates the render function at every frame time to produce draw lists,
segments them by structural identity, and extracts parameterized templates.
-/
def compileAnimation (steps : List Step)
    (render : Vector Float steps.length → Diagram SVG)
    (fps : Nat := 60) : CompiledAnimation :=
  let dur := totalDuration steps
  let totalFrames := if dur <= 0 then 1
    else Nat.max 1 (Nat.min ((dur * fps.toFloat).round.toUInt64.toNat) 600000)
  -- Hash the middle frame of each step for clip-path prefix uniqueness
  let clipHash : UInt64 := Id.run do
    let mut h : UInt64 := 0
    let mut elapsed : Float := 0
    for s in steps do
      let mid := elapsed + clampNonneg s.duration / 2
      let progress := progressAt steps mid
      let vec := progressVector progress steps.length
      h := mixHash h (hash (render vec))
      elapsed := elapsed + clampNonneg s.duration
    return h
  -- Evaluate all frames, accumulating draw lists and viewBox bounds
  let padding : Float := 5
  let (frameDrawLists, unifiedViewBox) := Id.run do
    let mut drawLists : Array (Array (DrawCmd SVG)) := #[]
    let mut minX : Float := 0
    let mut maxX : Float := 0
    let mut minY : Float := 0
    let mut maxY : Float := 0
    let mut first := true
    for i in List.range totalFrames do
      let t := i.toFloat / fps.toFloat
      let progress := progressAt steps t
      let vec := progressVector progress steps.length
      let d := render vec
      drawLists := drawLists.push d.compile
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
      if first then ViewBox.fallback
      else { minX := minX, minY := minY, width := maxX - minX, height := maxY - minY }
    (drawLists, vb)
  let clipPfx := s!"{clipHash.toNat % 65536}_"
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
        let syncSvg := match frameDrawLists[segStart]? with
          | some cmds => Svg.render cmds unifiedViewBox clipPfx true
          | none => ""
        segs := segs.push {
          startFrame := segStart
          frameCount := i - segStart
          syncFrame := syncSvg
          paramMap := pmap
          params
        }
        segStart := i
    -- Close final segment
    if segStart < totalFrames then
      let segFrames := frameDrawLists.extract segStart totalFrames
      let (pmap, params) := extractParams segFrames
      let syncSvg := match frameDrawLists[segStart]? with
        | some cmds => Svg.render cmds unifiedViewBox clipPfx true
        | none => ""
      segs := segs.push {
        startFrame := segStart
        frameCount := totalFrames - segStart
        syncFrame := syncSvg
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
