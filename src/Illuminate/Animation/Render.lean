/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/

import Lean.Data.Json
import Illuminate.Animation.Types


namespace Illuminate

-- ═══════════════════════════════════════════════════════════════
-- JSON serialization using Lean.Json
-- ═══════════════════════════════════════════════════════════════

open Lean in
/-- Serializes a ParamBinding to a Lean JSON value. -/
private def paramBindingToJson (pb : ParamBinding) : Json :=
  .mkObj [("e", .num pb.elemIdx), ("a", .str pb.attr)]

open Lean in
/-- Serializes a Segment to a Lean JSON value. -/
private def segmentToJson (seg : Segment) : Json :=
  let pmap := seg.paramMap.map paramBindingToJson
  let params := seg.params.map fun frameParams =>
    Json.arr (frameParams.map Json.str)
  .mkObj [
    ("sf", .num seg.startFrame),
    ("fc", .num seg.frameCount),
    ("sync", .str seg.syncFrame),
    ("pmap", .arr pmap),
    ("params", .arr params)]

open Lean in
/-- Serializes a StepInfo to a Lean JSON value. -/
private def stepInfoToJson (si : StepInfo) : Json :=
  .mkObj [("frame", .num si.frame), ("pause", .bool si.pause), ("loop", .bool si.loop)]

open Lean in
/-- Serializes a CompiledAnimation to a Lean JSON value. -/
def compiledAnimationToLeanJson (ca : CompiledAnimation) : Json :=
  let segs := ca.segments.map segmentToJson
  let steps := ca.steps.map stepInfoToJson
  .mkObj [
    ("fps", .num ca.fps),
    ("totalFrames", .num ca.totalFrames),
    ("segments", .arr segs),
    ("steps", .arr steps)]

open Lean in
/-- Serializes a CompiledAnimation to a JSON string safe for embedding in `<script>` tags. -/
def compiledAnimationToJson (ca : CompiledAnimation) : String :=
  toString (compiledAnimationToLeanJson ca) |>.replace "</" "<\\/"

-- ═══════════════════════════════════════════════════════════════
-- HTML player
-- ═══════════════════════════════════════════════════════════════

/-- Escapes a string for safe inclusion in a JavaScript single-quoted string literal. -/
private def escapeJs (s : String) : String :=
  s.foldl (init := "") fun acc c =>
    acc ++ match c with
    | '\\' => "\\\\"
    | '\'' => "\\'"
    | '\n' => "\\n"
    | '\r' => "\\r"
    | '\t' => "\\t"
    | c => c.toString

/-- Shared animation helper functions used by all player variants. -/
def animCoreJs : String :=
  include_str "../../../player_js/anim_core.js"

/--
The JavaScript player code for standalone SVG DOM playback.

Uses `__DATA__` and `__SELECTOR__` as placeholders that callers replace
with the serialized animation JSON and a CSS selector string, respectively.
-/
private def playerJs : String :=
  animCoreJs ++ "\n" ++ include_str "../../../player_js/standalone.js"

/--
The JavaScript player code for reveal.js fragment-driven playback.

Uses `__DATA__` and `__SELECTOR__` as placeholders.
-/
private def revealJs : String :=
  animCoreJs ++ "\n" ++ include_str "../../../player_js/reveal.js"

/-- Renders a `CompiledAnimation` to a self-contained HTML file. -/
def CompiledAnimation.renderHTML (ca : CompiledAnimation)
    (selector : String := "#anim-container") : String :=
  let dataJson := compiledAnimationToJson ca
  let js := playerJs
    |>.replace "__DATA__" dataJson
    |>.replace "__SELECTOR__" (escapeJs selector)
  s!"<!DOCTYPE html>
<html>
<head>
<meta charset=\"utf-8\">
<style>
body \{ margin: 0; display: flex; flex-direction: column; align-items: center; justify-content: center; min-height: 100vh; background: #f5f5f5; font-family: sans-serif; }
#anim-container \{ background: white; border: 1px solid #ddd; border-radius: 4px; padding: 10px; width: min(400px, 90vw); }
#anim-container svg \{ display: block; width: 100%; height: auto; max-width: 90vw; max-height: 70vh; }
.controls \{ margin-top: 12px; display: flex; align-items: center; gap: 8px; }
#anim-play \{ font-size: 18px; width: 36px; height: 36px; border: 1px solid #ccc; border-radius: 4px; background: white; cursor: pointer; }
#anim-scrub \{ width: 300px; }
</style>
</head>
<body>
<div id=\"anim-container\"></div>
<div class=\"controls\">
  <button id=\"anim-play\" aria-label=\"Play\">\u25B6</button>
  <input type=\"range\" id=\"anim-scrub\" min=\"0\" value=\"0\" aria-label=\"Animation progress\">
</div>
<script>
{js}
</script>
</body>
</html>"

/-- Renders a `CompiledAnimation` as a `<script>` snippet that initializes playback into
    the first element matching the given CSS selector. Designed for embedding in reveal.js
    slides or any page with multiple animations. -/
def CompiledAnimation.renderRevealHTML (ca : CompiledAnimation)
    (selector : String) : String :=
  let dataJson := compiledAnimationToJson ca
  let sel := escapeJs selector
  let js := revealJs
    |>.replace "__DATA__" dataJson
    |>.replace "__SELECTOR__" sel
  s!"<script>\n{js}\n</script>"
