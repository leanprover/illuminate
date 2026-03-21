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
  let pmap := seg.paramMap.toList.map paramBindingToJson
  let params := seg.params.toList.map fun frameParams =>
    Json.arr (frameParams.map Json.str)
  let svgs := seg.frameSvgs.toList.map Json.str
  .mkObj [
    ("sf", .num seg.startFrame),
    ("fc", .num seg.frameCount),
    ("sync", .str seg.syncFrame),
    ("svgs", .arr svgs.toArray),
    ("pmap", .arr pmap.toArray),
    ("params", .arr params.toArray)]

open Lean in
/-- Serializes a StepInfo to a Lean JSON value. -/
private def stepInfoToJson (si : StepInfo) : Json :=
  .mkObj [("frame", .num si.frame), ("pause", .bool si.pause), ("loop", .bool si.loop)]

open Lean in
/-- Serializes a CompiledAnimation to a Lean JSON value. -/
def compiledAnimationToLeanJson (ca : CompiledAnimation) : Json :=
  let segs := ca.segments.toList.map segmentToJson
  let steps := ca.steps.toList.map stepInfoToJson
  .mkObj [
    ("fps", .num ca.fps),
    ("totalFrames", .num ca.totalFrames),
    ("segments", .arr segs.toArray),
    ("steps", .arr steps.toArray)]

open Lean in
/-- Serializes a CompiledAnimation to a JSON string. -/
def compiledAnimationToJson (ca : CompiledAnimation) : String :=
  toString (compiledAnimationToLeanJson ca)

-- ═══════════════════════════════════════════════════════════════
-- HTML player
-- ═══════════════════════════════════════════════════════════════

/-- The JavaScript player code for SVG DOM playback. -/
private def playerJs : String :=
  "
(function() {
  var data = window.__ANIM_DATA__;
  var container = document.getElementById('anim-container');
  var playBtn = document.getElementById('anim-play');
  var scrubber = document.getElementById('anim-scrub');
  var currentSeg = null;
  var playing = false;
  var startTime = null;
  var pauseFrame = 0;
  var currentStep = 0;
  var waitingForClick = false;

  scrubber.max = data.totalFrames - 1;

  function findSegment(frame) {
    for (var i = 0; i < data.segments.length; i++) {
      var s = data.segments[i];
      if (frame >= s.sf && frame < s.sf + s.fc) return s;
    }
    return data.segments[data.segments.length - 1];
  }

  function seekTo(frame) {
    frame = Math.max(0, Math.min(frame, data.totalFrames - 1));
    var seg = findSegment(frame);
    var local = frame - seg.sf;

    if (seg !== currentSeg) {
      container.innerHTML = seg.sync;
      currentSeg = seg;
    }

    if (local > 0 && seg.params.length > local) {
      container.innerHTML = seg.sync;
    }

    scrubber.value = frame;
    pauseFrame = frame;
  }

  function findCurrentStep(frame) {
    for (var i = data.steps.length - 1; i >= 0; i--) {
      if (frame >= data.steps[i].frame) return i;
    }
    return 0;
  }

  function tick(timestamp) {
    if (!playing || waitingForClick) return;
    if (startTime === null) startTime = timestamp;
    var elapsed = (timestamp - startTime) / 1000;
    var frame = pauseFrame + Math.round(elapsed * data.fps);

    if (frame >= data.totalFrames) {
      frame = data.totalFrames - 1;
      playing = false;
      playBtn.textContent = '\\u25B6';
    }

    var step = findCurrentStep(frame);
    if (step > currentStep) {
      for (var s = currentStep + 1; s <= step; s++) {
        if (data.steps[s].pause) {
          frame = data.steps[s].frame;
          waitingForClick = true;
          currentStep = s;
          seekTo(frame);
          playBtn.textContent = '\\u25B6';
          return;
        }
      }
      currentStep = step;
    }

    seekTo(frame);
    if (playing) requestAnimationFrame(tick);
  }

  function advance() {
    if (waitingForClick) {
      waitingForClick = false;
      startTime = null;
      playing = true;
      playBtn.textContent = '\\u23F8';
      requestAnimationFrame(tick);
    } else if (playing) {
      playing = false;
      playBtn.textContent = '\\u25B6';
    } else {
      if (pauseFrame >= data.totalFrames - 1) {
        pauseFrame = 0;
        currentStep = 0;
      }
      playing = true;
      startTime = null;
      playBtn.textContent = '\\u23F8';
      requestAnimationFrame(tick);
    }
  }

  playBtn.addEventListener('click', advance);
  container.addEventListener('click', function() {
    if (waitingForClick) advance();
  });
  document.addEventListener('keydown', function(e) {
    if (e.key === ' ' || e.key === 'Enter') {
      e.preventDefault();
      advance();
    }
  });

  scrubber.addEventListener('input', function() {
    playing = false;
    waitingForClick = false;
    playBtn.textContent = '\\u25B6';
    var frame = parseInt(scrubber.value);
    currentStep = findCurrentStep(frame);
    seekTo(frame);
  });

  seekTo(0);
})();
"

/-- Renders a `CompiledAnimation` to a self-contained HTML file. -/
def CompiledAnimation.renderHTML (ca : CompiledAnimation) : String :=
  let dataJson := compiledAnimationToJson ca
  s!"<!DOCTYPE html>
<html>
<head>
<meta charset=\"utf-8\">
<style>
body \{ margin: 0; display: flex; flex-direction: column; align-items: center; justify-content: center; min-height: 100vh; background: #f5f5f5; font-family: sans-serif; }
#anim-container \{ background: white; border: 1px solid #ddd; border-radius: 4px; padding: 10px; }
#anim-container svg \{ display: block; max-width: 90vw; max-height: 70vh; }
.controls \{ margin-top: 12px; display: flex; align-items: center; gap: 8px; }
#anim-play \{ font-size: 18px; width: 36px; height: 36px; border: 1px solid #ccc; border-radius: 4px; background: white; cursor: pointer; }
#anim-scrub \{ width: 300px; }
</style>
</head>
<body>
<div id=\"anim-container\"></div>
<div class=\"controls\">
  <button id=\"anim-play\">\u25B6</button>
  <input type=\"range\" id=\"anim-scrub\" min=\"0\" value=\"0\">
</div>
<script>
window.__ANIM_DATA__ = {dataJson};
{playerJs}
</script>
</body>
</html>"

/-- Renders a `CompiledAnimation` as an HTML snippet for embedding in reveal.js slides. -/
def CompiledAnimation.renderRevealHTML (ca : CompiledAnimation) : String :=
  let dataJson := compiledAnimationToJson ca
  s!"<div class=\"illuminate-anim\">
<div id=\"anim-container\"></div>
<script>
(function() \{
  window.__ANIM_DATA__ = {dataJson};
  var data = window.__ANIM_DATA__;
  var container = document.getElementById('anim-container');
  var currentSeg = null;

  function findSegment(frame) \{
    for (var i = 0; i < data.segments.length; i++) \{
      var s = data.segments[i];
      if (frame >= s.sf && frame < s.sf + s.fc) return s;
    }
    return data.segments[data.segments.length - 1];
  }

  function seekTo(frame) \{
    var seg = findSegment(frame);
    if (seg !== currentSeg) \{
      container.innerHTML = seg.sync;
      currentSeg = seg;
    }
  }

  var pauseSteps = data.steps.filter(function(s) \{ return s.pause; });
  pauseSteps.forEach(function(s, i) \{
    var frag = document.createElement('span');
    frag.className = 'fragment';
    frag.dataset.fragmentIndex = i;
    frag.style.display = 'none';
    container.parentElement.appendChild(frag);
  });

  seekTo(0);

  if (typeof Reveal !== 'undefined') \{
    Reveal.addEventListener('fragmentshown', function(e) \{
      var idx = parseInt(e.fragment.dataset.fragmentIndex);
      if (!isNaN(idx) && idx < pauseSteps.length) \{
        seekTo(pauseSteps[idx].frame);
      }
    });
    Reveal.addEventListener('fragmenthidden', function(e) \{
      var idx = parseInt(e.fragment.dataset.fragmentIndex);
      if (!isNaN(idx)) \{
        var prevFrame = idx > 0 ? pauseSteps[idx - 1].frame : 0;
        seekTo(prevFrame);
      }
    });
  }
})();
</script>
</div>"
