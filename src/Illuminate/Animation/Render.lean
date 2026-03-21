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

/-- Escapes a string for safe inclusion in a JavaScript single-quoted string literal. -/
private def escapeJs (s : String) : String :=
  s.foldl (init := "") fun acc c =>
    acc ++ match c with
    | '\\' => "\\\\"
    | '\'' => "\\'"
    | '\n' => "\\n"
    | c => c.toString

/--
The JavaScript player code for SVG DOM playback.

Uses `__DATA__` and `__SELECTOR__` as placeholders that callers replace
with the serialized animation JSON and a CSS selector string, respectively.
-/
private def playerJs : String :=
  "
(function() {
  var data = __DATA__;
  var container = document.querySelector('__SELECTOR__');
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

  function showFrame(frame) {
    frame = Math.max(0, Math.min(frame, data.totalFrames - 1));
    var seg = findSegment(frame);
    var local = frame - seg.sf;

    if (seg !== currentSeg) {
      container.innerHTML = seg.sync;
      currentSeg = seg;
    }

    if (local > 0 && seg.svgs && seg.svgs[local]) {
      container.innerHTML = seg.svgs[local];
    }

    scrubber.value = frame;
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

    // Handle looping steps: wrap frame within step boundaries
    var stepInfo = data.steps[currentStep];
    if (stepInfo && stepInfo.loop) {
      var stepStart = stepInfo.frame;
      var stepEnd = (currentStep + 1 < data.steps.length)
        ? data.steps[currentStep + 1].frame : data.totalFrames;
      var stepLen = stepEnd - stepStart;
      if (stepLen > 0 && frame >= stepEnd) {
        var overshoot = (frame - stepStart) % stepLen;
        frame = stepStart + overshoot;
        startTime = timestamp;
        pauseFrame = stepStart;
      }
    }

    if (frame >= data.totalFrames) {
      frame = data.totalFrames - 1;
      pauseFrame = frame;
      playing = false;
      playBtn.textContent = '\\u25B6';
    }

    var step = findCurrentStep(frame);
    if (step > currentStep) {
      for (var s = currentStep + 1; s <= step; s++) {
        if (data.steps[s].pause) {
          frame = data.steps[s].frame;
          pauseFrame = frame;
          waitingForClick = true;
          currentStep = s;
          showFrame(frame);
          playBtn.textContent = '\\u25B6';
          return;
        }
      }
      currentStep = step;
    }

    showFrame(frame);
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
      pauseFrame = parseInt(scrubber.value);
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
    pauseFrame = frame;
    currentStep = findCurrentStep(frame);
    showFrame(frame);
  });

  showFrame(0);
})();
"

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
  <button id=\"anim-play\">\u25B6</button>
  <input type=\"range\" id=\"anim-scrub\" min=\"0\" value=\"0\">
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
  s!"<script>
(function() \{
  var data = {dataJson};
  var container = document.querySelector('{sel}');
  var currentSeg = null;
  var currentFrame = 0;
  var animId = null;

  function findSegment(frame) \{
    for (var i = 0; i < data.segments.length; i++) \{
      var s = data.segments[i];
      if (frame >= s.sf && frame < s.sf + s.fc) return s;
    }
    return data.segments[data.segments.length - 1];
  }

  function showFrame(frame) \{
    frame = Math.max(0, Math.min(frame, data.totalFrames - 1));
    var seg = findSegment(frame);
    var local = frame - seg.sf;
    if (seg !== currentSeg) \{
      container.innerHTML = seg.sync;
      currentSeg = seg;
    }
    if (local > 0 && seg.svgs && seg.svgs[local]) \{
      container.innerHTML = seg.svgs[local];
    }
    currentFrame = frame;
  }

  function stopAnim() \{
    if (animId !== null) \{ cancelAnimationFrame(animId); animId = null; }
  }

  function findStepEnd(stepFrame) \{
    for (var i = 0; i < data.steps.length; i++) \{
      if (data.steps[i].frame === stepFrame) \{
        for (var j = i + 1; j < data.steps.length; j++) \{
          if (data.steps[j].frame > stepFrame) return data.steps[j].frame;
        }
        return data.totalFrames;
      }
    }
    return data.totalFrames;
  }

  function startLoop(loopStart, loopEnd) \{
    var loopLen = loopEnd - loopStart;
    if (loopLen <= 0) return;
    var startTime = null;
    function tick(timestamp) \{
      if (startTime === null) startTime = timestamp;
      var elapsed = (timestamp - startTime) / 1000;
      var offset = Math.round(elapsed * data.fps) % loopLen;
      showFrame(loopStart + offset);
      animId = requestAnimationFrame(tick);
    }
    animId = requestAnimationFrame(tick);
  }

  function animateTo(targetFrame, onComplete) \{
    stopAnim();
    var startFrame = currentFrame;
    var startTime = null;
    var dir = targetFrame > startFrame ? 1 : -1;
    function tick(timestamp) \{
      if (startTime === null) startTime = timestamp;
      var elapsed = (timestamp - startTime) / 1000;
      var frame = startFrame + dir * Math.round(elapsed * data.fps);
      if ((dir > 0 && frame >= targetFrame) || (dir < 0 && frame <= targetFrame)) \{
        showFrame(targetFrame);
        animId = null;
        if (onComplete) onComplete();
        return;
      }
      showFrame(frame);
      animId = requestAnimationFrame(tick);
    }
    if (startFrame === targetFrame) \{
      if (onComplete) onComplete();
    } else \{
      animId = requestAnimationFrame(tick);
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

  showFrame(0);

  if (typeof Reveal !== 'undefined') \{
    Reveal.addEventListener('fragmentshown', function(e) \{
      stopAnim();
      var idx = parseInt(e.fragment.dataset.fragmentIndex);
      if (!isNaN(idx) && idx < pauseSteps.length) \{
        var ps = pauseSteps[idx];
        animateTo(ps.frame, function() \{
          if (ps.loop) \{
            startLoop(ps.frame, findStepEnd(ps.frame));
          }
        });
      }
    });
    Reveal.addEventListener('fragmenthidden', function(e) \{
      stopAnim();
      var idx = parseInt(e.fragment.dataset.fragmentIndex);
      if (!isNaN(idx)) \{
        var prevIdx = idx - 1;
        if (prevIdx >= 0 && pauseSteps[prevIdx].loop) \{
          var ps = pauseSteps[prevIdx];
          startLoop(ps.frame, findStepEnd(ps.frame));
        } else \{
          var prevFrame = prevIdx >= 0 ? pauseSteps[prevIdx].frame : 0;
          animateTo(prevFrame);
        }
      }
    });
  }
})();
</script>"
