/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/

import Lean
import Illuminate.Animation.Animate
import Illuminate.Animation.Compile
import Illuminate.Animation.Render
import Illuminate.Widget
import Illuminate.Backend.SVG


namespace Illuminate

/--
Wraps an animation render function for preview in the Lean infoview via `#diagram`.

The returned function takes a time slider value and produces the diagram at that time,
allowing interactive scrubbing through the animation.
-/
def previewAnimation (steps : List Step)
    (render : Vector Float steps.length → Diagram SVG)
    : Slider "time" 0 (totalDuration steps) 0 → Diagram SVG :=
  fun time =>
    let progress := progressAt steps time
    let vec := progressVector progress steps.length
    render vec

-- ═══════════════════════════════════════════════════════════════
-- Animation widget
-- ═══════════════════════════════════════════════════════════════

open Lean Widget in
/-- Widget module that plays compiled animations with requestAnimationFrame-driven SVG playback. -/
@[widget_module]
def animateWidget : Lean.Widget.Module where
  javascript := "
import * as React from 'react';
const e = React.createElement;

export default function(props) {
  var data = props.animData;
  var containerRef = React.useRef(null);
  var playingRef = React.useRef(false);
  var startTimeRef = React.useRef(null);
  var pauseFrameRef = React.useRef(0);
  var currentStepRef = React.useRef(0);
  var waitingRef = React.useRef(false);
  var advancePendingRef = React.useRef(false);
  var currentSegRef = React.useRef(null);
  var rafRef = React.useRef(null);
  var _playing = React.useState(false);
  var playing = _playing[0];
  var setPlaying = _playing[1];
  var _looping = React.useState(false);
  var looping = _looping[0];
  var setLooping = _looping[1];
  var _frame = React.useState(0);
  var frame = _frame[0];
  var setFrame = _frame[1];

  function findSegment(f) {
    for (var i = 0; i < data.segments.length; i++) {
      var s = data.segments[i];
      if (f >= s.sf && f < s.sf + s.fc) return s;
    }
    return data.segments[data.segments.length - 1];
  }

  // Index SVG elements depth-first, starting from the content group
  // inside <svg><g transform=\"scale(1,-1)\">...</g></svg>.
  // This matches the DrawCmd element ordering used by paramMap.
  function indexElements(container) {
    var elems = [];
    var svg = container.querySelector('svg');
    if (!svg) return elems;
    // The first <g> child is the scale(1,-1) wrapper added by Svg.render
    var contentGroup = svg.querySelector('g');
    if (!contentGroup) return elems;
    function walk(node) {
      for (var i = 0; i < node.childNodes.length; i++) {
        var child = node.childNodes[i];
        if (child.nodeType === 1) {
          elems.push(child);
          walk(child);
        }
      }
    }
    walk(contentGroup);
    return elems;
  }

  function renderFrame(f) {
    f = Math.max(0, Math.min(f, data.totalFrames - 1));
    var seg = findSegment(f);
    var container = containerRef.current;
    if (!container) return;
    var local = f - seg.sf;

    // If switching segments, rebuild from sync frame and index elements
    if (seg !== currentSegRef.current) {
      container.innerHTML = seg.sync;
      currentSegRef.current = seg;
      seg._elems = indexElements(container);
    }

    // Apply params via setAttribute if we have a paramMap and indexed elements
    if (seg._elems && seg.pmap && seg.params && seg.params[local]) {
      var p = seg.params[local];
      for (var i = 0; i < seg.pmap.length; i++) {
        var binding = seg.pmap[i];
        var elem = seg._elems[binding.e];
        if (elem && p[i] !== undefined) {
          if (binding.a === 'textContent') {
            elem.textContent = p[i];
          } else {
            elem.setAttribute(binding.a, p[i]);
          }
        }
      }
    } else if (local === 0) {
      // Frame 0 is the sync frame, already displayed
    } else if (seg.svgs && seg.svgs[local]) {
      // Fallback: full SVG replacement
      container.innerHTML = seg.svgs[local];
      currentSegRef.current = null; // force re-index on next segment entry
    }

    setFrame(f);
  }

  function findCurrentStep(f) {
    for (var i = data.steps.length - 1; i >= 0; i--) {
      if (f >= data.steps[i].frame) return i;
    }
    return 0;
  }

  function tick(timestamp) {
    if (!playingRef.current || waitingRef.current) return;
    if (startTimeRef.current === null) startTimeRef.current = timestamp;
    var elapsed = (timestamp - startTimeRef.current) / 1000;
    // pauseFrameRef holds the frame at which playback started
    var f = pauseFrameRef.current + Math.round(elapsed * data.fps);

    // Find the current step and the frame range it occupies
    var curStep = currentStepRef.current;
    var stepStart = data.steps[curStep] ? data.steps[curStep].frame : 0;
    var stepEnd = (curStep + 1 < data.steps.length)
      ? data.steps[curStep + 1].frame : data.totalFrames;
    var stepInfo = data.steps[curStep];
    var stepLen = stepEnd - stepStart;

    // If current step loops, wrap within its frame range
    var isLooping = stepInfo && stepInfo.loop && stepLen > 0;
    if (isLooping && f >= stepEnd) {
      if (advancePendingRef.current) {
        // Finish the loop: advance to the next step
        advancePendingRef.current = false;
        if (curStep + 1 < data.steps.length) {
          currentStepRef.current = curStep + 1;
          var nextFrame = data.steps[curStep + 1].frame;
          pauseFrameRef.current = nextFrame;
          startTimeRef.current = null;
          f = nextFrame;
          isLooping = false;
        }
      } else {
        var overshoot = (f - stepStart) % stepLen;
        f = stepStart + overshoot;
        startTimeRef.current = timestamp;
        pauseFrameRef.current = stepStart;
      }
    }
    setLooping(!!isLooping);

    if (f >= data.totalFrames) {
      f = data.totalFrames - 1;
      playingRef.current = false;
      setPlaying(false);
      renderFrame(f);
      pauseFrameRef.current = f;
      return;
    }

    // Check for pause steps we've moved past
    var step = findCurrentStep(f);
    if (step > currentStepRef.current) {
      for (var s = currentStepRef.current + 1; s <= step; s++) {
        if (data.steps[s].pause) {
          f = data.steps[s].frame;
          waitingRef.current = true;
          currentStepRef.current = s;
          renderFrame(f);
          pauseFrameRef.current = f;
          setPlaying(false);
          return;
        }
      }
      currentStepRef.current = step;
    }

    renderFrame(f);
    rafRef.current = requestAnimationFrame(tick);
  }

  function advance() {
    if (waitingRef.current) {
      // Non-looping pause step: resume playback
      waitingRef.current = false;
      startTimeRef.current = null;
      playingRef.current = true;
      setPlaying(true);
      rafRef.current = requestAnimationFrame(tick);
    } else if (playingRef.current) {
      // Check if we're in a looping step — click signals to finish the cycle then advance
      var cur = currentStepRef.current;
      var stepInfo = data.steps[cur];
      if (stepInfo && stepInfo.loop && cur + 1 < data.steps.length) {
        advancePendingRef.current = true;
        return;
      }
      // Normal pause
      playingRef.current = false;
      setPlaying(false);
      if (rafRef.current) cancelAnimationFrame(rafRef.current);
      pauseFrameRef.current = frame;
    } else {
      // Start/restart
      if (pauseFrameRef.current >= data.totalFrames - 1) {
        pauseFrameRef.current = 0;
        currentStepRef.current = 0;
        renderFrame(0);
      }
      startTimeRef.current = null;
      playingRef.current = true;
      setPlaying(true);
      rafRef.current = requestAnimationFrame(tick);
    }
  }

  // Initialize or reset when animation data changes
  React.useEffect(function() {
    playingRef.current = false;
    setPlaying(false);
    setLooping(false);
    waitingRef.current = false;
    advancePendingRef.current = false;
    startTimeRef.current = null;
    pauseFrameRef.current = 0;
    currentStepRef.current = 0;
    currentSegRef.current = null;
    if (rafRef.current) cancelAnimationFrame(rafRef.current);
    renderFrame(0);
    return function() {
      if (rafRef.current) cancelAnimationFrame(rafRef.current);
    };
  }, [data]);

  var totalSeconds = data.totalFrames / data.fps;
  var currentSeconds = frame / data.fps;

  return e('div', { style: { padding: '4px', background: 'white' } },
    e('div', {
      ref: containerRef,
      style: { width: '100%', cursor: 'pointer' },
      onClick: function() { if (waitingRef.current) advance(); }
    }),
    e('div', { style: { display: 'flex', alignItems: 'center', gap: '6px', marginTop: '6px' } },
      e('button', {
        onClick: advance,
        style: {
          fontSize: '14px', width: '28px', height: '28px',
          border: '1px solid #ccc', borderRadius: '4px',
          background: 'white', cursor: 'pointer',
          display: 'flex', alignItems: 'center', justifyContent: 'center'
        }
      }, playing ? (looping ? '\\u23ED' : '\\u23F8') : '\\u25B6'),
      e('input', {
        type: 'range',
        min: 0,
        max: data.totalFrames - 1,
        value: frame,
        style: { flex: 1 },
        onInput: function(ev) {
          playingRef.current = false;
          setPlaying(false);
          waitingRef.current = false;
          if (rafRef.current) cancelAnimationFrame(rafRef.current);
          var f = parseInt(ev.target.value);
          currentStepRef.current = findCurrentStep(f);
          pauseFrameRef.current = f;
          renderFrame(f);
        }
      }),
      e('span', { style: { fontSize: '11px', color: '#666', width: '5.5em', textAlign: 'right', flexShrink: 0 } },
        currentSeconds.toFixed(1) + ' / ' + totalSeconds.toFixed(1) + 's')
    )
  );
}
"

-- ═══════════════════════════════════════════════════════════════
-- #animate command
-- ═══════════════════════════════════════════════════════════════

/-- Syntax for the `#animate` command that plays an animation in the infoview. -/
syntax (name := animateCmd) "#animate " ("(" &"fps" " := " num ") ")? term:max term : command

open Lean Widget Elab Command Term Meta in
/-- Elaborates the `#animate` command, compiling the animation and rendering it in the infoview. -/
@[command_elab animateCmd]
unsafe def elabAnimateCmd : CommandElab := fun stx => do
  let fpsOpt := stx[1]
  let stepsStx : TSyntax `term := ⟨stx[2]⟩
  let renderStx : TSyntax `term := ⟨stx[3]⟩
  liftTermElabM do
    let compiledAnimTy := Lean.mkConst ``CompiledAnimation
    let fps : Nat := if fpsOpt.isNone then 60
      else fpsOpt[0][2].isNatLit?.getD 60
    let fpsLit := Syntax.mkNumLit (toString fps)
    let callStx ← `(compileAnimation $stepsStx $renderStx (fps := $fpsLit))
    let e ← Term.elabTerm callStx (some compiledAnimTy)
    Term.synthesizeSyntheticMVarsNoPostponing
    let e ← instantiateMVars e
    let ca ← evalExpr CompiledAnimation compiledAnimTy e (safety := .unsafe)
    let animJson := compiledAnimationToLeanJson ca
    let props : Json := .mkObj [
      ("animData", animJson)]
    savePanelWidgetInfo animateWidget.javascriptHash.val (pure props) stx
