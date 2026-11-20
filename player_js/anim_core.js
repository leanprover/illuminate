// @ts-check

// Shared animation helpers used by standalone.js, reveal.js, and animate_widget.js.
// The Lean build puts a copy of this file in front of each player script, so a page with several
// animations loads the file several times. This works because the helpers are function
// declarations: JavaScript lets a script declare a function that an earlier script on the page
// has already declared.

/* exported animFindSegment, animFindCurrentStep, animClampFrame, animComputeFrame,
   animFindStepEnd, animWrapLoop, animCheckPauseSteps, animRenderSegFrame */

/**
 * Finds the segment containing the given frame index.
 * @param {Segment[]} segments
 * @param {number} frame
 * @returns {Segment}
 */
function animFindSegment(segments, frame) {
    for (const s of segments) {
        if (frame >= s.sf && frame < s.sf + s.fc) return s;
    }
    const last = segments[segments.length - 1];
    if (last === undefined) throw new Error("An animation needs at least one segment.");
    return last;
}

/**
 * Returns the index of the step active at the given frame.
 * @param {StepInfo[]} steps
 * @param {number} frame
 * @returns {number}
 */
function animFindCurrentStep(steps, frame) {
    for (let i = steps.length - 1; i >= 0; i--) {
        const step = steps[i];
        if (step === undefined) throw new Error(`Step ${i} is missing.`);
        if (frame >= step.frame) return i;
    }
    return 0;
}

/**
 * Clamps a frame index to `[0, totalFrames - 1]`.
 * @param {number} frame
 * @param {number} totalFrames
 * @returns {number}
 */
function animClampFrame(frame, totalFrames) {
    return Math.max(0, Math.min(frame, totalFrames - 1));
}

/**
 * Computes a frame index from an elapsed-time measurement.
 * @param {number} startTime - timestamp when playback began
 * @param {number} timestamp - current requestAnimationFrame timestamp
 * @param {number} fps - frames per second
 * @param {number} pauseFrame - frame offset at which playback started
 * @returns {number}
 */
function animComputeFrame(startTime, timestamp, fps, pauseFrame) {
    const elapsed = (timestamp - startTime) / 1000;
    return pauseFrame + Math.round(elapsed * fps);
}

/**
 * Returns the frame index where the step at `stepIndex` ends.
 * @param {StepInfo[]} steps
 * @param {number} stepIndex
 * @param {number} totalFrames
 * @returns {number}
 */
function animFindStepEnd(steps, stepIndex, totalFrames) {
    const next = steps[stepIndex + 1];
    return next !== undefined ? next.frame : totalFrames;
}

/**
 * @typedef {{ wrapped: number, didCycle: boolean }} LoopResult
 */

/**
 * Wraps a frame within a looping step's range. Returns the wrapped frame
 * and whether a full cycle boundary was crossed.
 * @param {number} frame
 * @param {number} stepStart
 * @param {number} stepEnd
 * @returns {LoopResult}
 */
function animWrapLoop(frame, stepStart, stepEnd) {
    const stepLen = stepEnd - stepStart;
    if (stepLen <= 0 || frame < stepEnd) return { wrapped: frame, didCycle: false };
    const overshoot = (frame - stepStart) % stepLen;
    return { wrapped: stepStart + overshoot, didCycle: true };
}

/**
 * Scans forward from `currentStep` to check if any pause steps have been
 * crossed at the given frame. Returns the first pause found, or null.
 * @param {StepInfo[]} steps
 * @param {number} currentStep
 * @param {number} frame
 * @returns {{ pauseAtStep: number, pauseAtFrame: number } | null}
 */
function animCheckPauseSteps(steps, currentStep, frame) {
    const step = animFindCurrentStep(steps, frame);
    if (step > currentStep) {
        for (let s = currentStep + 1; s <= step; s++) {
            const stepInfo = steps[s];
            if (stepInfo === undefined) throw new Error(`Step ${s} is missing.`);
            if (stepInfo.pause) {
                return { pauseAtStep: s, pauseAtFrame: stepInfo.frame };
            }
        }
    }
    return null;
}

/**
 * Indexes patchable SVG elements by their `data-e` attribute.
 *
 * The Lean SVG renderer tags each element that produces a DOM node with
 * `data-e="N"` where N is the element index used by paramMap. This
 * function collects those elements into a sparse array keyed by N,
 * decoupling the JS side from any particular DOM walk order.
 * @param {HTMLElement} container
 * @returns {Element[]}
 */
function animIndexElements(container) {
    /** @type {Element[]} */
    const elems = [];
    const tagged = container.querySelectorAll("[data-e]");
    for (const el of tagged) {
        const idx = parseInt(el.getAttribute("data-e") || "0", 10);
        elems[idx] = el;
    }
    return elems;
}

/**
 * Renders a frame into a container using parameterized attribute updates
 * when available, falling back to full SVG replacement.
 * Returns the segment used (for callers that track the current segment).
 * @param {HTMLElement} container
 * @param {Segment} seg
 * @param {Segment | null} currentSeg - the previously rendered segment (for change detection)
 * @param {number} local - frame index within the segment
 * @returns {Segment} the segment (with `_elems` populated)
 */
function animRenderSegFrame(container, seg, currentSeg, local) {
    if (seg !== currentSeg) {
        // eslint-disable-next-line no-unsanitized/property -- `seg.sync` is SVG from the Lean renderer
        container.innerHTML = seg.sync;
        seg._elems = animIndexElements(container);
    }
    if (seg._elems && seg.pmap && seg.params && seg.params[local]) {
        const p = seg.params[local];
        for (const [i, binding] of seg.pmap.entries()) {
            const elem = seg._elems[binding.e];
            const value = p[i];
            if (elem && value !== undefined) {
                if (binding.a === "textContent") {
                    elem.textContent = value;
                } else {
                    elem.setAttribute(binding.a, value);
                }
            }
        }
    }
    return seg;
}
