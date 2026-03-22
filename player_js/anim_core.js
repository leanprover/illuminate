// @ts-check

// Shared animation helpers used by standalone.js, reveal.js, and animate_widget.js.
// Defined as var declarations so they work when concatenated before any player.

/**
 * Finds the segment containing the given frame index.
 * @param {Segment[]} segments
 * @param {number} frame
 * @returns {Segment}
 */
var animFindSegment = function (segments, frame) {
    for (var i = 0; i < segments.length; i++) {
        var s = segments[i];
        if (frame >= s.sf && frame < s.sf + s.fc) return s;
    }
    return segments[segments.length - 1];
};

/**
 * Returns the index of the step active at the given frame.
 * @param {StepInfo[]} steps
 * @param {number} frame
 * @returns {number}
 */
var animFindCurrentStep = function (steps, frame) {
    for (var i = steps.length - 1; i >= 0; i--) {
        if (frame >= steps[i].frame) return i;
    }
    return 0;
};

/**
 * Clamps a frame index to `[0, totalFrames - 1]`.
 * @param {number} frame
 * @param {number} totalFrames
 * @returns {number}
 */
var animClampFrame = function (frame, totalFrames) {
    return Math.max(0, Math.min(frame, totalFrames - 1));
};

/**
 * Computes a frame index from an elapsed-time measurement.
 * @param {number} startTime - timestamp when playback began
 * @param {number} timestamp - current requestAnimationFrame timestamp
 * @param {number} fps - frames per second
 * @param {number} pauseFrame - frame offset at which playback started
 * @returns {number}
 */
var animComputeFrame = function (startTime, timestamp, fps, pauseFrame) {
    var elapsed = (timestamp - startTime) / 1000;
    return pauseFrame + Math.round(elapsed * fps);
};

/**
 * Returns the frame index where the step at `stepIndex` ends.
 * @param {StepInfo[]} steps
 * @param {number} stepIndex
 * @param {number} totalFrames
 * @returns {number}
 */
var animFindStepEnd = function (steps, stepIndex, totalFrames) {
    return stepIndex + 1 < steps.length ? steps[stepIndex + 1].frame : totalFrames;
};

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
var animWrapLoop = function (frame, stepStart, stepEnd) {
    var stepLen = stepEnd - stepStart;
    if (stepLen <= 0 || frame < stepEnd) return { wrapped: frame, didCycle: false };
    var overshoot = (frame - stepStart) % stepLen;
    return { wrapped: stepStart + overshoot, didCycle: true };
};

/**
 * Scans forward from `currentStep` to check if any pause steps have been
 * crossed at the given frame. Returns the first pause found, or null.
 * @param {StepInfo[]} steps
 * @param {number} currentStep
 * @param {number} frame
 * @returns {{ pauseAtStep: number, pauseAtFrame: number } | null}
 */
var animCheckPauseSteps = function (steps, currentStep, frame) {
    var step = animFindCurrentStep(steps, frame);
    if (step > currentStep) {
        for (var s = currentStep + 1; s <= step; s++) {
            if (steps[s].pause) {
                return { pauseAtStep: s, pauseAtFrame: steps[s].frame };
            }
        }
    }
    return null;
};
