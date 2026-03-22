// @ts-check

// Types: StepInfo, Segment, ParamBinding, AnimData — see globals.d.ts
// Shared helpers: animFindSegment, animClampFrame, etc. — see anim_core.js

(function () {
    /** @type {AnimData} */
    var data = __DATA__;
    if (!data || !data.segments || data.segments.length === 0) {
        console.error("Animation: invalid or empty animation data");
        return;
    }
    /** @type {HTMLElement} */
    var container = /** @type {HTMLElement} */ (document.querySelector("__SELECTOR__"));
    if (!container) {
        console.error("Animation: container element not found");
        return;
    }
    /** @type {Segment | null} */
    var currentSeg = null;
    /** @type {number} */
    var currentFrame = 0;
    /** @type {number | null} */
    var animId = null;

    /**
     * Displays the SVG for the given frame.
     * @param {number} frame
     * @returns {void}
     */
    function showFrame(frame) {
        frame = animClampFrame(frame, data.totalFrames);
        var seg = animFindSegment(data.segments, frame);
        var local = frame - seg.sf;
        if (seg !== currentSeg) {
            container.innerHTML = seg.sync;
            currentSeg = seg;
        }
        if (local > 0 && seg.svgs && seg.svgs[local]) {
            container.innerHTML = seg.svgs[local];
        }
        currentFrame = frame;
    }

    /**
     * Cancels any running animation or loop.
     * @returns {void}
     */
    function stopAnim() {
        if (animId !== null) {
            cancelAnimationFrame(animId);
            animId = null;
        }
    }

    /**
     * Loops playback between `loopStart` and `loopEnd` indefinitely.
     * @param {number} loopStart
     * @param {number} loopEnd
     * @returns {void}
     */
    function startLoop(loopStart, loopEnd) {
        var loopLen = loopEnd - loopStart;
        if (loopLen <= 0) return;
        /** @type {number | null} */
        var startTime = null;
        /** @param {number} timestamp */
        function tick(timestamp) {
            if (startTime === null) startTime = timestamp;
            var frame = animComputeFrame(startTime, timestamp, data.fps, loopStart);
            var loop = animWrapLoop(frame, loopStart, loopEnd);
            showFrame(loop.wrapped);
            animId = requestAnimationFrame(tick);
        }
        animId = requestAnimationFrame(tick);
    }

    /**
     * Animates from the current frame to `targetFrame` at the animation's fps.
     * @param {number} targetFrame
     * @param {(() => void)} [onComplete]
     * @returns {void}
     */
    function animateTo(targetFrame, onComplete) {
        stopAnim();
        var startFrame = currentFrame;
        /** @type {number | null} */
        var startTime = null;
        var dir = targetFrame > startFrame ? 1 : -1;
        /** @param {number} timestamp */
        function tick(timestamp) {
            if (startTime === null) startTime = timestamp;
            var frame = animComputeFrame(startTime, timestamp, data.fps, startFrame);
            if (dir < 0) {
                frame = startFrame - (frame - startFrame);
            }
            if ((dir > 0 && frame >= targetFrame) || (dir < 0 && frame <= targetFrame)) {
                showFrame(targetFrame);
                animId = null;
                if (onComplete) onComplete();
                return;
            }
            showFrame(frame);
            animId = requestAnimationFrame(tick);
        }
        if (startFrame === targetFrame) {
            if (onComplete) onComplete();
        } else {
            animId = requestAnimationFrame(tick);
        }
    }

    var pauseSteps = data.steps.filter(function (s) {
        return s.pause;
    });
    pauseSteps.forEach(function (s, i) {
        var frag = document.createElement("span");
        frag.className = "fragment";
        frag.dataset.fragmentIndex = String(i);
        frag.style.display = "none";
        /** @type {HTMLElement | null} */
        var parent = container.parentElement;
        if (parent) parent.appendChild(frag);
    });

    showFrame(0);

    if (typeof Reveal !== "undefined") {
        Reveal.addEventListener("fragmentshown", function (e) {
            stopAnim();
            var idx = parseInt(e.fragment.dataset.fragmentIndex || "", 10);
            if (!isNaN(idx) && idx < pauseSteps.length) {
                var ps = pauseSteps[idx];
                var stepIdx = animFindCurrentStep(data.steps, ps.frame);
                animateTo(ps.frame, function () {
                    if (ps.loop) {
                        startLoop(ps.frame, animFindStepEnd(data.steps, stepIdx, data.totalFrames));
                    }
                });
            }
        });
        Reveal.addEventListener("fragmenthidden", function (e) {
            stopAnim();
            var idx = parseInt(e.fragment.dataset.fragmentIndex || "", 10);
            if (!isNaN(idx)) {
                var prevIdx = idx - 1;
                if (prevIdx >= 0 && pauseSteps[prevIdx].loop) {
                    var ps = pauseSteps[prevIdx];
                    var stepIdx = animFindCurrentStep(data.steps, ps.frame);
                    startLoop(ps.frame, animFindStepEnd(data.steps, stepIdx, data.totalFrames));
                } else {
                    var prevFrame = prevIdx >= 0 ? pauseSteps[prevIdx].frame : 0;
                    animateTo(prevFrame);
                }
            }
        });
    }
})();
