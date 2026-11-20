// @ts-check

// Types: StepInfo, Segment, ParamBinding, AnimData — see anim_data.d.ts
// Shared helpers: see anim_core.js, which the Lean build concatenates before this file.
/* global Reveal, animFindSegment, animFindCurrentStep, animClampFrame, animComputeFrame,
   animFindStepEnd, animWrapLoop, animRenderSegFrame */

(function () {
    /** @type {AnimData} */
    const data = __ILLUMINATE_DATA_98712__;
    if (!data || !data.segments || data.segments.length === 0) {
        console.error("Animation: invalid or empty animation data");
        return;
    }
    /** @type {HTMLElement} */
    const container = /** @type {HTMLElement} */ (
        document.querySelector("__ILLUMINATE_SELECTOR_98712__")
    );
    if (!container) {
        console.error("Animation: container element not found");
        return;
    }
    /** @type {Segment | null} */
    let currentSeg = null;
    /** @type {number} */
    let currentFrame = 0;
    /** @type {number | null} */
    let animId = null;

    /**
     * Displays the SVG for the given frame.
     * @param {number} frame
     * @returns {void}
     */
    function showFrame(frame) {
        frame = animClampFrame(frame, data.totalFrames);
        const seg = animFindSegment(data.segments, frame);
        currentSeg = animRenderSegFrame(container, seg, currentSeg, frame - seg.sf);
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
        const loopLen = loopEnd - loopStart;
        if (loopLen <= 0) return;
        /** @type {number | null} */
        let startTime = null;
        /** @param {number} timestamp */
        function tick(timestamp) {
            if (startTime === null) startTime = timestamp;
            const frame = animComputeFrame(startTime, timestamp, data.fps, loopStart);
            const loop = animWrapLoop(frame, loopStart, loopEnd);
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
        const startFrame = currentFrame;
        /** @type {number | null} */
        let startTime = null;
        const dir = targetFrame > startFrame ? 1 : -1;
        /** @param {number} timestamp */
        function tick(timestamp) {
            if (startTime === null) startTime = timestamp;
            let frame = animComputeFrame(startTime, timestamp, data.fps, startFrame);
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

    const pauseSteps = data.steps.filter((s) => {
        return s.pause;
    });
    for (const i of pauseSteps.keys()) {
        const frag = document.createElement("span");
        frag.className = "fragment";
        frag.dataset["fragmentIndex"] = String(i);
        frag.style.display = "none";
        /** @type {HTMLElement | null} */
        const parent = container.parentElement;
        if (parent) parent.appendChild(frag);
    }

    showFrame(0);

    if (typeof Reveal !== "undefined") {
        const listen = typeof Reveal.on === "function" ? Reveal.on : Reveal.addEventListener;
        if (listen === undefined) {
            throw new Error("Reveal provides neither `on` nor `addEventListener`.");
        }
        /** @type {(type: string, fn: (e: { fragment: HTMLElement }) => void) => void} */
        const revealOn = listen.bind(Reveal);
        revealOn("fragmentshown", (e) => {
            stopAnim();
            const idx = parseInt(e.fragment.dataset["fragmentIndex"] || "", 10);
            if (!isNaN(idx) && idx < pauseSteps.length) {
                const ps = pauseSteps[idx];
                if (ps === undefined) throw new Error(`Pause step ${idx} is missing.`);
                const stepIdx = animFindCurrentStep(data.steps, ps.frame);
                animateTo(ps.frame, () => {
                    if (ps.loop) {
                        startLoop(ps.frame, animFindStepEnd(data.steps, stepIdx, data.totalFrames));
                    }
                });
            }
        });
        revealOn("fragmenthidden", (e) => {
            stopAnim();
            const idx = parseInt(e.fragment.dataset["fragmentIndex"] || "", 10);
            if (!isNaN(idx)) {
                const prevIdx = idx - 1;
                if (prevIdx >= 0) {
                    const ps = pauseSteps[prevIdx];
                    if (ps === undefined) throw new Error(`Pause step ${prevIdx} is missing.`);
                    if (ps.loop) {
                        const stepIdx = animFindCurrentStep(data.steps, ps.frame);
                        startLoop(ps.frame, animFindStepEnd(data.steps, stepIdx, data.totalFrames));
                    } else {
                        animateTo(ps.frame);
                    }
                } else {
                    animateTo(0);
                }
            }
        });
    }
})();
