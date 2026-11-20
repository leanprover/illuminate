// @ts-check

// Types: StepInfo, Segment, ParamBinding, AnimData — see anim_data.d.ts
// Shared helpers: see anim_core.js, which the Lean build concatenates before this file.
/* global animFindSegment, animFindCurrentStep, animClampFrame, animComputeFrame, animFindStepEnd,
   animWrapLoop, animCheckPauseSteps, animRenderSegFrame */

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
    /** @type {HTMLButtonElement} */
    const playBtn = /** @type {HTMLButtonElement} */ (document.getElementById("anim-play"));
    /** @type {HTMLInputElement} */
    const scrubber = /** @type {HTMLInputElement} */ (document.getElementById("anim-scrub"));
    if (!container || !playBtn || !scrubber) {
        console.error("Animation: missing required DOM elements");
        return;
    }
    /** @type {Segment | null} */
    let currentSeg = null;
    /** @type {boolean} */
    let playing = false;
    /** @type {number | null} */
    let startTime = null;
    /** @type {number} */
    let pauseFrame = 0;
    /** @type {number} */
    let currentStep = 0;
    /** @type {boolean} */
    let waitingForClick = false;
    /** @type {boolean} */
    let advancePending = false;

    scrubber.max = String(data.totalFrames - 1);

    /**
     * Displays the SVG for the given frame without updating playback state.
     * @param {number} frame
     * @returns {void}
     */
    function showFrame(frame) {
        frame = animClampFrame(frame, data.totalFrames);
        const seg = animFindSegment(data.segments, frame);
        currentSeg = animRenderSegFrame(container, seg, currentSeg, frame - seg.sf);
        scrubber.value = String(frame);
    }

    /**
     * Sets the play button to the paused visual state.
     * @returns {void}
     */
    function showPaused() {
        playBtn.textContent = "\u25B6";
        playBtn.setAttribute("aria-label", "Play");
    }

    /**
     * Sets the play button to the playing visual state.
     * @returns {void}
     */
    function showPlaying() {
        playBtn.textContent = "\u23F8";
        playBtn.setAttribute("aria-label", "Pause");
    }

    /**
     * Animation loop driven by requestAnimationFrame.
     * @param {number} timestamp
     * @returns {void}
     */
    function tick(timestamp) {
        if (!playing || waitingForClick) return;
        if (startTime === null) startTime = timestamp;
        let frame = animComputeFrame(startTime, timestamp, data.fps, pauseFrame);

        // Handle looping steps
        const stepInfo = data.steps[currentStep];
        let isLooping = false;
        if (stepInfo && stepInfo.loop) {
            isLooping = true;
            const stepStart = stepInfo.frame;
            const stepEnd = animFindStepEnd(data.steps, currentStep, data.totalFrames);
            const loop = animWrapLoop(frame, stepStart, stepEnd);
            if (loop.didCycle) {
                if (advancePending) {
                    // Finish the loop: advance to the next step
                    advancePending = false;
                    if (currentStep + 1 < data.steps.length) {
                        const nextStep = data.steps[currentStep + 1];
                        if (nextStep === undefined) {
                            throw new Error(`Step ${currentStep + 1} is missing.`);
                        }
                        currentStep = currentStep + 1;
                        const nextFrame = nextStep.frame;
                        pauseFrame = nextFrame;
                        startTime = null;
                        frame = nextFrame;
                        isLooping = false;
                    }
                } else {
                    frame = loop.wrapped;
                    startTime = timestamp;
                    pauseFrame = stepStart;
                }
            }
        }

        if (frame >= data.totalFrames) {
            frame = data.totalFrames - 1;
            pauseFrame = frame;
            playing = false;
            showPaused();
        }

        // Check for pause steps we've moved past
        if (!isLooping) {
            const pause = animCheckPauseSteps(data.steps, currentStep, frame);
            if (pause) {
                frame = pause.pauseAtFrame;
                pauseFrame = frame;
                waitingForClick = true;
                currentStep = pause.pauseAtStep;
                showFrame(frame);
                showPaused();
                return;
            }
            currentStep = animFindCurrentStep(data.steps, frame);
        }

        showFrame(frame);
        if (playing) requestAnimationFrame(tick);
    }

    /**
     * Advances playback: resumes from pause, signals loop exit, pauses, or starts.
     * @returns {void}
     */
    function advance() {
        if (waitingForClick) {
            waitingForClick = false;
            startTime = null;
            playing = true;
            showPlaying();
            requestAnimationFrame(tick);
        } else if (playing) {
            // Check if we're in a looping step — click signals to finish the cycle
            const stepInfo = data.steps[currentStep];
            if (stepInfo && stepInfo.loop && currentStep + 1 < data.steps.length) {
                advancePending = true;
                return;
            }
            // Normal pause
            playing = false;
            pauseFrame = parseInt(scrubber.value, 10);
            showPaused();
        } else {
            if (pauseFrame >= data.totalFrames - 1) {
                pauseFrame = 0;
                currentStep = 0;
            }
            playing = true;
            startTime = null;
            showPlaying();
            requestAnimationFrame(tick);
        }
    }

    playBtn.addEventListener("click", advance);
    container.addEventListener("click", () => {
        if (waitingForClick) advance();
    });
    document.addEventListener("keydown", (e) => {
        if (e.key === " " || e.key === "Enter") {
            e.preventDefault();
            advance();
        }
    });

    scrubber.addEventListener("input", () => {
        playing = false;
        waitingForClick = false;
        advancePending = false;
        showPaused();
        const frame = parseInt(scrubber.value, 10);
        pauseFrame = frame;
        currentStep = animFindCurrentStep(data.steps, frame);
        showFrame(frame);
    });

    showFrame(0);
})();
