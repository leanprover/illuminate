// @ts-check

// Types: StepInfo, Segment, ParamBinding, AnimData — see globals.d.ts
// Shared helpers: animFindSegment, animFindCurrentStep, etc. — see anim_core.js

(function () {
    /** @type {AnimData} */
    var data = __DATA__;
    if (!data || !data.segments || data.segments.length === 0) {
        console.error("Animation: invalid or empty animation data");
        return;
    }
    /** @type {HTMLElement} */
    var container = /** @type {HTMLElement} */ (document.querySelector("__SELECTOR__"));
    /** @type {HTMLButtonElement} */
    var playBtn = /** @type {HTMLButtonElement} */ (document.getElementById("anim-play"));
    /** @type {HTMLInputElement} */
    var scrubber = /** @type {HTMLInputElement} */ (document.getElementById("anim-scrub"));
    if (!container || !playBtn || !scrubber) {
        console.error("Animation: missing required DOM elements");
        return;
    }
    /** @type {Segment | null} */
    var currentSeg = null;
    /** @type {boolean} */
    var playing = false;
    /** @type {number | null} */
    var startTime = null;
    /** @type {number} */
    var pauseFrame = 0;
    /** @type {number} */
    var currentStep = 0;
    /** @type {boolean} */
    var waitingForClick = false;
    /** @type {boolean} */
    var advancePending = false;

    scrubber.max = String(data.totalFrames - 1);

    /**
     * Displays the SVG for the given frame without updating playback state.
     * @param {number} frame
     * @returns {void}
     */
    function showFrame(frame) {
        frame = animClampFrame(frame, data.totalFrames);
        var seg = animFindSegment(data.segments, frame);
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
        var frame = animComputeFrame(startTime, timestamp, data.fps, pauseFrame);

        // Handle looping steps
        var stepInfo = data.steps[currentStep];
        var isLooping = stepInfo && stepInfo.loop;
        if (isLooping) {
            var stepStart = stepInfo.frame;
            var stepEnd = animFindStepEnd(data.steps, currentStep, data.totalFrames);
            var loop = animWrapLoop(frame, stepStart, stepEnd);
            if (loop.didCycle) {
                if (advancePending) {
                    // Finish the loop: advance to the next step
                    advancePending = false;
                    if (currentStep + 1 < data.steps.length) {
                        currentStep = currentStep + 1;
                        var nextFrame = data.steps[currentStep].frame;
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
            var pause = animCheckPauseSteps(data.steps, currentStep, frame);
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
            var stepInfo = data.steps[currentStep];
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
    container.addEventListener("click", function () {
        if (waitingForClick) advance();
    });
    document.addEventListener("keydown", function (e) {
        if (e.key === " " || e.key === "Enter") {
            e.preventDefault();
            advance();
        }
    });

    scrubber.addEventListener("input", function () {
        playing = false;
        waitingForClick = false;
        advancePending = false;
        showPaused();
        var frame = parseInt(scrubber.value, 10);
        pauseFrame = frame;
        currentStep = animFindCurrentStep(data.steps, frame);
        showFrame(frame);
    });

    showFrame(0);
})();
