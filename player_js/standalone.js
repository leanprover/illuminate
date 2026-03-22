// @ts-check

// Types: StepInfo, Segment, ParamBinding, AnimData — see globals.d.ts

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

    scrubber.max = String(data.totalFrames - 1);

    /**
     * Finds the segment containing the given frame index.
     * @param {number} frame
     * @returns {Segment}
     */
    function findSegment(frame) {
        for (var i = 0; i < data.segments.length; i++) {
            var s = data.segments[i];
            if (frame >= s.sf && frame < s.sf + s.fc) return s;
        }
        return data.segments[data.segments.length - 1];
    }

    /**
     * Displays the SVG for the given frame without updating playback state.
     * @param {number} frame
     * @returns {void}
     */
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

        scrubber.value = String(frame);
    }

    /**
     * Returns the index of the step active at the given frame.
     * @param {number} frame
     * @returns {number}
     */
    function findCurrentStep(frame) {
        for (var i = data.steps.length - 1; i >= 0; i--) {
            if (frame >= data.steps[i].frame) return i;
        }
        return 0;
    }

    /**
     * Animation loop driven by requestAnimationFrame.
     * @param {number} timestamp
     * @returns {void}
     */
    function tick(timestamp) {
        if (!playing || waitingForClick) return;
        if (startTime === null) startTime = timestamp;
        var elapsed = (timestamp - startTime) / 1000;
        var frame = pauseFrame + Math.round(elapsed * data.fps);

        // Handle looping steps: wrap frame within step boundaries
        var stepInfo = data.steps[currentStep];
        if (stepInfo && stepInfo.loop) {
            var stepStart = stepInfo.frame;
            var stepEnd =
                currentStep + 1 < data.steps.length
                    ? data.steps[currentStep + 1].frame
                    : data.totalFrames;
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
            playBtn.textContent = "\u25B6";
            playBtn.setAttribute("aria-label", "Play");
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
                    playBtn.textContent = "\u25B6";
                    playBtn.setAttribute("aria-label", "Play");
                    return;
                }
            }
            currentStep = step;
        }

        showFrame(frame);
        if (playing) requestAnimationFrame(tick);
    }

    /**
     * Advances playback: resumes from pause, pauses if playing, or starts from beginning.
     * @returns {void}
     */
    function advance() {
        if (waitingForClick) {
            waitingForClick = false;
            startTime = null;
            playing = true;
            playBtn.textContent = "\u23F8";
            playBtn.setAttribute("aria-label", "Pause");
            requestAnimationFrame(tick);
        } else if (playing) {
            playing = false;
            pauseFrame = parseInt(scrubber.value, 10);
            playBtn.textContent = "\u25B6";
            playBtn.setAttribute("aria-label", "Play");
        } else {
            if (pauseFrame >= data.totalFrames - 1) {
                pauseFrame = 0;
                currentStep = 0;
            }
            playing = true;
            startTime = null;
            playBtn.textContent = "\u23F8";
            playBtn.setAttribute("aria-label", "Pause");
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
        playBtn.textContent = "\u25B6";
        playBtn.setAttribute("aria-label", "Play");
        var frame = parseInt(scrubber.value, 10);
        pauseFrame = frame;
        currentStep = findCurrentStep(frame);
        showFrame(frame);
    });

    showFrame(0);
})();
