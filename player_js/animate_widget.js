// @ts-check
import * as React from "react";
const e = React.createElement;

// Types: StepInfo, Segment, ParamBinding, AnimData — see globals.d.ts
// Shared helpers: animFindSegment, animFindCurrentStep, etc. — see anim_core.js

/**
 * @typedef {{ animData: AnimData }} AnimateProps
 */

/**
 * Animation player widget component for the Lean infoview.
 * @param {AnimateProps} props
 * @returns {React.ReactElement}
 */
export default function (props) {
    var data = props.animData;
    /** @type {React.MutableRefObject<HTMLDivElement | null>} */
    var containerRef = React.useRef(null);
    var playingRef = React.useRef(false);
    /** @type {React.MutableRefObject<number | null>} */
    var startTimeRef = React.useRef(null);
    var pauseFrameRef = React.useRef(0);
    var currentStepRef = React.useRef(0);
    var waitingRef = React.useRef(false);
    var advancePendingRef = React.useRef(false);
    /** @type {React.MutableRefObject<Segment | null>} */
    var currentSegRef = React.useRef(null);
    /** @type {React.MutableRefObject<number | null>} */
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

    /**
     * Indexes SVG elements depth-first, starting from the content group
     * inside `<svg><g transform="scale(1,-1)">...</g></svg>`.
     * This matches the DrawCmd element ordering used by paramMap.
     * @param {HTMLElement} container
     * @returns {Element[]}
     */
    function indexElements(container) {
        /** @type {Element[]} */
        var elems = [];
        var svg = container.querySelector("svg");
        if (!svg) return elems;
        // The first <g> child is the scale(1,-1) wrapper added by Svg.render
        var contentGroup = svg.querySelector("g");
        if (!contentGroup) return elems;
        /** @param {Element} node */
        function walk(node) {
            for (var i = 0; i < node.childNodes.length; i++) {
                var child = node.childNodes[i];
                if (child.nodeType === 1) {
                    elems.push(/** @type {Element} */ (child));
                    walk(/** @type {Element} */ (child));
                }
            }
        }
        walk(contentGroup);
        return elems;
    }

    /**
     * Renders the given frame into the container, using parameterized updates when possible.
     * @param {number} f
     * @returns {void}
     */
    function renderFrame(f) {
        f = animClampFrame(f, data.totalFrames);
        var seg = animFindSegment(data.segments, f);
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
                    if (binding.a === "textContent") {
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

    /**
     * Animation loop driven by requestAnimationFrame.
     * @param {number} timestamp
     * @returns {void}
     */
    function tick(timestamp) {
        if (!playingRef.current || waitingRef.current) return;
        if (startTimeRef.current === null) startTimeRef.current = timestamp;
        var f = animComputeFrame(startTimeRef.current, timestamp, data.fps, pauseFrameRef.current);

        // Handle looping steps
        var curStep = currentStepRef.current;
        var stepInfo = data.steps[curStep];
        var isLooping = stepInfo && stepInfo.loop;
        if (isLooping) {
            var stepStart = stepInfo.frame;
            var stepEnd = animFindStepEnd(data.steps, curStep, data.totalFrames);
            var loop = animWrapLoop(f, stepStart, stepEnd);
            if (loop.didCycle) {
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
                    f = loop.wrapped;
                    startTimeRef.current = timestamp;
                    pauseFrameRef.current = stepStart;
                }
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
        if (!isLooping) {
            var pause = animCheckPauseSteps(data.steps, currentStepRef.current, f);
            if (pause) {
                f = pause.pauseAtFrame;
                waitingRef.current = true;
                currentStepRef.current = pause.pauseAtStep;
                renderFrame(f);
                pauseFrameRef.current = f;
                setPlaying(false);
                return;
            }
            currentStepRef.current = animFindCurrentStep(data.steps, f);
        }

        renderFrame(f);
        rafRef.current = requestAnimationFrame(tick);
    }

    /**
     * Advances playback: resumes from pause, signals loop exit, pauses, or starts.
     * @returns {void}
     */
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
    React.useEffect(
        function () {
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
            return function () {
                if (rafRef.current) cancelAnimationFrame(rafRef.current);
            };
        },
        [data],
    );

    var totalSeconds = data.totalFrames / data.fps;
    var currentSeconds = frame / data.fps;

    return e(
        "div",
        { style: { padding: "4px", background: "white" } },
        e("div", {
            ref: containerRef,
            style: { width: "100%", cursor: "pointer" },
            onClick: function () {
                if (waitingRef.current) advance();
            },
        }),
        e(
            "div",
            { style: { display: "flex", alignItems: "center", gap: "6px", marginTop: "6px" } },
            e(
                "button",
                {
                    onClick: advance,
                    style: {
                        fontSize: "14px",
                        width: "28px",
                        height: "28px",
                        border: "1px solid #ccc",
                        borderRadius: "4px",
                        background: "white",
                        cursor: "pointer",
                        display: "flex",
                        alignItems: "center",
                        justifyContent: "center",
                    },
                    "aria-label": playing ? (looping ? "Next" : "Pause") : "Play",
                },
                playing ? (looping ? "\u23ED" : "\u23F8") : "\u25B6",
            ),
            e("input", {
                type: "range",
                min: 0,
                max: data.totalFrames - 1,
                value: frame,
                "aria-label": "Animation progress",
                style: { flex: 1 },
                onInput: /** @param {React.FormEvent<HTMLInputElement>} ev */ function (ev) {
                    playingRef.current = false;
                    setPlaying(false);
                    waitingRef.current = false;
                    advancePendingRef.current = false;
                    if (rafRef.current) cancelAnimationFrame(rafRef.current);
                    var f = parseInt(/** @type {HTMLInputElement} */ (ev.target).value, 10);
                    currentStepRef.current = animFindCurrentStep(data.steps, f);
                    pauseFrameRef.current = f;
                    renderFrame(f);
                },
            }),
            e(
                "span",
                {
                    style: {
                        fontSize: "11px",
                        color: "#666",
                        width: "5.5em",
                        textAlign: "right",
                        flexShrink: 0,
                    },
                },
                currentSeconds.toFixed(1) + " / " + totalSeconds.toFixed(1) + "s",
            ),
        ),
    );
}
