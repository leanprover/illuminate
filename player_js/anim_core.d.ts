// Type declarations for shared animation helpers defined in anim_core.js.

interface LoopResult {
    wrapped: number;
    didCycle: boolean;
}

declare var animFindSegment: (segments: Segment[], frame: number) => Segment;
declare var animFindCurrentStep: (steps: StepInfo[], frame: number) => number;
declare var animClampFrame: (frame: number, totalFrames: number) => number;
declare var animComputeFrame: (
    startTime: number,
    timestamp: number,
    fps: number,
    pauseFrame: number,
) => number;
declare var animFindStepEnd: (steps: StepInfo[], stepIndex: number, totalFrames: number) => number;
declare var animWrapLoop: (frame: number, stepStart: number, stepEnd: number) => LoopResult;
declare var animCheckPauseSteps: (
    steps: StepInfo[],
    currentStep: number,
    frame: number,
) => { pauseAtStep: number; pauseAtFrame: number } | null;
