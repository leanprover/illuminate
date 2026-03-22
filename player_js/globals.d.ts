// Type declarations for template placeholders and external APIs used by player JS.

interface StepInfo {
    frame: number;
    pause: boolean;
    loop: boolean;
}
interface ParamBinding {
    e: number;
    a: string;
}
interface Segment {
    sf: number;
    fc: number;
    sync: string;
    svgs: string[];
    pmap: ParamBinding[];
    params: string[][];
    /** Runtime cache of indexed SVG elements, populated by the widget player. */
    _elems?: Element[];
}
interface AnimData {
    fps: number;
    totalFrames: number;
    segments: Segment[];
    steps: StepInfo[];
}

// standalone.js and reveal.js use __DATA__ and __SELECTOR__ as placeholders
// that are string-replaced before the JS is embedded in HTML.
declare var __DATA__: AnimData;
declare var __SELECTOR__: string;

// reveal.js uses the Reveal.js API when available
declare var Reveal: {
    addEventListener(type: string, fn: (e: { fragment: HTMLElement }) => void): void;
};
