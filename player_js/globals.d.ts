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

// standalone.js and reveal.js use these placeholders that are
// string-replaced before the JS is embedded in HTML.
declare var __ILLUMINATE_DATA_98712__: AnimData;
declare var __ILLUMINATE_SELECTOR_98712__: string;

// reveal.js uses the Reveal.js API when available (3.x and 4.x)
declare var Reveal: {
    addEventListener?(type: string, fn: (e: { fragment: HTMLElement }) => void): void;
    on?(type: string, fn: (e: { fragment: HTMLElement }) => void): void;
};
