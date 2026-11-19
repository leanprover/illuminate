// The shape of the compiled animation data that the players read.

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
