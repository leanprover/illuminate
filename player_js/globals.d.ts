// Type declarations for template placeholders and external APIs used by player JS.
//
// The animation data types are declared in anim_data.d.ts.

// standalone.js and reveal.js use these placeholders that are
// string-replaced before the JS is embedded in HTML.
declare var __ILLUMINATE_DATA_98712__: AnimData;
declare var __ILLUMINATE_SELECTOR_98712__: string;

// reveal.js uses the Reveal.js API when available (3.x and 4.x)
declare var Reveal: {
    addEventListener?(type: string, fn: (e: { fragment: HTMLElement }) => void): void;
    on?(type: string, fn: (e: { fragment: HTMLElement }) => void): void;
};
