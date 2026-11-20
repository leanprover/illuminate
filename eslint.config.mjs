import globals from "globals";
import shared from "./eslint.shared.mjs";

// The players in `player_js/` are classic scripts. The infoview widgets are ES modules. The shared
// presets and rules come last.
export default [
    {
        ignores: ["node_modules/", ".lake/", "**/.lake/", "vendored_js/", "test_output/"],
    },
    {
        files: ["**/*.js"],
        languageOptions: {
            ecmaVersion: 2024,
            sourceType: "script",
            globals: { ...globals.browser },
        },
    },
    {
        // The Lean build replaces this placeholder with the animation data. A comment that named
        // it would receive the data too.
        files: ["player_js/standalone.js", "player_js/reveal.js"],
        languageOptions: {
            globals: { __ILLUMINATE_DATA_98712__: "readonly" },
        },
    },
    {
        files: [
            "player_js/animate_widget.js",
            "player_js/color_widget.js",
            "player_js/diagram_widget.js",
        ],
        languageOptions: {
            sourceType: "module",
        },
    },
    ...shared,
];
