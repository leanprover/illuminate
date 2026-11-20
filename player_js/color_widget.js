// @ts-check
import * as React from "react";
import { EditorContext } from "@leanprover/infoview";
const e = React.createElement;

/**
 * @typedef {{ line: number, character: number }} LspPos
 * @typedef {{ start: LspPos, end: LspPos }} LspRange
 * @typedef {{
 *   r: number, g: number, b: number, a: number,
 *   hex: string,
 *   range: LspRange,
 *   stxRange: LspRange,
 *   uri: string,
 *   pos?: LspPos
 * }} ColorProps
 */

/**
 * Checks whether an LSP position falls within an LSP range.
 * @param {LspPos} pos
 * @param {LspRange} range
 * @returns {boolean}
 */
function posInRange(pos, range) {
    if (pos.line < range.start.line || pos.line > range.end.line) return false;
    if (pos.line === range.start.line && pos.character < range.start.character) return false;
    if (pos.line === range.end.line && pos.character >= range.end.character) return false;
    return true;
}

/**
 * Converts RGB values to a 6-digit hex string.
 * @param {number} r
 * @param {number} g
 * @param {number} b
 * @returns {string}
 */
function toHex6(r, g, b) {
    const digits = [r, g, b].map((c) => c.toString(16).padStart(2, "0")).join("");
    return `#${digits}`;
}

/**
 * Converts a float alpha (0-1) to a 2-digit hex string.
 * @param {number} a
 * @returns {string}
 */
function alphaToHex(a) {
    return Math.round(a * 255)
        .toString(16)
        .padStart(2, "0");
}

/**
 * Applies a text edit to the source document via the infoview editor API.
 * @param {import("@leanprover/infoview").EditorConnection} ec
 * @param {string} uri
 * @param {{ start: { line: number, character: number }, end: { line: number, character: number } }} range
 * @param {string} newText
 */
function applyEdit(ec, uri, range, newText) {
    ec.api
        .applyEdit({
            documentChanges: [
                {
                    textDocument: { uri: uri, version: null },
                    edits: [{ range: range, newText: newText }],
                },
            ],
        })
        .catch((err) => {
            console.error("color edit error:", err);
        });
}

/**
 * Color swatch and picker widget for rgb! literals.
 * @param {ColorProps} props
 * @returns {React.ReactElement | null}
 */
export default function (props) {
    // The infoview appends `pos` (cursor position) to panel widget props.
    // Only render when the cursor is within the rgb! expression.
    if (props.pos && props.stxRange && !posInRange(props.pos, props.stxRange)) return null;

    const r = props.r,
        g = props.g,
        b = props.b,
        a = props.a;
    const hexDigits = props.hex.replace(/^#/, "");
    const hasAlpha = hexDigits.length === 8;
    const hex6 = toHex6(r, g, b);

    // Local state so the slider responds immediately while edits are debounced.
    const alphaState = React.useState(a);
    const localAlpha = alphaState[0];
    const setLocalAlpha = alphaState[1];
    // Sync local state when props change (e.g. after re-elaboration).
    React.useEffect(() => {
        setLocalAlpha(a);
    }, [a]);

    /** @type {React.MutableRefObject<ReturnType<typeof setTimeout> | null>} */
    const debounceRef = React.useRef(null);

    const ec = React.useContext(EditorContext);

    /**
     * Handles color picker change by applying a workspace edit.
     * @param {React.ChangeEvent<HTMLInputElement>} ev
     */
    const onColorChange = React.useCallback(
        /** @param {React.ChangeEvent<HTMLInputElement>} ev */
        (ev) => {
            const newHex = ev.target.value;
            if (debounceRef.current) clearTimeout(debounceRef.current);
            debounceRef.current = setTimeout(() => {
                const newStr = hasAlpha ? `"${newHex}${alphaToHex(localAlpha)}"` : `"${newHex}"`;
                applyEdit(ec, props.uri, props.range, newStr);
            }, 50);
        },
        [ec, props.uri, props.range, hasAlpha, localAlpha],
    );

    /**
     * Handles alpha slider change.
     * @param {React.ChangeEvent<HTMLInputElement>} ev
     */
    const onAlphaChange = React.useCallback(
        /** @param {React.ChangeEvent<HTMLInputElement>} ev */
        (ev) => {
            const newAlpha = parseFloat(ev.target.value);
            setLocalAlpha(newAlpha);
            if (debounceRef.current) clearTimeout(debounceRef.current);
            debounceRef.current = setTimeout(() => {
                const newStr = `"${hex6}${alphaToHex(newAlpha)}"`;
                applyEdit(ec, props.uri, props.range, newStr);
            }, 50);
        },
        [ec, props.uri, props.range, hex6],
    );

    return e(
        "div",
        {
            style: {
                padding: "8px",
                display: "flex",
                gap: "12px",
                alignItems: "flex-start",
            },
        },
        // Swatch with checkerboard behind alpha
        e(
            "div",
            {
                style: {
                    position: "relative",
                    width: "48px",
                    height: "48px",
                    flexShrink: 0,
                },
            },
            // Checkerboard background layer (only for alpha)
            hasAlpha
                ? e("div", {
                      style: {
                          position: "absolute",
                          inset: 0,
                          borderRadius: "4px",
                          backgroundImage:
                              "linear-gradient(45deg, #808080 25%, transparent 25%, transparent 75%, #808080 75%), " +
                              "linear-gradient(45deg, #808080 25%, transparent 25%, transparent 75%, #808080 75%)",
                          backgroundSize: "8px 8px",
                          backgroundPosition: "0 0, 4px 4px",
                          backgroundColor: "#c0c0c0",
                      },
                  })
                : null,
            // Color layer
            e("div", {
                style: {
                    position: "absolute",
                    inset: 0,
                    borderRadius: "4px",
                    border: "1px solid var(--vscode-widget-border, #ccc)",
                    backgroundColor: `rgba(${r},${g},${b},${localAlpha})`,
                },
            }),
        ),
        // Info and controls
        e(
            "div",
            {
                style: {
                    display: "flex",
                    flexDirection: "column",
                    gap: "4px",
                },
            },
            // Color picker input + hex label
            e(
                "div",
                {
                    style: {
                        display: "flex",
                        alignItems: "center",
                        gap: "8px",
                    },
                },
                e("input", {
                    type: "color",
                    value: hex6,
                    onChange: onColorChange,
                    style: {
                        width: "28px",
                        height: "28px",
                        padding: 0,
                        border: "1px solid var(--vscode-widget-border, #ccc)",
                        borderRadius: "3px",
                        cursor: "pointer",
                        backgroundColor: "transparent",
                    },
                }),
                e(
                    "span",
                    {
                        style: {
                            fontSize: "13px",
                            fontFamily: "var(--vscode-editor-font-family, monospace)",
                        },
                    },
                    props.hex,
                ),
            ),
            // RGB values
            e(
                "div",
                {
                    style: {
                        fontSize: "11px",
                        color: "var(--vscode-descriptionForeground, #666)",
                    },
                },
                `rgb(${r}, ${g}, ${b})${hasAlpha ? `  \u03B1 = ${localAlpha.toFixed(2)}` : ""}`,
            ),
            // Alpha slider (only for RGBA colors)
            hasAlpha
                ? e(
                      "div",
                      {
                          style: {
                              display: "flex",
                              alignItems: "center",
                              gap: "6px",
                              marginTop: "2px",
                          },
                      },
                      e(
                          "span",
                          {
                              style: {
                                  fontSize: "11px",
                                  color: "var(--vscode-descriptionForeground, #666)",
                              },
                          },
                          "\u03B1",
                      ),
                      e("input", {
                          type: "range",
                          min: "0",
                          max: "1",
                          step: "0.01",
                          value: String(localAlpha),
                          onChange: onAlphaChange,
                          style: { width: "80px" },
                      }),
                  )
                : null,
        ),
    );
}
