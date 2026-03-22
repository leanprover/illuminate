// @ts-check
import * as React from "react";
import { useRpcSession } from "@leanprover/infoview";
const e = React.createElement;

/**
 * @typedef {{ kind: 'slider', name: string, min: number, max: number, initial: number }} SliderParam
 * @typedef {{ kind: 'textInput', name: string, initial: string }} TextInputParam
 * @typedef {{ kind: 'checkbox', name: string, initial: boolean }} CheckboxParam
 * @typedef {SliderParam | TextInputParam | CheckboxParam} GadgetParam
 * @typedef {{ exprId: number, initialSvg: string, parameters: GadgetParam[] }} DiagramProps
 * @typedef {{ kind: string, value?: number, label?: string }} HitInfo
 * @typedef {(number | string | boolean)} ParamValue
 */

/**
 * Renders a single gadget control (slider, text input, or checkbox).
 * @param {GadgetParam} p
 * @param {number} i
 * @param {ParamValue[]} values
 * @param {(v: ParamValue[]) => void} setValues
 * @returns {React.ReactElement | null}
 */
function renderControl(p, i, values, setValues) {
    var val = values[i];
    if (p.kind === "slider") {
        return e(
            "div",
            { key: i, style: { marginBottom: "6px" } },
            e(
                "label",
                { style: { fontSize: "12px", display: "block", marginBottom: "2px" } },
                p.name + ": " + Number(val).toFixed(2),
            ),
            e("input", {
                type: "range",
                min: p.min,
                max: p.max,
                step: (p.max - p.min) / 200,
                value: /** @type {number} */ (val),
                style: { width: "100%" },
                onInput: /** @param {React.FormEvent<HTMLInputElement>} ev */ function (ev) {
                    var v = values.slice();
                    v[i] = parseFloat(/** @type {HTMLInputElement} */ (ev.target).value);
                    setValues(v);
                },
            }),
        );
    } else if (p.kind === "textInput") {
        return e(
            "div",
            { key: i, style: { marginBottom: "6px" } },
            e(
                "label",
                { style: { fontSize: "12px", display: "block", marginBottom: "2px" } },
                p.name + ":",
            ),
            e("input", {
                type: "text",
                value: /** @type {string} */ (val),
                style: { width: "100%", fontSize: "12px", padding: "2px 4px" },
                onInput: /** @param {React.FormEvent<HTMLInputElement>} ev */ function (ev) {
                    var v = values.slice();
                    v[i] = /** @type {HTMLInputElement} */ (ev.target).value;
                    setValues(v);
                },
            }),
        );
    } else if (p.kind === "checkbox") {
        return e(
            "div",
            { key: i, style: { marginBottom: "6px" } },
            e(
                "label",
                { style: { fontSize: "12px", cursor: "pointer" } },
                e("input", {
                    type: "checkbox",
                    checked: !!val,
                    onChange: /** @param {React.ChangeEvent<HTMLInputElement>} ev */ function (ev) {
                        var v = values.slice();
                        v[i] = ev.target.checked;
                        setValues(v);
                    },
                    style: { marginRight: "4px" },
                }),
                p.name,
            ),
        );
    }
    return null;
}

/**
 * Diagram widget component for the Lean infoview.
 * @param {DiagramProps} props
 * @returns {React.ReactElement}
 */
export default function (props) {
    var rs = useRpcSession();
    var params = props.parameters || [];
    var hasParams = params.length > 0;
    /** @type {ParamValue[]} */
    var initials = React.useMemo(function () {
        return params.map(function (p) {
            return p.initial;
        });
    }, []);
    var _vals = React.useState(initials);
    var values = _vals[0];
    var setValues = _vals[1];
    var _svg = React.useState(props.initialSvg || "");
    var svg = _svg[0];
    var setSvg = _svg[1];
    /** @type {React.MutableRefObject<ReturnType<typeof setTimeout> | null>} */
    var timer = React.useRef(null);
    var latestValues = React.useRef(initials);
    /** @type {React.MutableRefObject<HTMLDivElement | null>} */
    var svgRef = React.useRef(null);
    var _hitInfo = React.useState(/** @type {HitInfo | null} */ (null));
    var hitInfo = _hitInfo[0];
    var setHitInfo = _hitInfo[1];
    /** @type {React.MutableRefObject<ReturnType<typeof setTimeout> | null>} */
    var hitTimer = React.useRef(null);

    // Re-evaluate diagram when parameters change
    React.useEffect(
        function () {
            if (!hasParams) return;
            latestValues.current = values;
            if (timer.current) clearTimeout(timer.current);
            timer.current = setTimeout(function () {
                rs.call("Illuminate.evalParamDiagram", {
                    id: props.exprId,
                    values: latestValues.current,
                })
                    .then(function (/** @type {{ svg: string }} */ resp) {
                        setSvg(resp.svg);
                    })
                    .catch(function (/** @type {unknown} */ err) {
                        console.error("RPC error:", err);
                    });
            }, 50);
            return function () {
                if (timer.current) clearTimeout(timer.current);
            };
        },
        [values],
    );

    // Mouse move handler for hit testing
    var onMouseMove = React.useCallback(
        /** @param {React.MouseEvent} ev */ function (ev) {
            var container = svgRef.current;
            if (!container) return;
            /** @type {SVGSVGElement | null} */
            var svgEl = container.querySelector("svg");
            if (!svgEl) return;
            // Map client coordinates to SVG user space
            var ctm = svgEl.getScreenCTM();
            if (!ctm) return;
            var inv = ctm.inverse();
            var svgX = inv.a * ev.clientX + inv.c * ev.clientY + inv.e;
            var svgY = inv.b * ev.clientX + inv.d * ev.clientY + inv.f;
            // The SVG wraps content in <g transform="scale(1,-1)">,
            // so negate y to get diagram coordinates
            var diagX = svgX;
            var diagY = -svgY;
            if (hitTimer.current) clearTimeout(hitTimer.current);
            hitTimer.current = setTimeout(function () {
                rs.call("Illuminate.hitTestDiagram", {
                    id: props.exprId,
                    x: diagX,
                    y: diagY,
                    values: latestValues.current,
                })
                    .then(function (/** @type {HitInfo} */ resp) {
                        setHitInfo(resp);
                    })
                    .catch(function () {
                        setHitInfo(null);
                    });
            }, 30);
        },
        [],
    );

    var onMouseLeave = React.useCallback(function () {
        if (hitTimer.current) clearTimeout(hitTimer.current);
        setHitInfo(null);
    }, []);

    // Format hit info for display
    /** @type {string | null} */
    var hitLabel = null;
    if (hitInfo && hitInfo.kind !== "nothing") {
        if (hitInfo.label) {
            hitLabel = hitInfo.label;
        } else if (hitInfo.kind === "tag") {
            hitLabel = "tag " + hitInfo.value;
        } else {
            hitLabel = hitInfo.kind;
        }
    }

    var controls = hasParams
        ? params.map(function (p, i) {
              return renderControl(p, i, values, setValues);
          })
        : null;

    return e(
        "div",
        { style: { padding: "4px", background: "white", position: "relative" } },
        controls ? e("div", { style: { marginBottom: "8px" } }, controls) : null,
        e("div", {
            ref: svgRef,
            style: { width: "100%" },
            dangerouslySetInnerHTML: { __html: svg },
            onMouseMove: onMouseMove,
            onMouseLeave: onMouseLeave,
        }),
        hitLabel
            ? e(
                  "div",
                  {
                      style: {
                          position: "absolute",
                          bottom: "4px",
                          right: "8px",
                          background: "rgba(0,0,0,0.75)",
                          color: "white",
                          padding: "2px 8px",
                          borderRadius: "4px",
                          fontSize: "11px",
                          pointerEvents: "none",
                      },
                  },
                  hitLabel,
              )
            : null,
    );
}
