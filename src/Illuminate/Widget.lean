/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/

import Lean
import Illuminate.Diagram
import Illuminate.Render


namespace Illuminate

-- ═══════════════════════════════════════════════════════════════
-- Gadget types
-- ═══════════════════════════════════════════════════════════════

/-- A slider parameter with a label, min, max, and optional initial value. Reduces to `Float`. -/
def Slider (_name : String) (_min _max : Float)
    (_initial : Float := (_min + _max) / 2) : Type :=
  Float

/-- A text input parameter with a label and optional initial value. Reduces to `String`. -/
def TextInput (_name : String) (_initial : String := "") : Type := String

/-- A checkbox parameter with a label and optional initial value. Reduces to `Bool`. -/
def Checkbox (_name : String) (_initial : Bool := false) : Type := Bool

-- ═══════════════════════════════════════════════════════════════
-- Widget
-- ═══════════════════════════════════════════════════════════════

open Lean Widget in
/-- Widget module that renders diagrams with optional parameter controls and hit-test hover. -/
@[widget_module]
def diagramWidget : Lean.Widget.Module where
  javascript := "
import * as React from 'react';
import { useRpcSession } from '@leanprover/infoview';
const e = React.createElement;

function renderControl(p, i, values, setValues) {
  var val = values[i];
  if (p.kind === 'slider') {
    return e('div', { key: i, style: { marginBottom: '6px' } },
      e('label', { style: { fontSize: '12px', display: 'block', marginBottom: '2px' } },
        p.name + ': ' + Number(val).toFixed(2)),
      e('input', {
        type: 'range',
        min: p.min, max: p.max,
        step: (p.max - p.min) / 200,
        value: val,
        style: { width: '100%' },
        onInput: function(ev) {
          var v = values.slice();
          v[i] = parseFloat(ev.target.value);
          setValues(v);
        }
      })
    );
  } else if (p.kind === 'textInput') {
    return e('div', { key: i, style: { marginBottom: '6px' } },
      e('label', { style: { fontSize: '12px', display: 'block', marginBottom: '2px' } },
        p.name + ':'),
      e('input', {
        type: 'text',
        value: val,
        style: { width: '100%', fontSize: '12px', padding: '2px 4px' },
        onInput: function(ev) {
          var v = values.slice();
          v[i] = ev.target.value;
          setValues(v);
        }
      })
    );
  } else if (p.kind === 'checkbox') {
    return e('div', { key: i, style: { marginBottom: '6px' } },
      e('label', { style: { fontSize: '12px', cursor: 'pointer' } },
        e('input', {
          type: 'checkbox',
          checked: !!val,
          onChange: function(ev) {
            var v = values.slice();
            v[i] = ev.target.checked;
            setValues(v);
          },
          style: { marginRight: '4px' }
        }),
        p.name
      )
    );
  }
  return null;
}

export default function(props) {
  var rs = useRpcSession();
  var params = props.parameters || [];
  var hasParams = params.length > 0;
  var initials = React.useMemo(function() {
    return params.map(function(p) { return p.initial; });
  }, []);
  var _vals = React.useState(initials);
  var values = _vals[0];
  var setValues = _vals[1];
  var _svg = React.useState(props.initialSvg || '');
  var svg = _svg[0];
  var setSvg = _svg[1];
  var timer = React.useRef(null);
  var latestValues = React.useRef(initials);
  var svgRef = React.useRef(null);
  var _hitInfo = React.useState(null);
  var hitInfo = _hitInfo[0];
  var setHitInfo = _hitInfo[1];
  var hitTimer = React.useRef(null);

  // Re-evaluate diagram when parameters change
  React.useEffect(function() {
    if (!hasParams) return;
    latestValues.current = values;
    if (timer.current) clearTimeout(timer.current);
    timer.current = setTimeout(function() {
      rs.call('Illuminate.evalParamDiagram',
        { id: props.exprId, values: latestValues.current })
        .then(function(resp) { setSvg(resp.svg); })
        .catch(function(err) { console.error('RPC error:', err); });
    }, 50);
    return function() { if (timer.current) clearTimeout(timer.current); };
  }, [values]);

  // Mouse move handler for hit testing
  var onMouseMove = React.useCallback(function(ev) {
    var container = svgRef.current;
    if (!container) return;
    var svgEl = container.querySelector('svg');
    if (!svgEl) return;
    // Map client coordinates to SVG user space
    var ctm = svgEl.getScreenCTM();
    if (!ctm) return;
    var inv = ctm.inverse();
    var svgX = inv.a * ev.clientX + inv.c * ev.clientY + inv.e;
    var svgY = inv.b * ev.clientX + inv.d * ev.clientY + inv.f;
    // The SVG wraps content in <g transform=\"scale(1,-1)\">,
    // so negate y to get diagram coordinates
    var diagX = svgX;
    var diagY = -svgY;
    if (hitTimer.current) clearTimeout(hitTimer.current);
    hitTimer.current = setTimeout(function() {
      rs.call('Illuminate.hitTestDiagram',
        { id: props.exprId, x: diagX, y: diagY })
        .then(function(resp) { setHitInfo(resp); })
        .catch(function() { setHitInfo(null); });
    }, 30);
  }, []);

  var onMouseLeave = React.useCallback(function() {
    if (hitTimer.current) clearTimeout(hitTimer.current);
    setHitInfo(null);
  }, []);

  // Format hit info for display
  var hitLabel = null;
  if (hitInfo && hitInfo.kind !== 'nothing') {
    if (hitInfo.kind === 'tag') {
      hitLabel = 'tag ' + hitInfo.value;
    } else {
      hitLabel = hitInfo.kind;
    }
  }

  var controls = hasParams ? params.map(function(p, i) {
    return renderControl(p, i, values, setValues);
  }) : null;

  return e('div', { style: { padding: '4px', background: 'white', position: 'relative' } },
    controls ? e('div', { style: { marginBottom: '8px' } }, controls) : null,
    e('div', {
      ref: svgRef,
      style: { width: '100%' },
      dangerouslySetInnerHTML: { __html: svg },
      onMouseMove: onMouseMove,
      onMouseLeave: onMouseLeave
    }),
    hitLabel ? e('div', {
      style: {
        position: 'absolute',
        bottom: '4px',
        right: '8px',
        background: 'rgba(0,0,0,0.75)',
        color: 'white',
        padding: '2px 8px',
        borderRadius: '4px',
        fontSize: '11px',
        pointerEvents: 'none'
      }
    }, hitLabel) : null
  );
}
"

-- ═══════════════════════════════════════════════════════════════
-- Helpers
-- ═══════════════════════════════════════════════════════════════

/-- Renders a `Diagram Empty` to SVG with default settings. -/
def diagramToSvg (d : Diagram Empty) : String :=
  d.renderDiagram (padding := 5)

/-- Hit-tests a `Diagram Empty` at the given point. -/
def diagramHitTest (d : Diagram Empty) (x y : Float) : Click :=
  d.hitTest (Point.mk x y)

/-- Validates a diagram and returns a list of warning strings. -/
def validateDiagram (d : Diagram Empty) : List String :=
  let treeWarnings := collectWarnings d
  let layoutWarnings := match validate d with
    | .ok () => []
    | .error errs => errs.toList.map toString
  treeWarnings ++ layoutWarnings

-- ═══════════════════════════════════════════════════════════════
-- Float expression helper
-- ═══════════════════════════════════════════════════════════════

open Lean in
/-- Builds a Lean `Expr` representing a `Float` literal, including negative values. -/
private def mkFloatExpr (f : Float) : Expr :=
  -- Float.ofScientific (m : Nat) (s : Bool) (e : Nat) : Float
  -- Represents m × 10^(if s then -e else e)
  -- We use 6 decimal places of precision
  let precision : Nat := 6
  let scale := (10 : Float) ^ precision.toFloat
  let absVal := f.abs
  let mantissa := (absVal * scale).round.toUInt64.toNat
  let posExpr := mkApp3 (mkConst ``Float.ofScientific)
    (mkNatLit mantissa) (toExpr true) (mkNatLit precision)
  if f < 0 then
    mkApp (mkConst ``Float.neg) posExpr
  else
    posExpr

-- ═══════════════════════════════════════════════════════════════
-- RPC infrastructure
-- ═══════════════════════════════════════════════════════════════

/-- Stored diagram for RPC re-evaluation and hit testing. -/
structure StoredDiagram where
  /-- The environment in which the expression was elaborated. -/
  env : Lean.Environment
  /-- Options from the elaboration context. -/
  opts : Lean.Options
  /-- The elaborated expression (possibly a function taking gadget parameters). -/
  expr : Lean.Expr
  /-- Gadget specifications for each parameter (empty for static diagrams). -/
  gadgets : Array Lean.Json

/-- Global store for diagram expressions, keyed by unique ID. -/
initialize diagramStore : IO.Ref (Array (Nat × StoredDiagram)) ← IO.mkRef #[]

/-- Counter for unique diagram IDs. -/
initialize nextDiagramId : IO.Ref Nat ← IO.mkRef 0

/-- Request to evaluate a parameterized diagram with new parameter values. -/
structure EvalParamRequest where
  /-- The unique ID of the stored diagram. -/
  id : Nat
  /-- Parameter values as JSON (Float, String, or Bool). -/
  values : Array Lean.Json
deriving Lean.FromJson, Lean.ToJson

/-- Response containing the rendered SVG string. -/
structure EvalParamResponse where
  /-- The rendered SVG markup. -/
  svg : String
deriving Lean.FromJson, Lean.ToJson

/-- Request to hit-test a diagram at a point. -/
structure HitTestRequest where
  /-- The unique ID of the stored diagram. -/
  id : Nat
  /-- X coordinate in diagram space. -/
  x : Float
  /-- Y coordinate in diagram space. -/
  y : Float
deriving Lean.FromJson, Lean.ToJson

/-- Response from a hit test. -/
structure HitTestResponse where
  /-- The kind of hit: "nothing", "something", or "tag". -/
  kind : String
  /-- The tag value (only meaningful when `kind` is "tag"). -/
  value : Nat := 0
deriving Lean.FromJson, Lean.ToJson

/--
Applies stored gadget parameter values to a diagram expression, producing the
fully-applied `Diagram Empty` expression.
-/
private def applyGadgetValues (sd : StoredDiagram) (values : Array Lean.Json)
    : Except String Lean.Expr := do
  let mut app := sd.expr
  for i in [:sd.gadgets.size] do
    let gadget := sd.gadgets[i]!
    let kind := gadget.getObjValD "kind" |>.getStr? |>.toOption |>.getD ""
    let v := values[i]!
    match kind with
    | "slider" =>
      match Lean.FromJson.fromJson? v with
      | .ok (f : Float) => app := Lean.mkApp app (mkFloatExpr f)
      | .error e => throw s!"bad float: {e}"
    | "textInput" =>
      match Lean.FromJson.fromJson? v with
      | .ok (s : String) => app := Lean.mkApp app (Lean.toExpr s)
      | .error e => throw s!"bad string: {e}"
    | "checkbox" =>
      match Lean.FromJson.fromJson? v with
      | .ok (b : Bool) => app := Lean.mkApp app (Lean.toExpr b)
      | .error e => throw s!"bad bool: {e}"
    | other => throw s!"unknown gadget: {other}"
  return app

open Lean Server Elab Term Meta in
/-- Unsafe implementation of the parameterized diagram RPC evaluator. -/
private unsafe def evalParamDiagramUnsafe (req : EvalParamRequest) :
    RequestM (RequestTask EvalParamResponse) :=
  RequestM.asTask do
    let store ← diagramStore.get
    let some (_, sd) := store.find? (fun (k, _) => k == req.id)
      | throw (.mk .invalidParams "unknown diagram id" : RequestError)
    let app ← match applyGadgetValues sd req.values with
      | .ok e => pure e
      | .error msg => throw (.mk .invalidParams msg : RequestError)
    let svgExpr := mkApp (mkConst ``diagramToSvg) app
    let ctx : Core.Context := { options := sd.opts, fileName := "<rpc>", fileMap := default }
    let st : Core.State := { env := sd.env }
    let action : CoreM String := MetaM.run' (TermElabM.run' (do
      evalExpr String (mkConst ``String) svgExpr (safety := .unsafe)))
    let evalResult ← (action.run ctx st).toBaseIO
    match evalResult with
    | Except.ok (svg, _) => return ⟨svg⟩
    | Except.error _ =>
      throw (.mk .internalError "diagram evaluation failed" : RequestError)

open Lean Server in
/-- Safe wrapper for the unsafe evaluator, linked via `@[implementedBy]`. -/
@[implemented_by evalParamDiagramUnsafe]
private opaque evalParamDiagramImpl (req : EvalParamRequest) :
    RequestM (RequestTask EvalParamResponse)

open Lean Server in
/-- Server RPC method that re-evaluates a parameterized diagram with new values. -/
@[server_rpc_method]
def evalParamDiagram (req : EvalParamRequest) :
    RequestM (RequestTask EvalParamResponse) :=
  evalParamDiagramImpl req

-- ═══════════════════════════════════════════════════════════════
-- Hit-test RPC
-- ═══════════════════════════════════════════════════════════════

open Lean Server Elab Term Meta in
/-- Unsafe implementation of the hit-test RPC evaluator. -/
private unsafe def hitTestDiagramUnsafe (req : HitTestRequest) :
    RequestM (RequestTask HitTestResponse) :=
  RequestM.asTask do
    let store ← diagramStore.get
    let some (_, sd) := store.find? (fun (k, _) => k == req.id)
      | throw (.mk .invalidParams "unknown diagram id" : RequestError)
    -- For parameterized diagrams, we hit-test using the initial values
    let app ← if sd.gadgets.isEmpty then
        pure sd.expr
      else
        let initials := sd.gadgets.map fun g =>
          g.getObjValD "initial"
        match applyGadgetValues sd initials with
        | .ok e => pure e
        | .error msg => throw (.mk .invalidParams msg : RequestError)
    let hitExpr := mkApp3 (mkConst ``diagramHitTest) app
      (mkFloatExpr req.x) (mkFloatExpr req.y)
    let ctx : Core.Context := { options := sd.opts, fileName := "<rpc>", fileMap := default }
    let st : Core.State := { env := sd.env }
    let clickTy := Lean.mkConst ``Click
    let action : CoreM Click := MetaM.run' (TermElabM.run' (do
      evalExpr Click clickTy hitExpr (safety := .unsafe)))
    let evalResult ← (action.run ctx st).toBaseIO
    match evalResult with
    | Except.ok (click, _) =>
      match click with
      | .nothing => return { kind := "nothing" }
      | .something => return { kind := "something" }
      | .tag n => return { kind := "tag", value := n }
    | Except.error _ =>
      throw (.mk .internalError "hit test evaluation failed" : RequestError)

open Lean Server in
/-- Safe wrapper for the unsafe hit-test evaluator, linked via `@[implementedBy]`. -/
@[implemented_by hitTestDiagramUnsafe]
private opaque hitTestDiagramImpl (req : HitTestRequest) :
    RequestM (RequestTask HitTestResponse)

open Lean Server in
/-- Server RPC method that hit-tests a stored diagram at a given point. -/
@[server_rpc_method]
def hitTestDiagram (req : HitTestRequest) :
    RequestM (RequestTask HitTestResponse) :=
  hitTestDiagramImpl req

-- ═══════════════════════════════════════════════════════════════
-- Gadget extraction
-- ═══════════════════════════════════════════════════════════════

open Lean Meta in
/--
Extracts gadget parameter specifications from a function type.
Walks binders, recognizing `Slider`, `TextInput`, `Checkbox` applications.
Returns an array of JSON gadget specs.
-/
unsafe def extractGadgets (ty : Expr) : MetaM (Array Json) := do
  forallTelescopeReducing ty fun args _ => do
    let mut gadgets : Array Json := #[]
    for arg in args do
      let argTy ← inferType arg
      let whnfTy ← whnfR argTy
      if let some g ← matchGadget whnfTy then
        gadgets := gadgets.push g
      else
        return #[] -- non-gadget parameter: treat as static diagram
    return gadgets
where
  matchGadget (e : Expr) : MetaM (Option Json) := do
    let e ← whnfR e
    let fn := e.getAppFn
    let args := e.getAppArgs
    if fn.isConst then
      let name := fn.constName!
      if name == ``Slider && args.size >= 3 then
        let label ← evalExpr String (mkConst ``String) args[0]!
        let minVal ← evalExpr Float (mkConst ``Float) args[1]!
        let maxVal ← evalExpr Float (mkConst ``Float) args[2]!
        let initial ← if args.size >= 4 then
          evalExpr Float (mkConst ``Float) args[3]!
        else pure ((minVal + maxVal) / 2)
        return some <| Json.mkObj [
          ("kind", "slider"), ("name", toJson label),
          ("min", toJson minVal), ("max", toJson maxVal),
          ("initial", toJson initial)]
      else if name == ``TextInput && args.size >= 1 then
        let label ← evalExpr String (mkConst ``String) args[0]!
        let initial ← if args.size >= 2 then
          evalExpr String (mkConst ``String) args[1]!
        else pure ""
        return some <| Json.mkObj [
          ("kind", "textInput"), ("name", toJson label),
          ("initial", toJson initial)]
      else if name == ``Checkbox && args.size >= 1 then
        let label ← evalExpr String (mkConst ``String) args[0]!
        let initial ← if args.size >= 2 then
          evalExpr Bool (mkConst ``Bool) args[1]!
        else pure false
        return some <| Json.mkObj [
          ("kind", "checkbox"), ("name", toJson label),
          ("initial", toJson initial)]
      else return none
    else return none

-- ═══════════════════════════════════════════════════════════════
-- #diagram command
-- ═══════════════════════════════════════════════════════════════

open Lean Widget Elab Command Term Meta in
/-- Syntax for the `#diagram` command that renders a diagram in the infoview. -/
syntax (name := diagramCmd) "#diagram " term : command

open Lean Widget Elab Command Term Meta in
/-- Elaborates the `#diagram` command, evaluating the term and rendering it as SVG. -/
@[command_elab diagramCmd]
unsafe def elabDiagramCmd : CommandElab := fun stx => do
  let t := stx[1]
  liftTermElabM do
    let e ← Term.elabTerm t none
    let ty ← Meta.inferType e
    Meta.forallTelescope ty fun _args ret => do
      let diaTy ← Meta.mkAppM ``Diagram #[.const ``Empty []]
      unless ← Meta.isDefEq ret diaTy do
        throwErrorAt t "Expected a type resulting in `{diaTy}` but got `{ret}`"
    Term.synthesizeSyntheticMVarsNoPostponing
    let e ← instantiateMVars e
    let ty ← instantiateMVars ty
    let gadgets ← extractGadgets ty
    -- Store the diagram for RPC (hit testing and parameter re-evaluation)
    let env ← getEnv
    let opts ← getOptions
    let id ← nextDiagramId.modifyGet fun n => (n, n + 1)
    diagramStore.modify (·.push (id, { env, opts, expr := e, gadgets }))
    if gadgets.isEmpty then
      -- Static diagram
      let diagramType ← mkAppM ``Diagram #[mkConst ``Empty]
      let e ← Term.ensureHasType diagramType e
      let e ← instantiateMVars e
      -- Run validation and emit warnings
      let listStringType := mkApp (mkConst ``List [.zero]) (mkConst ``String)
      let warnings ← evalExpr (List String) listStringType
        (mkApp (mkConst ``validateDiagram) e)
      for w in warnings do
        logWarningAt stx (m!"#diagram: {w}")
      -- Render SVG and display widget
      let svgStr ← evalExpr String (mkConst ``String)
        (mkApp (mkConst ``diagramToSvg) e)
      let props : Json := .mkObj [
        ("exprId", toJson id),
        ("initialSvg", .str svgStr),
        ("parameters", .arr #[])]
      savePanelWidgetInfo diagramWidget.javascriptHash.val (pure props) stx
    else
      -- Parameterized diagram: evaluate at initial values, show controls
      let mut initApp := e
      for g in gadgets do
        let kind := g.getObjValD "kind" |>.getStr? |>.toOption |>.getD ""
        match kind with
        | "slider" =>
          match Lean.FromJson.fromJson? (g.getObjValD "initial") with
          | .ok (f : Float) => initApp := mkApp initApp (mkFloatExpr f)
          | .error _ => pure ()
        | "textInput" =>
          match Lean.FromJson.fromJson? (g.getObjValD "initial") with
          | .ok (s : String) => initApp := mkApp initApp (toExpr s)
          | .error _ => pure ()
        | "checkbox" =>
          match Lean.FromJson.fromJson? (g.getObjValD "initial") with
          | .ok (b : Bool) => initApp := mkApp initApp (toExpr b)
          | .error _ => pure ()
        | _ => pure ()
      let svgStr ← evalExpr String (mkConst ``String)
        (mkApp (mkConst ``diagramToSvg) initApp)
      let props : Json := .mkObj [
        ("exprId", toJson id),
        ("initialSvg", .str svgStr),
        ("parameters", .arr gadgets)]
      savePanelWidgetInfo diagramWidget.javascriptHash.val (pure props) stx
