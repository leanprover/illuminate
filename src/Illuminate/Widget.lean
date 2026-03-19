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
-- Widgets
-- ═══════════════════════════════════════════════════════════════

open Lean Widget in
/-- Widget module that renders static SVG HTML in the Lean infoview. -/
@[widget_module]
def svgWidget : Lean.Widget.Module where
  javascript := "
import * as React from 'react';
const e = React.createElement;
export default function(props) {
  const ref = React.useRef(null);
  React.useEffect(() => {
    if (ref.current) {
      ref.current.innerHTML = props.html || '';
      const svg = ref.current.querySelector('svg');
      if (svg) {
        svg.style.width = '100%';
        svg.style.height = 'auto';
        svg.style.maxHeight = '300px';
        svg.removeAttribute('width');
        svg.removeAttribute('height');
      }
    }
  }, [props.html]);
  return e('div', {
    ref: ref,
    style: {
      padding: '4px',
      display: 'block',
      background: 'white'
    }
  });
}
"

open Lean Widget in
/-- Widget module that renders parameterized diagrams with interactive controls. -/
@[widget_module]
def paramWidget : Lean.Widget.Module where
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

  React.useEffect(function() {
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

  var controls = params.map(function(p, i) {
    return renderControl(p, i, values, setValues);
  });

  return e('div', { style: { padding: '4px', background: 'white' } },
    e('div', { style: { marginBottom: '8px' } }, controls),
    e('div', {
      style: { width: '100%' },
      dangerouslySetInnerHTML: { __html: svg }
    })
  );
}
"

-- ═══════════════════════════════════════════════════════════════
-- Helpers
-- ═══════════════════════════════════════════════════════════════

/-- Renders a `Diagram Empty` to SVG with default settings. -/
def diagramToSvg (d : Diagram Empty) : String :=
  d.renderDiagram (padding := 5)

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
/-- Build a Lean `Expr` representing a `Float` literal, including negative values. -/
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

/-- Stored diagram function for RPC re-evaluation. -/
structure StoredParamDiagram where
  /-- The environment in which the expression was elaborated. -/
  env : Lean.Environment
  /-- Options from the elaboration context. -/
  opts : Lean.Options
  /-- The elaborated function expression. -/
  expr : Lean.Expr
  /-- Gadget specifications for each parameter. -/
  gadgets : Array Lean.Json

/-- Global store for parameterized diagram expressions, keyed by unique ID. -/
initialize paramDiagramStore : IO.Ref (Array (Nat × StoredParamDiagram)) ← IO.mkRef #[]

/-- Counter for unique parameterized diagram IDs. -/
initialize nextParamId : IO.Ref Nat ← IO.mkRef 0

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

open Lean Server Elab Term Meta in
/-- Unsafe implementation of the parameterized diagram RPC evaluator. -/
private unsafe def evalParamDiagramUnsafe (req : EvalParamRequest) :
    RequestM (RequestTask EvalParamResponse) :=
  RequestM.asTask do
    let store ← paramDiagramStore.get
    let some (_, sd) := store.find? (fun (k, _) => k == req.id)
      | throw (.mk .invalidParams "unknown diagram id" : RequestError)
    -- Build application expression from parameter values
    let mut app := sd.expr
    for i in [:sd.gadgets.size] do
      let gadget := sd.gadgets[i]!
      let kind := gadget.getObjValD "kind" |>.getStr? |>.toOption |>.getD ""
      let v := req.values[i]!
      match kind with
      | "slider" =>
        match Lean.FromJson.fromJson? v with
        | .ok (f : Float) => app := mkApp app (mkFloatExpr f)
        | .error e => throw (.mk .invalidParams s!"bad float: {e}" : RequestError)
      | "textInput" =>
        match Lean.FromJson.fromJson? v with
        | .ok (s : String) => app := mkApp app (toExpr s)
        | .error e => throw (.mk .invalidParams s!"bad string: {e}" : RequestError)
      | "checkbox" =>
        match Lean.FromJson.fromJson? v with
        | .ok (b : Bool) => app := mkApp app (toExpr b)
        | .error e => throw (.mk .invalidParams s!"bad bool: {e}" : RequestError)
      | other => throw (.mk .invalidParams s!"unknown gadget: {other}" : RequestError)
    -- Evaluate diagramToSvg(app) in the stored environment
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
-- Gadget extraction
-- ═══════════════════════════════════════════════════════════════

open Lean Meta in
/--
Extract gadget parameter specifications from a function type.
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
    if gadgets.isEmpty then
      -- Static diagram: existing behavior
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
      let props : Json := .mkObj [("html", .str svgStr)]
      savePanelWidgetInfo svgWidget.javascriptHash.val (pure props) stx
    else
      -- Parameterized diagram: store expr, evaluate at initial values, show paramWidget
      let env ← getEnv
      let opts ← getOptions
      -- Allocate unique ID
      let id ← nextParamId.modifyGet fun n => (n, n + 1)
      -- Store for RPC
      paramDiagramStore.modify (·.push (id,
        { env, opts, expr := e, gadgets }))
      -- Build initial application using initial values from gadgets
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
      -- Evaluate initial SVG
      let svgStr ← evalExpr String (mkConst ``String)
        (mkApp (mkConst ``diagramToSvg) initApp)
      -- Build widget props
      let props : Json := .mkObj [
        ("exprId", toJson id),
        ("initialSvg", .str svgStr),
        ("parameters", .arr gadgets)]
      savePanelWidgetInfo paramWidget.javascriptHash.val (pure props) stx
