/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/

import Lean
import Std.Data.HashMap
import Illuminate.Diagram
import Illuminate.Render
import Illuminate.Backend.SVG


namespace Illuminate

/-!
# DiagramWithInfo
-/

/-- A diagram bundled with human-readable labels for tagged regions. -/
structure DiagramWithInfo where
  /-- The diagram to render. -/
  diagram : Diagram SVG
  /-- Maps tag IDs to human-readable region labels shown on hover. -/
  regions : Std.HashMap Nat String := {}

instance : Coe (Diagram SVG) DiagramWithInfo where
  coe d := { diagram := d }

/-!
# Gadget types
-/

/-- A slider parameter with a label, min, max, and optional initial value. Reduces to {name}`Float`. -/
def Slider (_name : String) (_min _max : Float)
    (_initial : Float := (_min + _max) / 2) : Type :=
  Float

/-- A text input parameter with a label and optional initial value. Reduces to {name}`String`. -/
def TextInput (_name : String) (_initial : String := "") : Type := String

/-- A checkbox parameter with a label and optional initial value. Reduces to {name}`Bool`. -/
def Checkbox (_name : String) (_initial : Bool := false) : Type := Bool

/-!
# Widget
-/

open Lean Widget in
/-- Widget module that renders diagrams with optional parameter controls and hit-test hover. -/
@[widget_module]
def diagramWidget : Lean.Widget.Module where
  javascript := include_str "../../player_js/diagram_widget.js"

/-!
# Helpers
-/

/-- Renders a {lean}`Diagram SVG` to SVG with default settings. -/
def diagramToSvg (d : Diagram SVG) (viewBoxPixelWidth : Float := 0) : String :=
  d.renderDiagram (padding := 5) (viewBoxPixelWidth := viewBoxPixelWidth)

/-- Hit-tests a {lean}`Diagram SVG` at the given point. -/
def diagramHitTest (d : Diagram SVG) (x y : Float) : Click :=
  d.hitTest (Point.mk x y)

/-- Renders a {name}`DiagramWithInfo` to SVG with default settings. -/
def dwiToSvg (dwi : DiagramWithInfo) (viewBoxPixelWidth : Float := 0) : String :=
  dwi.diagram.renderDiagram (padding := 5) (viewBoxPixelWidth := viewBoxPixelWidth)

/-- Hit-tests a {name}`DiagramWithInfo` at the given point. -/
def dwiHitTest (dwi : DiagramWithInfo) (x y : Float) : Click :=
  dwi.diagram.hitTest (Point.mk x y)

/-- Validates a diagram and returns a list of warning strings. -/
def validateDiagram (d : Diagram SVG) : List String :=
  let treeWarnings := collectWarnings d
  let layoutWarnings := match validate d with
    | .ok () => []
    | .error errs => errs.toList.map toString
  treeWarnings ++ layoutWarnings

/-!
# Float expression helper
-/

open Lean in
/-- Builds a Lean {name}`Expr` representing a {name}`Float` literal, including negative values. -/
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

/-!
# RPC infrastructure
-/

/-- Stored diagram for RPC re-evaluation and hit testing. -/
structure StoredDiagram where
  /-- The environment in which the expression was elaborated. -/
  env : Lean.Environment
  /-- Options from the elaboration context. -/
  opts : Lean.Options
  /-- The elaborated diagram expression (possibly a function taking gadget parameters). -/
  expr : Lean.Expr
  /-- Gadget specifications for each parameter (empty for static diagrams). -/
  gadgets : Array Lean.Json
  /-- Maps tag IDs to human-readable labels, evaluated at elaboration time. -/
  regions : Std.HashMap Nat String := {}
  /-- Whether the stored expression produces {name}`DiagramWithInfo` (after applying gadget values). -/
  returnsDwi : Bool := false

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
  /-- Pixel width of the widget container, for resolving scale-invariant lengths. -/
  pixelWidth : Float := 0
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
  /-- Current parameter values (for parameterized diagrams). -/
  values : Array Lean.Json := #[]
deriving Lean.FromJson, Lean.ToJson

/-- Response from a hit test. -/
structure HitTestResponse where
  /-- The kind of hit: "nothing", "something", or "tag". -/
  kind : String
  /-- The tag value (only meaningful when {name (full := HitTestResponse.kind)}`kind` is "tag"). -/
  value : Nat := 0
  /-- Human-readable label for the hit region, if available. -/
  label : String := ""
deriving Lean.FromJson, Lean.ToJson

/--
Applies stored gadget parameter values to a diagram expression, producing the
fully-applied {lean}`Diagram SVG` expression.
-/
private def applyGadgetValues (sd : StoredDiagram) (values : Array Lean.Json) :
    Except String Lean.Expr := do
  let mut app := sd.expr
  if values.size < sd.gadgets.size then
    throw s!"expected {sd.gadgets.size} gadget values, got {values.size}"
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
    let diagApp := if sd.returnsDwi
      then mkApp (mkConst ``DiagramWithInfo.diagram) app else app
    let svgExpr := mkApp2 (mkConst ``diagramToSvg) diagApp (mkFloatExpr req.pixelWidth)
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
/-- Safe wrapper for the unsafe evaluator, linked via {attr}`@[implemented_by]`. -/
@[implemented_by evalParamDiagramUnsafe]
private opaque evalParamDiagramImpl (req : EvalParamRequest) :
    RequestM (RequestTask EvalParamResponse)

open Lean Server in
/-- Server RPC method that re-evaluates a parameterized diagram with new values. -/
@[server_rpc_method]
def evalParamDiagram (req : EvalParamRequest) :
    RequestM (RequestTask EvalParamResponse) :=
  evalParamDiagramImpl req

/-!
# Hit-test RPC
-/

open Lean Server Elab Term Meta in
/-- Unsafe implementation of the hit-test RPC evaluator. -/
private unsafe def hitTestDiagramUnsafe (req : HitTestRequest) :
    RequestM (RequestTask HitTestResponse) :=
  RequestM.asTask do
    let store ← diagramStore.get
    let some (_, sd) := store.find? (fun (k, _) => k == req.id)
      | throw (.mk .invalidParams "unknown diagram id" : RequestError)
    -- Apply current parameter values (or initials if not provided)
    let app ← if sd.gadgets.isEmpty then
        pure sd.expr
      else
        let values := if req.values.isEmpty then
          sd.gadgets.map fun g => g.getObjValD "initial"
        else req.values
        match applyGadgetValues sd values with
        | .ok e => pure e
        | .error msg => throw (.mk .invalidParams msg : RequestError)
    let ctx : Core.Context := { options := sd.opts, fileName := "<rpc>", fileMap := default }
    let st : Core.State := { env := sd.env }
    -- If the expression returns DiagramWithInfo, evaluate it to extract fresh regions
    let (diagApp, regions) ← if sd.returnsDwi then do
        let dwiTy := Lean.mkConst ``DiagramWithInfo
        let evalDwi : CoreM DiagramWithInfo := MetaM.run' (TermElabM.run' (do
          evalExpr DiagramWithInfo dwiTy app (safety := .unsafe)))
        let dwiResult ← (evalDwi.run ctx st).toBaseIO
        match dwiResult with
        | Except.ok (dwi, _) =>
          let diagExpr := mkApp (mkConst ``DiagramWithInfo.diagram) app
          pure (diagExpr, dwi.regions)
        | Except.error _ =>
          pure (mkApp (mkConst ``DiagramWithInfo.diagram) app, sd.regions)
      else
        pure (app, sd.regions)
    let hitExpr := mkApp3 (mkConst ``diagramHitTest) diagApp
      (mkFloatExpr req.x) (mkFloatExpr req.y)
    let clickTy := Lean.mkConst ``Click
    let action : CoreM Click := MetaM.run' (TermElabM.run' (do
      evalExpr Click clickTy hitExpr (safety := .unsafe)))
    let evalResult ← (action.run ctx st).toBaseIO
    match evalResult with
    | Except.ok (click, _) =>
      match click with
      | .nothing => return { kind := "nothing" }
      | .something => return { kind := "something" }
      | .tag n =>
        let label := regions[n]?.getD ""
        return { kind := "tag", value := n, label }
    | Except.error _ =>
      throw (.mk .internalError "hit test evaluation failed" : RequestError)

open Lean Server in
/-- Safe wrapper for the unsafe hit-test evaluator, linked via {attr}`@[implemented_by]`. -/
@[implemented_by hitTestDiagramUnsafe]
private opaque hitTestDiagramImpl (req : HitTestRequest) :
    RequestM (RequestTask HitTestResponse)

open Lean Server in
/-- Server RPC method that hit-tests a stored diagram at a given point. -/
@[server_rpc_method]
def hitTestDiagram (req : HitTestRequest) :
    RequestM (RequestTask HitTestResponse) :=
  hitTestDiagramImpl req

/-!
# Gadget extraction
-/

open Lean Meta in
/--
Extracts gadget parameter specifications from a function type.
Walks binders, recognizing {name}`Slider`, {name}`TextInput`, {name}`Checkbox` applications.
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

/-!
# #diagram command
-/

open Lean Widget Elab Command Term Meta in
/-- A command that renders a diagram in the InfoView. -/
syntax (name := diagramCmd) "#diagram " term : command

open Lean Widget Elab Command Term Meta in
/-- Applies initial gadget values to a parameterized diagram expression. -/
private unsafe def applyInitialGadgetValues (e : Expr) (gadgets : Array Json) : TermElabM Expr := do
  let mut app := e
  for g in gadgets do
    let kind := g.getObjValD "kind" |>.getStr? |>.toOption |>.getD ""
    match kind with
    | "slider" =>
      match Lean.FromJson.fromJson? (g.getObjValD "initial") with
      | .ok (f : Float) => app := mkApp app (mkFloatExpr f)
      | .error _ => pure ()
    | "textInput" =>
      match Lean.FromJson.fromJson? (g.getObjValD "initial") with
      | .ok (s : String) => app := mkApp app (toExpr s)
      | .error _ => pure ()
    | "checkbox" =>
      match Lean.FromJson.fromJson? (g.getObjValD "initial") with
      | .ok (b : Bool) => app := mkApp app (toExpr b)
      | .error _ => pure ()
    | _ => pure ()
  return app

open Lean Widget Elab Command Term Meta in
/-- Elaborates the {kw}`#diagram` command, evaluating the term and rendering it as SVG. -/
@[command_elab diagramCmd]
unsafe def elabDiagramCmd : CommandElab := fun stx => do
  let t := stx[1]
  liftTermElabM do
    let e ← Term.elabTerm t none
    let ty ← Meta.inferType e
    -- Validate and unify the return type
    let dwiTy := Lean.mkConst ``DiagramWithInfo
    Meta.forallTelescope ty fun _args ret => do
      let diaTy ← Meta.mkAppM ``Diagram #[.const ``SVG []]
      -- Try Diagram SVG first (this unifies β = SVG)
      unless ← Meta.isDefEq ret diaTy do
        -- Otherwise accept DiagramWithInfo
        unless ← Meta.isDefEq ret dwiTy do
          throwErrorAt t "Expected `DiagramWithInfo` or `Diagram SVG` but got `{ret}`"
    Term.synthesizeSyntheticMVarsNoPostponing
    let e ← instantiateMVars e
    -- Don't evaluate if elaboration produced errors (e.g., type mismatches)
    if (← Core.getMessageLog).hasErrors then return
    let ty ← instantiateMVars ty
    let gadgets ← extractGadgets ty
    -- Check if the return type is DiagramWithInfo (vs Diagram SVG)
    let retTy ← Meta.forallTelescope ty fun _args ret => do whnf ret
    let returnsDwi := retTy.isAppOf ``DiagramWithInfo
    let env ← getEnv
    let opts ← getOptions
    let id ← nextDiagramId.modifyGet fun n => (n, n + 1)
    -- Apply initial gadget values to get a concrete value
    let initExpr ← if gadgets.isEmpty then pure e
                    else applyInitialGadgetValues e gadgets
    -- Coerce to DiagramWithInfo (inserts Coe if the term is Diagram SVG)
    let initDwi ← Term.ensureHasType dwiTy initExpr
    let initDwi ← instantiateMVars initDwi
    let dwi ← evalExpr DiagramWithInfo dwiTy initDwi (safety := .unsafe)
    let regions := dwi.regions
    -- Project .diagram to get Diagram SVG for rendering/hit-testing
    let diagExpr := mkApp (mkConst ``DiagramWithInfo.diagram) initDwi
    -- For RPC, store a Diagram-SVG-producing expression:
    -- for static diagrams, store the projected diagram directly;
    -- for parameterized, store the original (RPC applies values then renders)
    let storedExpr := if gadgets.isEmpty then diagExpr else e
    let sd : StoredDiagram := { env, opts, expr := storedExpr, gadgets, regions, returnsDwi }
    diagramStore.modify (·.push (id, sd))
    if gadgets.isEmpty then
      let listStringType := mkApp (mkConst ``List [.zero]) (mkConst ``String)
      let warnings ← evalExpr (List String) listStringType
        (mkApp (mkConst ``validateDiagram) diagExpr)
      for w in warnings do
        logWarningAt stx (m!"#diagram: {w}")
    let svgStr ← evalExpr String (mkConst ``String)
      (mkApp2 (mkConst ``diagramToSvg) diagExpr (mkFloatExpr 0))
    let props : Json := .mkObj [
      ("exprId", toJson id),
      ("initialSvg", .str svgStr),
      ("parameters", .arr gadgets)]
    savePanelWidgetInfo diagramWidget.javascriptHash.val (pure props) stx
