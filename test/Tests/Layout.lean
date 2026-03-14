import Tests.Helpers

open Illuminate

-- ══════════════════════════════════════════════════════════════════
-- Layout (5)
-- ══════════════════════════════════════════════════════════════════

def testLayout_renderDiagramM : IO Unit := do
  let d : Diagram Empty := Diagram.rect 4 2
  let svg := Id.run (renderDiagramM d)
  -- renderDiagramM should produce the same output as renderDiagram
  let expected := renderDiagram d
  assertTrue (svg == expected) "renderDiagramM matches renderDiagram"

def testLayout_namedAnchors : IO Unit := do
  let d : Diagram Empty :=
    .transform (Matrix.translate 10 20) (.named `A (Diagram.rect 4 4))
  let ld := (toLayout d).resolve
  match ld.anchorPoint `A with
  | some pos =>
    assertApproxEq pos.x 10 "anchor A x"
    assertApproxEq pos.y 20 "anchor A y"
  | none => throw <| IO.userError "anchor A not found"

def testLayout_envelopeIn : IO Unit := do
  let d : Diagram Empty := .named `box (Diagram.rect 6 4)
  let ld := (toLayout d).resolve
  match ld.envelopeIn `box Vec2.east with
  | some ext => assertApproxEq ext 3 "envelopeIn east" (tol := 0.01)
  | none => throw <| IO.userError "envelope for box not found"

def testLayout_deterministic : IO Unit := do
  let d : Diagram Empty := .compose
    (.named `X (Diagram.rect 4 4))
    (.named `Y (.transform (Matrix.translate 5 0) (Diagram.circle 3)))
  let ld1 := (toLayout d).resolve
  let ld2 := (toLayout d).resolve
  assertTrue (ld1.compile.length == ld2.compile.length) "deterministic cmd count"
  let n1 := ld1.collectNames Matrix.identity .anonymous []
  let n2 := ld2.collectNames Matrix.identity .anonymous []
  assertTrue (n1.length == n2.length) "deterministic name count"

def testLayout_monoVsFixed : IO Unit := do
  let d : Diagram Empty := .text "Hello"
  let ld1 := (toLayout d).resolve
  let ld2 := (toLayout d).resolve
  -- Same topological structure (same number of commands)
  assertTrue (ld1.compile.length == ld2.compile.length) "same cmd structure"

-- ══════════════════════════════════════════════════════════════════
-- Validate (5)
-- ══════════════════════════════════════════════════════════════════

def testValidate_wellFormed : IO Unit := do
  let d : Diagram Empty := .named `A (Diagram.rect 4 4)
  let ld := (toLayout d).resolve
  match validate ld with
  | .ok () => pure ()
  | .error errs => throw <| IO.userError s!"expected ok, got {errs.size} errors"

def testValidate_duplicateName : IO Unit := do
  let d : Diagram Empty := .compose
    (.named `foo (Diagram.rect 2 2))
    (.named `foo (Diagram.rect 2 2))
  let ld := (toLayout d).resolve
  match validate ld with
  | .ok () => throw <| IO.userError "expected duplicate error"
  | .error errs =>
    let hasDup := errs.any fun e => match e with
      | .duplicateName _ => true
      | _ => false
    assertTrue hasDup "has duplicateName error"

def testValidate_emptyPath : IO Unit := do
  let emptyPd : PathData := PathData.empty
  -- Build a LayoutDiagram containing a path prim with empty commands
  -- and a fill style so compile emits a fillPath command
  let ld : LayoutDiagram Empty :=
    .prim (.core (.path emptyPd (some { color := { r := 0, g := 0, b := 0, a := 1 } }) none))
  match validate ld with
  | .ok () => throw <| IO.userError "expected malformed path error"
  | .error errs =>
    let hasMalformed := errs.any fun e => match e with
      | .malformedPath _ => true
      | _ => false
    assertTrue hasMalformed "has malformedPath error"

def testValidate_idempotent : IO Unit := do
  let d : Diagram Empty := .named `A (Diagram.rect 4 4)
  let ld := (toLayout d).resolve
  let r1 := validate ld
  let r2 := validate ld
  match r1, r2 with
  | .ok (), .ok () => pure ()
  | _, _ => throw <| IO.userError "validation not idempotent"

def testValidate_noErrors : IO Unit := do
  let d : Diagram Empty := .compose
    (.named `A (Diagram.rect 4 4))
    (.named `B (.transform (Matrix.translate 10 0) (Diagram.rect 4 4)))
  let ld := (toLayout d).resolve
  match validate ld with
  | .ok () => pure ()
  | .error errs => throw <| IO.userError s!"expected ok, got {errs.size} errors"

def testValidate_pinOverDuplicateName : IO Unit := do
  -- Two diagrams sharing name `node` composed via pinOver should trigger duplicate name
  let d1 : Diagram Empty := Diagram.circle 10 (name := some `node)
  let d2 : Diagram Empty := Diagram.circle 10 (name := some `node)
  let combined := Diagram.compose d1 d2
  let pinned := Diagram.pinOver `node (Diagram.circle 3) combined
  let ld := (toLayout pinned).resolve
  match validate ld with
  | .ok () => throw <| IO.userError "expected duplicate name error from pinOver collision"
  | .error errs =>
    let hasDup := errs.any fun e => match e with
      | .duplicateName n => n == `node
      | _ => false
    assertTrue hasDup "pinOver with colliding names triggers duplicateName"

def layoutTests : List (String × IO Unit) := [
  -- Layout (5)
  ("Layout/renderDiagramM", testLayout_renderDiagramM),
  ("Layout/namedAnchors", testLayout_namedAnchors),
  ("Layout/envelopeIn", testLayout_envelopeIn),
  ("Layout/deterministic", testLayout_deterministic),
  ("Layout/monoVsFixed", testLayout_monoVsFixed),
  -- Validate (5)
  ("Validate/wellFormed", testValidate_wellFormed),
  ("Validate/duplicateName", testValidate_duplicateName),
  ("Validate/emptyPath", testValidate_emptyPath),
  ("Validate/idempotent", testValidate_idempotent),
  ("Validate/noErrors", testValidate_noErrors),
  ("Validate/pinOverDuplicateName", testValidate_pinOverDuplicateName)
]
