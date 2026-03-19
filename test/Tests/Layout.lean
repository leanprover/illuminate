/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/

import Tests.Helpers

open Illuminate

-- ══════════════════════════════════════════════════════════════════
-- Name resolution (5)
-- ══════════════════════════════════════════════════════════════════

def testLayout_renderDiagram : IO Unit := do
  let d : Diagram Empty := Diagram.rect 4 2
  let svg := d.renderDiagram
  assertContains svg "<svg" "has svg tag"
  assertContains svg "<path" "has path"

def testLayout_namedAnchors : IO Unit := do
  let d : Diagram Empty :=
    .transform (Matrix.translate 10 20) (.named `A (Diagram.rect 4 4))
  let pos := (d.find `A).origin
  assertApproxEq pos.x 10 "anchor A x"
  assertApproxEq pos.y 20 "anchor A y"

def testLayout_envelopeIn : IO Unit := do
  let d : Diagram Empty := .named `box (Diagram.rect 6 4)
  let env := (d.find `box).getEnvelope
  assertApproxEq (env[Vec2.east]) 3 "envelope east" (tol := 0.01)

def testLayout_deterministic : IO Unit := do
  let d : Diagram Empty := .compose
    (.named `X (Diagram.rect 4 4))
    (.named `Y (.transform (Matrix.translate 5 0) (Diagram.circle 3)))
  let cmds1 := d.compile
  let cmds2 := d.compile
  assertTrue (cmds1.length == cmds2.length) "deterministic cmd count"
  let n1 := d.collectNames Matrix.identity .anonymous []
  let n2 := d.collectNames Matrix.identity .anonymous []
  assertTrue (n1.length == n2.length) "deterministic name count"

def testLayout_compileText : IO Unit := do
  let d : Diagram Empty := .text "Hello"
  let cmds1 := d.compile
  let cmds2 := d.compile
  assertTrue (cmds1.length == cmds2.length) "same cmd structure"

-- ══════════════════════════════════════════════════════════════════
-- Validate (5)
-- ══════════════════════════════════════════════════════════════════

def testValidate_wellFormed : IO Unit := do
  let d : Diagram Empty := .named `A (Diagram.rect 4 4)
  match validate d with
  | .ok () => pure ()
  | .error errs => throw <| IO.userError s!"expected ok, got {errs.size} errors"

def testValidate_duplicateName : IO Unit := do
  let d : Diagram Empty := .compose
    (.named `foo (Diagram.rect 2 2))
    (.named `foo (Diagram.rect 2 2))
  match validate d with
  | .ok () => throw <| IO.userError "expected duplicate error"
  | .error errs =>
    let hasDup := errs.any fun e => match e with
      | .duplicateName _ => true
      | _ => false
    assertTrue hasDup "has duplicateName error"

def testValidate_emptyPath : IO Unit := do
  let emptyPd : PathData := PathData.empty
  let d : Diagram Empty :=
    .prim (.core (.path emptyPd { color := (⟨0, 0, 0, 1⟩ : Color) } {}))
  match validate d with
  | .ok () => throw <| IO.userError "expected malformed path error"
  | .error errs =>
    let hasMalformed := errs.any fun e => match e with
      | .malformedPath _ => true
      | _ => false
    assertTrue hasMalformed "has malformedPath error"

def testValidate_idempotent : IO Unit := do
  let d : Diagram Empty := .named `A (Diagram.rect 4 4)
  let r1 := validate d
  let r2 := validate d
  match r1, r2 with
  | .ok (), .ok () => pure ()
  | _, _ => throw <| IO.userError "validation not idempotent"

def testValidate_noErrors : IO Unit := do
  let d : Diagram Empty := .compose
    (.named `A (Diagram.rect 4 4))
    (.named `B (.transform (Matrix.translate 10 0) (Diagram.rect 4 4)))
  match validate d with
  | .ok () => pure ()
  | .error errs => throw <| IO.userError s!"expected ok, got {errs.size} errors"

def testValidate_pinOverDuplicateName : IO Unit := do
  let d1 : Diagram Empty := Diagram.circle 10 (name := some `node)
  let d2 : Diagram Empty := Diagram.circle 10 (name := some `node)
  let combined := Diagram.compose d1 d2
  let pinned := Diagram.pinOver `node (Diagram.circle 3) combined
  match validate pinned with
  | .ok () => throw <| IO.userError "expected duplicate name error from pinOver collision"
  | .error errs =>
    let hasDup := errs.any fun e => match e with
      | .duplicateName n => n == `node
      | _ => false
    assertTrue hasDup "pinOver with colliding names triggers duplicateName"

def testConnect_explicitArrowhead : IO Unit := do
  -- Passing an explicit arrowhead parameter to connect should produce an arrowhead.
  let d : Diagram Empty :=
    Diagram.hsep 40 [
      Diagram.circle 10 (name := some `A),
      Diagram.circle 10 (name := some `B)
    ]
    |>.connect `A.east `B.west (arrowhead := some { type := .stealth })
  -- Stealth arrowheads produce a filled polygon, which compiles to a fillPath.
  -- The two circles each produce one fillPath (2 total), plus one for the stealth head.
  let fillCmds := d.compile.filter fun cmd => match cmd with
    | .fillPath _ _ => true
    | _ => false
  assertTrue (fillCmds.length == 3)
    s!"explicit arrowhead should produce a fill (expected 3 fills, got {fillCmds.length})"


def layoutTests : List (String × IO Unit) := [
  -- Name resolution (5)
  ("Layout/renderDiagram", testLayout_renderDiagram),
  ("Layout/namedAnchors", testLayout_namedAnchors),
  ("Layout/envelopeIn", testLayout_envelopeIn),
  ("Layout/deterministic", testLayout_deterministic),
  ("Layout/compileText", testLayout_compileText),
  -- Validate (5)
  ("Validate/wellFormed", testValidate_wellFormed),
  ("Validate/duplicateName", testValidate_duplicateName),
  ("Validate/emptyPath", testValidate_emptyPath),
  ("Validate/idempotent", testValidate_idempotent),
  ("Validate/noErrors", testValidate_noErrors),
  ("Validate/pinOverDuplicateName", testValidate_pinOverDuplicateName),
  -- Connect config regression (issue 2)
  ("Connect/explicitArrowhead", testConnect_explicitArrowhead)
]
