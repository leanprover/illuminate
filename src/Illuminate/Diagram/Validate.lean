/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/

import Illuminate.Diagram.Compile


namespace Illuminate

-- ═══════════════════════════════════════════════════════════════
-- Validation errors
-- ═══════════════════════════════════════════════════════════════

/-- Structural errors detected during validation. -/
inductive DiagramError where
  /-- Indicates two sub-diagrams share the same hierarchical name. -/
  | duplicateName : Lean.Name → DiagramError
  /-- Indicates a path primitive with invalid or empty command data. -/
  | malformedPath : String → DiagramError
deriving Repr, BEq

instance : ToString DiagramError where
  toString
    | .duplicateName n   => s!"duplicate name: {n}"
    | .malformedPath msg => s!"malformed path: {msg}"

-- ═══════════════════════════════════════════════════════════════
-- Validation pass
-- ═══════════════════════════════════════════════════════════════

/--
Validates a diagram for structural well-formedness before rendering.
Checks for duplicate names and empty paths.
-/
def validate {β : Type} [Backend β] (d : Diagram β) : Except (Array DiagramError) Unit :=
  let names := d.collectNames Matrix.identity .anonymous []
  let cmds := d.compile
  let errors := checkDuplicateNames names #[]
  let errors := checkCommands cmds errors
  if errors.isEmpty then .ok ()
  else .error errors
where
  checkDuplicateNames (names : List (Lean.Name × Vec2)) (acc : Array DiagramError)
      : Array DiagramError :=
    let seen := names.foldl (init := (acc, ([] : List Lean.Name))) fun (errs, seen) (name, _) =>
      if seen.any (· == name) then
        (errs.push (.duplicateName name), seen)
      else
        (errs, name :: seen)
    seen.1
  checkCommands (cmds : Array (DrawCmd β)) (acc : Array DiagramError)
      : Array DiagramError :=
    cmds.foldl (init := acc) fun errs cmd =>
      match cmd with
      | .fillPath pd _ =>
        if pd.commands.isEmpty then errs.push (.malformedPath "empty fill path")
        else errs
      | .strokePath pd _ =>
        if pd.commands.isEmpty then errs.push (.malformedPath "empty stroke path")
        else errs
      | _ => errs

/-- Collects all warning messages embedded in a diagram tree. -/
def collectWarnings {β : Type} [Backend β] (d : Diagram β) : List String :=
  go d []
where
  go (d : Diagram β) (acc : List String) : List String :=
    match d with
    | .empty | .prim _ => acc
    | .foreign _ d | .tag _ d | .named _ d | .transform _ d | .withEnv _ d
    | .cellophane _ d | .clip _ d => go d acc
    | .compose a b => go b (go a acc)
    | .warning msg d => go d (acc ++ [msg])
