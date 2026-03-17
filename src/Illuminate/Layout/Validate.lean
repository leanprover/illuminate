/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/

import Illuminate.Diagram
import Illuminate.Layout.Basic


namespace Illuminate

-- ═══════════════════════════════════════════════════════════════
-- Validation errors
-- ═══════════════════════════════════════════════════════════════

/-- Structural errors detected during validation. -/
inductive DiagramError where
  /-- Indicates a reference to a named anchor that does not exist. -/
  | missingAnchor   : NamePath → DiagramError
  /-- Indicates two sub-diagrams share the same hierarchical name. -/
  | duplicateName   : NamePath → DiagramError
  /-- Indicates a path primitive with invalid or empty command data. -/
  | malformedPath   : String → DiagramError
  /-- Indicates a foreign primitive with no core fallback for pure rendering. -/
  | missingFallback : String → DiagramError
deriving Repr, BEq

instance : ToString DiagramError where
  toString
    | .missingAnchor n   => s!"missing anchor: {n}"
    | .duplicateName n   => s!"duplicate name: {n}"
    | .malformedPath msg => s!"malformed path: {msg}"
    | .missingFallback m => s!"missing fallback: {m}"

-- ═══════════════════════════════════════════════════════════════
-- Validation pass
-- ═══════════════════════════════════════════════════════════════

/--
Validates a diagram for structural well-formedness before rendering.
Every diagram renders to something, but certain structural issues cause
degraded behavior. Checks for duplicate names, empty paths, and missing fallbacks.
-/
def validate {β : Type} (ld : LayoutDiagram β) : Except (Array DiagramError) Unit :=
  let names := ld.collectNames Matrix.identity .anonymous []
  let cmds := ld.compile
  let errors := checkDuplicateNames names #[]
  let errors := checkCommands cmds errors
  if errors.isEmpty then .ok ()
  else .error errors
where
  checkDuplicateNames (names : List (NamePath × Vec2)) (acc : Array DiagramError)
      : Array DiagramError :=
    let seen := names.foldl (init := (acc, ([] : List NamePath))) fun (errs, seen) (name, _) =>
      if seen.any (· == name) then
        (errs.push (.duplicateName name), seen)
      else
        (errs, name :: seen)
    seen.1
  checkCommands (cmds : List DrawCmd) (acc : Array DiagramError)
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
def collectWarnings {β : Type} (d : Diagram β) : List String :=
  go d []
where
  go (d : Diagram β) (acc : List String) : List String :=
    match d with
    | .empty | .prim _ => acc
    | .annotate _ d | .named _ d | .transform _ d | .withEnv _ d | .withConfig _ d
    | .cellophane _ d | .clip _ d => go d acc
    | .compose a b => go b (go a acc)
    | .warning msg d => go d (acc ++ [msg])
    | .deferred _ _ => acc
    | .deferredEnvelope d _ => go d acc
