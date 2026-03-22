/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/

import Lean
import Illuminate.Animation.Animate
import Illuminate.Animation.Compile
import Illuminate.Animation.Render
import Illuminate.Widget
import Illuminate.Backend.SVG


namespace Illuminate

/--
Wraps an animation render function for preview in the Lean infoview via `#diagram`.

The returned function takes a time slider value and produces the diagram at that time,
allowing interactive scrubbing through the animation.
-/
def previewAnimation (steps : List Step)
    (render : Vector Float steps.length → Diagram SVG)
    : Slider "time" 0 (totalDuration steps) 0 → Diagram SVG :=
  fun time =>
    let progress := progressAt steps time
    let vec := progressVector progress steps.length
    render vec

-- ═══════════════════════════════════════════════════════════════
-- Animation widget
-- ═══════════════════════════════════════════════════════════════

open Lean Widget in
/-- Widget module that plays compiled animations with requestAnimationFrame-driven SVG playback. -/
@[widget_module]
def animateWidget : Lean.Widget.Module where
  javascript := include_str "../../../player_js/animate_widget.js"

-- ═══════════════════════════════════════════════════════════════
-- #animate command
-- ═══════════════════════════════════════════════════════════════

/-- Syntax for the `#animate` command that plays an animation in the infoview. -/
syntax (name := animateCmd) "#animate " ("(" &"fps" " := " num ") ")? term:max term : command

open Lean Widget Elab Command Term Meta in
/-- Elaborates the `#animate` command, compiling the animation and rendering it in the infoview. -/
@[command_elab animateCmd]
unsafe def elabAnimateCmd : CommandElab := fun stx => do
  let fpsOpt := stx[1]
  let stepsStx : TSyntax `term := ⟨stx[2]⟩
  let renderStx : TSyntax `term := ⟨stx[3]⟩
  liftTermElabM do
    let compiledAnimTy := Lean.mkConst ``CompiledAnimation
    let fps : Nat := if fpsOpt.isNone then 60
      else fpsOpt[0][2].isNatLit?.getD 60
    let fpsLit := Syntax.mkNumLit (toString fps)
    let callStx ← `(compileAnimation $stepsStx $renderStx (fps := $fpsLit))
    let e ← Term.elabTerm callStx (some compiledAnimTy)
    Term.synthesizeSyntheticMVarsNoPostponing
    let e ← instantiateMVars e
    let ca ← evalExpr CompiledAnimation compiledAnimTy e (safety := .unsafe)
    let animJson := compiledAnimationToLeanJson ca
    let props : Json := .mkObj [
      ("animData", animJson)]
    savePanelWidgetInfo animateWidget.javascriptHash.val (pure props) stx
