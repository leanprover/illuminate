/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/
module
public meta import Lean.Widget.UserWidget
public import Illuminate.Style.Color
import Lean.DocString.Syntax
public section

namespace Illuminate

open Lean Widget in
/-- Widget module that shows a color swatch and picker for {lit}`rgb!` literals. -/
@[widget_module]
meta def colorWidget : Lean.Widget.Module where
  javascript := include_str "../../../player_js/color_widget.js"

open Lean Widget Elab Term in
/-- Elaborates {lit}`rgb!` literals with an attached color picker widget in the infoview. -/
@[term_elab rgbLit]
meta def elabRgbLitWidget : TermElab := fun stx _expectedType? => do
  match stx with
  | `(rgb! $s:str) =>
    let str := s.getString
    let (r, g, b, a?) ← match parseRgbHex str with
      | .ok v => pure v
      | .error msg => throwErrorAt s msg
    let result ← liftMacroM <| rgbHexToSyntax r g b a?
    let expr ← elabTerm result none
    -- Compute source ranges for editing and cursor visibility
    if let some strRange := s.raw.getRange? then
      let fileMap ← getFileMap
      let startPos := fileMap.toPosition strRange.start
      let endPos := fileMap.toPosition strRange.stop
      let ctx ← readThe Core.Context
      let uri := "file://" ++ ctx.fileName
      let alphaFloat : Float := match a? with
        | some a => Float.round (a.toFloat / 255.0 * 10000) / 10000
        | none => 1.0
      -- Full expression range for cursor-based visibility filtering
      let stxRangeJson ← match stx.getRange? with
        | some r =>
          let s := fileMap.toPosition r.start
          let e := fileMap.toPosition r.stop
          pure <| Json.mkObj [
            ("start", .mkObj [("line", toJson (s.line - 1)), ("character", toJson s.column)]),
            ("end", .mkObj [("line", toJson (e.line - 1)), ("character", toJson e.column)])]
        | none => pure Json.null
      let props : Json := .mkObj [
        ("r", toJson r.toNat),
        ("g", toJson g.toNat),
        ("b", toJson b.toNat),
        ("a", toJson alphaFloat),
        ("hex", .str str),
        ("range", .mkObj [
          ("start", .mkObj [("line", toJson (startPos.line - 1)), ("character", toJson startPos.column)]),
          ("end", .mkObj [("line", toJson (endPos.line - 1)), ("character", toJson endPos.column)])
        ]),
        ("stxRange", stxRangeJson),
        ("uri", .str uri)]
      savePanelWidgetInfo colorWidget.javascriptHash (pure props) stx
    return expr
  | _ => throwUnsupportedSyntax
