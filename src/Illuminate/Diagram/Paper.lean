/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/

import Illuminate.Diagram.Basic
import Illuminate.Diagram.Placement

namespace Illuminate

variable {β : Type}

namespace Diagram

/--
A sheet of paper with a folded corner in the upper right, centered at the origin.

The `cornerFold` parameter controls the fold size as a fraction (0–1) of the
minimum of width and height; it defaults to 0.25 and is clamped with a warning
if out of range. If `label` is provided, it is centered on the page. If only one
of `width`/`height` is given, the other is computed to match the A4 aspect ratio
(297∶210 ≈ √2∶1). If neither is given, the smallest A4-proportioned rectangle
surrounding the label is used.
-/
def paper (label : Option (Diagram β) := none)
    (width : Option Float := none) (height : Option Float := none)
    (cornerFold : Float := 0.25)
    (fill : Fill := {}) (stroke : Stroke := {})
    (padding : Float := 6)
    (name : Option Lean.Name := none) : Diagram β :=
  let (cf, warn) :=
    if cornerFold < 0 then (0, true)
    else if cornerFold > 1 then (1, true)
    else (cornerFold, false)
  let a4Ratio := 297.0 / 210.0
  let (w, h) := match width, height with
    | some w, some h => (w, h)
    | some w, none => (w, w * a4Ratio)
    | none, some h => (h / a4Ratio, h)
    | none, none =>
      match label with
      | some lbl =>
        let env := lbl.getEnvelope
        let lw := env[Vec2.east] + env[Vec2.west] + 2 * padding
        let lh := env[Vec2.north] + env[Vec2.south] + 2 * padding
        if lh / lw < a4Ratio then (lw, lw * a4Ratio)
        else (lh / a4Ratio, lh)
      | none => (60, 60 * a4Ratio)
  let fold := min w h * cf
  let hw := w / 2
  let hh := h / 2
  let outline := PathData.empty
    |>.moveTo ⟨-hw, -hh⟩
    |>.lineTo ⟨hw, -hh⟩
    |>.lineTo ⟨hw, hh - fold⟩
    |>.lineTo ⟨hw - fold, hh⟩
    |>.lineTo ⟨-hw, hh⟩
    |>.close
  let foldMark := PathData.empty
    |>.moveTo ⟨hw - fold, hh⟩
    |>.lineTo ⟨hw - fold, hh - fold⟩
    |>.lineTo ⟨hw, hh - fold⟩
  let body : Diagram β := fromPath outline fill stroke
  let foldLine : Diagram β := fromStroke foldMark stroke
  let sheet := Diagram.compose body foldLine
  let sheet := match label with
    | some lbl => atop sheet lbl
    | none => sheet
  let result := match name with
    | none => sheet
    | some n =>
      withNameAndAnchors sheet n [
        (`north, ⟨0, hh⟩), (`south, ⟨0, -hh⟩),
        (`east, ⟨hw, 0⟩), (`west, ⟨-hw, 0⟩),
        (`northeast, ⟨hw - fold, hh⟩), (`northwest, ⟨-hw, hh⟩),
        (`southeast, ⟨hw, -hh⟩), (`southwest, ⟨-hw, -hh⟩)
      ]
  if warn then
    .warning s!"paper: cornerFold={cornerFold} is outside 0–1, clamped to {cf}" result
  else result
