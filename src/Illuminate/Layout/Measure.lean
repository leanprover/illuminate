/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/

import Illuminate.Style
import Illuminate.Render.Renderable
import Illuminate.Geometry.Envelope


namespace Illuminate

/--
Type class for measuring text and foreign primitives during layout.
The layout pass is parameterized over an arbitrary monad `m` and foreign
primitive type `β`, allowing different backends to supply their own
measurement strategies (e.g., heuristic, Harfbuzz, browser JS).
-/
class LayoutMeasure (β : Type) (m : Type → Type) where
  /-- Measures a text string with the given style. -/
  measureText : String → TextStyle → m MeasuredBox
  /-- Computes the envelope of a foreign primitive. -/
  measureForeign : β → m Envelope

/--
Default measurement instance for diagrams with no foreign primitives.
Uses per-class character width heuristics for text.
-/
instance {m : Type → Type} [Pure m] : LayoutMeasure Empty m where
  measureText s style :=
    let lines := s.splitOn "\n"
    let w := lines.foldl (fun acc line => Max.max acc (estimateTextWidth style.fontSize line)) 0
    let lineHeight := style.fontSize * 1.2
    let nLines := Max.max 1 lines.length
    let totalH := if nLines == 1 then style.fontSize
                  else lineHeight * nLines.toFloat
    let ascent := totalH / 2
    let descent := totalH / 2
    pure { width := w, ascent := ascent, descent := descent,
           baseline := ascent, mathAxis := ascent }
  measureForeign e := nomatch e
