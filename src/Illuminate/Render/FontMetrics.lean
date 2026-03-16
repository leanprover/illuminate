/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/

import Illuminate.Style
import Illuminate.Render.Renderable


namespace Illuminate

-- ═══════════════════════════════════════════════════════════════
-- FontMetrics interface
-- ═══════════════════════════════════════════════════════════════

/--
Font metrics interface for measuring text content.
Two pure Lean implementations are provided for offline SVG generation.
HarfBuzz-based implementations are used for canvas/PDF backends.
-/
structure FontMetrics where
  /-- Measures a text string and returns its bounding box. -/
  measureText : String → TextStyle → MeasuredBox

namespace FontMetrics

/--
Monospace metrics: every character is `fontSize × 0.6` wide.
Ascent is `fontSize × 0.8`, descent is `fontSize × 0.2`.
-/
def monospace : FontMetrics where
  measureText := fun s style =>
    let charWidth := style.fontSize * 0.6
    let w := s.length.toFloat * charWidth
    let ascent := style.fontSize * 0.8
    let descent := style.fontSize * 0.2
    { width := w
      ascent := ascent
      descent := descent
      baseline := ascent
      mathAxis := ascent }

/--
Fixed-table metrics: uses per-class advance widths for a proportional
sans-serif approximation. Uppercase letters are wider than lowercase,
digits are uniform width, punctuation is narrow.
-/
def fixedTable : FontMetrics where
  measureText := fun s style =>
    let w := estimateTextWidth style.fontSize s
    let ascent := style.fontSize * 0.75
    let descent := style.fontSize * 0.25
    { width := w
      ascent := ascent
      descent := descent
      baseline := ascent
      mathAxis := ascent }
