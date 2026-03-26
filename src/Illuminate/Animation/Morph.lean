/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/

import Illuminate.Animation.Morph.CubicSeg
import Illuminate.Animation.Morph.Style
import Illuminate.Animation.Morph.Types
import Illuminate.Animation.Morph.Evaluate
import Illuminate.Animation.Morph.Prepare


namespace Illuminate

/--
Prepares a morph plan between two diagrams.

All expensive work (path normalization, segment equalization, alignment,
name matching, gradient stop normalization) happens here. The returned
{name}`Morph` can be evaluated cheaply at any parameter via
{name}`Morph.evaluate`.
-/
def Diagram.morph {β : Type} [Backend β] (a b : Diagram β) : Morph β :=
  { node := prepareMorph a b, source := a, target := b }
