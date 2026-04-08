/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/
module
public import Illuminate.Animation.Morph.Types
public import Illuminate.Animation.Morph.Evaluate
public import Illuminate.Animation.Morph.Prepare
import Lean.DocString.Syntax
public section


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
