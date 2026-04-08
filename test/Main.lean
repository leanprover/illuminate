/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/
module
import Tests.Geometry
import Tests.Style
import Tests.Diagram
import Tests.Render
import Tests.Layout
import Tests.DSL
import Tests.Trace
import Tests.Visual
import Tests.HitTest
import Tests.Animation
import Tests.Morph
public section

def main : IO Unit := do
  let tests := geometryTests ++ styleTests ++ diagramTests ++ renderTests
    ++ layoutTests ++ dslTests ++ traceTests ++ visualTests ++ hitTestTests
    ++ animationTests ++ morphTests
  let mut passed := 0
  let mut failed := 0
  for (name, test) in tests do
    try
      test
      IO.println s!"  ✓ {name}"
      passed := passed + 1
    catch e =>
      IO.eprintln s!"  ✗ {name}: {e}"
      failed := failed + 1
  IO.println s!"\n{passed} passed, {failed} failed"
  if failed > 0 then
    throw <| IO.userError "tests failed"
