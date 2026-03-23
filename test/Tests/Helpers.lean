/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/

import Illuminate

open Illuminate

/-- Checks that a {name}`Float` is approximately equal to an expected value. -/
def assertApproxEq (actual expected : Float) (label : String) (tol : Float := 1e-9) : IO Unit := do
  unless (actual - expected).abs < tol do
    throw <| IO.userError s!"{label}: expected {expected}, got {actual}"

/-- Checks that two {name}`Vec2` values are approximately equal. -/
def assertVec2Eq (actual expected : Vec2) (label : String) (tol : Float := 1e-9) : IO Unit := do
  assertApproxEq actual.x expected.x s!"{label}.x" tol
  assertApproxEq actual.y expected.y s!"{label}.y" tol

def assertTrue (b : Bool) (label : String) : IO Unit := do
  unless b do throw <| IO.userError s!"{label}: expected true"

def strContains (haystack needle : String) : Bool :=
  let hs := haystack.toList
  let ns := needle.toList
  if ns.isEmpty then true
  else hs.length >= ns.length &&
    (List.range (hs.length - ns.length + 1)).any fun i =>
      (List.range ns.length).all fun j =>
        hs.getD (i + j) ' ' == ns.getD j ' '

def assertContains (haystack needle label : String) : IO Unit := do
  unless strContains haystack needle do
    throw <| IO.userError s!"{label}: expected to find \"{needle}\" in output"

def assertCellophane (d : Diagram Empty) (expected : Float) (label : String)
    (tol : Float := 1e-9) : IO Unit :=
  match d with
  | .cellophane α _ => assertApproxEq α expected label tol
  | _ => throw <| IO.userError s!"{label}: expected `cellophane`"

def assertCrossFade (d : Diagram Empty) (expectedA expectedB : Float) (label : String)
    (tol : Float := 1e-9) : IO Unit :=
  match d with
  | .compose (.cellophane α1 _) (.cellophane α2 _) => do
    assertApproxEq α1 expectedA s!"{label} a" tol
    assertApproxEq α2 expectedB s!"{label} b" tol
  | _ => throw <| IO.userError s!"{label}: expected `compose` of `cellophane`s"

def assertTransform (d : Diagram Empty) (check : Matrix → IO Unit) (label : String) : IO Unit :=
  match d with
  | .transform m _ => check m
  | _ => throw <| IO.userError s!"{label}: expected `transform`"

/-- Renders a diagram to SVG, writes it to a file, and runs basic assertions. -/
def testVisualWrite (filename : String) (diagram : Illuminate.Diagram SVG)
    (padding : Float := 15) (checks : List (String × String) := []) : IO Unit := do
  let svg := diagram.renderDiagram (padding := padding)
  IO.FS.writeFile filename svg
  let contents ← IO.FS.readFile filename
  assertContains contents "<svg" "written file has svg"
  for (needle, label) in checks do
    assertContains contents needle label
  IO.println s!"  → wrote {filename} ({svg.length} bytes)"

/--
Checks that an envelope is convex by computing the boundary polygon via
tangent-line intersections (the same method used by {name}`showEnvelope`) and
verifying that all consecutive turns have the same winding direction.
-/
def assertEnvelopeConvex (env : Envelope) (label : String) (n : Nat := 64) : IO Unit := do
  match env with
  | .empty => pure ()
  | .nonempty f =>
    let step := 2 * Illuminate.pi / n.toFloat
    -- Sample directions and extents
    let dirs := (List.range n).map fun i =>
      let θ := i.toFloat * step
      let dir : Vec2 := ⟨Float.cos θ, Float.sin θ⟩
      (dir, f dir)
    -- Compute boundary vertices as tangent-line intersections
    let vertices := (List.range n).map fun i =>
      let (d1, e1) := dirs[i]!
      let (d2, e2) := dirs[(i + 1) % n]!
      let det := d1.x * d2.y - d1.y * d2.x
      if det.abs < 1e-10 then e1 • d1
      else ⟨(e1 * d2.y - e2 * d1.y) / det, (d1.x * e2 - d2.x * e1) / det⟩
    -- Check that all consecutive cross products have the same sign
    let mut positiveCount := 0
    let mut negativeCount := 0
    for i in List.range n do
      let a := vertices[i]!
      let b := vertices[(i + 1) % n]!
      let c := vertices[(i + 2) % n]!
      let ab : Vec2 := b - a
      let bc : Vec2 := c - b
      let cross := ab.x * bc.y - ab.y * bc.x
      if cross > 1e-6 then positiveCount := positiveCount + 1
      else if cross < -1e-6 then negativeCount := negativeCount + 1
    if positiveCount > 0 && negativeCount > 0 then
      throw <| IO.userError s!"{label}: envelope is not convex ({positiveCount} CCW, {negativeCount} CW turns)"
