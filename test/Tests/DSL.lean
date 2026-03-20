/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/

import Tests.Helpers

open Illuminate

-- ══════════════════════════════════════════════════════════════════
-- CommDiag (10)
-- ══════════════════════════════════════════════════════════════════

#diagram commDiag do
  let a ← CommDiagM.node "A"
  let b ← CommDiagM.node "B"
  let c ← CommDiagM.node "C"
  let d ← CommDiagM.node "D"
  CommDiagM.grid #[#[some a, some b], #[some c, some d]]
  CommDiagM.arrowWith a b { label := some "f" }
  CommDiagM.arrowWith a c { label := some "g", side := .left }
  CommDiagM.arrowWith b d { label := some "h", side := .right }
  CommDiagM.arrowWith c d { label := some "k" }

def testCD_nodeCreation : IO Unit := do
  let d : Diagram Empty := commDiag do
    let _ ← CommDiagM.node "A"
    pure ()
  -- Should produce a non-empty diagram
  let env := d.getEnvelope
  assertTrue (env[Vec2.east] > 0) "node has positive extent"

def testCD_twoNodes : IO Unit := do
  let d : Diagram Empty := commDiag do
    let a ← CommDiagM.node "A"
    let b ← CommDiagM.node "B"
    CommDiagM.grid #[#[some a, some b]]
  let env := d.getEnvelope
  assertTrue (env[Vec2.east] > 0) "two nodes have extent"

def testCD_arrow : IO Unit := do
  let d : Diagram Empty := commDiag do
    let a ← CommDiagM.node "A"
    let b ← CommDiagM.node "B"
    CommDiagM.grid #[#[some a, some b]]
    CommDiagM.arrow a b
  let cmds := d.compile
  assertTrue (cmds.length > 0) s!"arrow diagram has cmds: {cmds.length}"

def testCD_square : IO Unit := do
  let d : Diagram Empty := commDiag do
    let a ← CommDiagM.node "A"
    let b ← CommDiagM.node "B"
    let c ← CommDiagM.node "C"
    let dd ← CommDiagM.node "D"
    CommDiagM.grid #[#[some a, some b], #[some c, some dd]]
    CommDiagM.arrow a b
    CommDiagM.arrow a c
    CommDiagM.arrow b dd
    CommDiagM.arrow c dd
  let cmds := d.compile
  -- Should have node paths + arrow paths
  assertTrue (cmds.length > 4) s!"square has many cmds: {cmds.length}"

def testCD_labeledArrow : IO Unit := do
  let d : Diagram Empty := commDiag do
    let a ← CommDiagM.node "A"
    let b ← CommDiagM.node "B"
    CommDiagM.grid #[#[some a, some b]]
    CommDiagM.arrowWith a b { label := some "f" }
  let svg := d.renderDiagram (padding := 10)
  assertContains svg ">f</text>" "labeled arrow has label text"

def testCD_curvedArrow : IO Unit := do
  let d : Diagram Empty := commDiag do
    let a ← CommDiagM.node "X"
    let b ← CommDiagM.node "Y"
    CommDiagM.grid #[#[some a, some b]]
    CommDiagM.arrowWith a b { bend := 1.0 }
  let svg := d.renderDiagram (padding := 10)
  -- Curved arrow should produce cubic bezier
  assertContains svg "C" "curved arrow has bezier"

def testCD_svgOutput : IO Unit := do
  let d : Diagram Empty := commDiag do
    let a ← CommDiagM.node "A"
    let b ← CommDiagM.node "B"
    CommDiagM.grid #[#[some a, some b]]
    CommDiagM.arrow a b
  let svg := d.renderDiagram (padding := 10)
  assertContains svg "<svg" "has svg tag"
  assertContains svg "</svg>" "svg is closed"
  assertContains svg ">A</text>" "has node A text"
  assertContains svg ">B</text>" "has node B text"

def testCD_annotation : IO Unit := do
  let d : Diagram Empty := commDiag do
    let a ← CommDiagM.node "A" (tag := some 1)
    let b ← CommDiagM.node "B" (tag := some 2)
    CommDiagM.grid #[#[some a, some b]]
  let svg := d.renderDiagram (padding := 10)
  assertContains svg "data-anno-id=\"1\"" "has annotation 1"
  assertContains svg "data-anno-id=\"2\"" "has annotation 2"

def testCD_noGrid : IO Unit := do
  let d : Diagram Empty := commDiag do
    let a ← CommDiagM.node "X"
    let b ← CommDiagM.node "Y"
    CommDiagM.arrow a b
  let cmds := d.compile
  assertTrue (cmds.length > 0) "no-grid still produces cmds"

def testCD_writeSquare : IO Unit := do
  let d : Diagram Empty := commDiag do
    let a ← CommDiagM.node "A"
    let b ← CommDiagM.node "B"
    let c ← CommDiagM.node "C"
    let dd ← CommDiagM.node "D"
    CommDiagM.grid #[#[some a, some b], #[some c, some dd]]
    CommDiagM.arrowWith a b { label := some "f" }
    CommDiagM.arrowWith a c { label := some "g", side := .left }
    CommDiagM.arrowWith b dd { label := some "h", side := .right }
    CommDiagM.arrowWith c dd { label := some "k" }
  let svg := d.renderDiagram (padding := 15)
  IO.FS.writeFile "commdiag.svg" svg

def dslTests : List (String × IO Unit) := [
    ("CommDiag/nodeCreation", testCD_nodeCreation),
    ("CommDiag/twoNodes", testCD_twoNodes),
    ("CommDiag/arrow", testCD_arrow),
    ("CommDiag/square", testCD_square),
    ("CommDiag/labeledArrow", testCD_labeledArrow),
    ("CommDiag/curvedArrow", testCD_curvedArrow),
    ("CommDiag/svgOutput", testCD_svgOutput),
    ("CommDiag/annotation", testCD_annotation),
    ("CommDiag/noGrid", testCD_noGrid),
    ("CommDiag/writeSquare", testCD_writeSquare)
]
