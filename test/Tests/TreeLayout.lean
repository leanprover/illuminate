/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/
module
public import Tests.Helpers
public section

set_option linter.missingDocs false

open Illuminate

/-!
# Tree layout unit tests
-/

def testTreeLayout_singleNode : IO Unit := do
  let tree : Tree (Diagram SVG) := .leaf (.circle 10 (fill := Color.blue))
  let d := treeLayout tree
  let env := d.getEnvelope
  assertTrue (env[Vec2.east] > 0) "single node has positive east extent"
  assertTrue (env[Vec2.north] > 0) "single node has positive north extent"

def testTreeLayout_binarySymmetric : IO Unit := do
  let leaf := Tree.leaf (Diagram.circle 10 (fill := Color.blue) : Diagram SVG)
  let tree := Tree.binary (.circle 10 (fill := Color.red)) leaf leaf
  let d := treeLayout tree
  let env := d.getEnvelope
  let w := env[Vec2.east] + env[Vec2.west]
  let h := env[Vec2.north] + env[Vec2.south]
  assertTrue (w > 30) s!"binary tree width should be > 30, got {w}"
  assertTrue (h > 40) s!"binary tree height should be > 40, got {h}"

def testTreeLayout_unbalancedNary : IO Unit := do
  let leaf := Tree.leaf (Diagram.circle 5 (fill := Color.green) : Diagram SVG)
  let wide := Tree.node (.rect 30 10 (fill := Color.blue)) #[leaf, leaf, leaf, leaf]
  let tree := Tree.node (.circle 10 (fill := Color.red)) #[wide, leaf]
  let d := treeLayout tree
  let env := d.getEnvelope
  assertTrue (env[Vec2.east] > 0) "unbalanced tree has positive east extent"

def testTreeLayout_noEdges : IO Unit := do
  let leaf := Tree.leaf (Diagram.circle 10 (fill := Color.blue) : Diagram SVG)
  let tree := Tree.binary (.circle 10 (fill := Color.red)) leaf leaf
  let config : TreeConfig SVG := { drawEdge := none }
  let d := treeLayout tree (config := config)
  -- With no edges, should still have the nodes laid out
  let env := d.getEnvelope
  assertTrue (env[Vec2.east] > 0) "no-edge tree still has extent"

def testTreeLayout_leftToRight : IO Unit := do
  let leaf := Tree.leaf (Diagram.circle 10 (fill := Color.blue) : Diagram SVG)
  let inner := Tree.binary (.circle 10 (fill := Color.green)) leaf leaf
  let tree := Tree.binary (.circle 10 (fill := Color.red)) inner leaf
  let config : TreeConfig SVG := { orientation := 0, drawEdge := none }
  let d := treeLayout tree (config := config)
  let env := d.getEnvelope
  let w := env[Vec2.east] + env[Vec2.west]
  let h := env[Vec2.north] + env[Vec2.south]
  -- Left-to-right with 3 depth levels: width should be larger than height
  assertTrue (w > h) s!"LTR tree should be wider than tall: w={w}, h={h}"

def testTreeLayout_nodeNames : IO Unit := do
  let tree : Tree (Diagram SVG) := Tree.binary
    (.circle 10 (fill := Color.red))
    (.leaf (.circle 10 (fill := Color.blue)))
    (.leaf (.circle 10 (fill := Color.green)))
  let d : Diagram SVG := treeLayout tree (name := some `test) (config := { drawEdge := none })
  -- Path-based names: root = node_0, left child = node_0_0, right child = node_0_1
  let root := d.find (`test ++ Lean.Name.mkSimple "node_0")
  let left := d.find (`test ++ Lean.Name.mkSimple "node_0_0")
  let right := d.find (`test ++ Lean.Name.mkSimple "node_0_1")
  let rootPos := root.origin
  let leftPos := left.origin
  let rightPos := right.origin
  -- Root should be above children (top-down orientation: root has higher y)
  assertTrue (rootPos.y > leftPos.y)
    s!"root y ({rootPos.y}) should be > left y ({leftPos.y})"
  assertTrue (rootPos.y > rightPos.y)
    s!"root y ({rootPos.y}) should be > right y ({rightPos.y})"
  -- Left child should be to the left of right child
  assertTrue (leftPos.x < rightPos.x)
    s!"left x ({leftPos.x}) should be < right x ({rightPos.x})"

def testTreeLayout_scopeIsolation : IO Unit := do
  let leaf := Tree.leaf (Diagram.circle 10 (fill := Color.blue) : Diagram SVG)
  let tree := Tree.binary (.circle 10 (fill := Color.red)) leaf leaf
  -- Two anonymous trees composed together should not produce duplicate name warnings
  let d1 := treeLayout tree (config := { drawEdge := none })
  let d2 := treeLayout tree (config := { drawEdge := none })
  let combined := Diagram.compose d1 d2
  match validate combined with
  | .ok () => pure ()
  | .error errs => throw <| IO.userError s!"validation failed: {errs.map toString}"

/-!
# Scope unit tests
-/

def testScope_hidesNames : IO Unit := do
  let inner : Diagram SVG := Diagram.namedWithAnchors `secret (.circle 10)
  let sc := Diagram.scopeNames inner
  -- find from outside should fail, producing a "missing anchor" warning
  let result := sc.find `secret
  let warnings := collectWarnings result
  assertTrue (warnings.any fun w => strContains w "missing anchor")
    s!"expected missing anchor warning, got: {warnings}"

def testScope_collectNamesEmpty : IO Unit := do
  let inner : Diagram SVG :=
    Diagram.compose
      (Diagram.namedWithAnchors `a (.circle 10))
      (Diagram.namedWithAnchors `b (.rect 20 10))
  let sc := Diagram.scopeNames inner
  let names := sc.collectNames Matrix.identity .anonymous []
  assertTrue (names.isEmpty)
    s!"expected no names from scoped diagram, got {names.length}"

def testScope_namedTreeExposesNames : IO Unit := do
  let leaf := Tree.leaf (Diagram.circle 10 (fill := Color.blue) : Diagram SVG)
  let tree := Tree.binary (.circle 10 (fill := Color.red)) leaf leaf
  -- Named tree: nodes are visible under the namespace
  let d : Diagram SVG := treeLayout tree (name := some `t) (config := { drawEdge := none })
  let root := d.find (`t ++ Lean.Name.mkSimple "node_0")
  let warnings := collectWarnings root
  assertTrue (warnings.isEmpty)
    s!"named tree root should be findable, got warnings: {warnings}"

def testScope_transparentEnvelope : IO Unit := do
  let inner : Diagram SVG := Diagram.circle 20
  let sc := Diagram.scopeNames inner
  let envInner := inner.getEnvelope
  let envScoped := sc.getEnvelope
  -- Scope should not change the envelope
  assertApproxEq envScoped[Vec2.east] envInner[Vec2.east] "scope preserves east extent"
  assertApproxEq envScoped[Vec2.north] envInner[Vec2.north] "scope preserves north extent"

/-!
# Visual test diagrams
-/

/-- A binary tree with text labels. -/
def binaryTreeDiagram : Diagram SVG :=
  let node (label : String) : Diagram SVG :=
    Diagram.atop
      (.text label { fontSize := 10 })
      (.circle 12 (fill := rgb!"#dcebff") (stroke := { color := Color.black, width := 1 }))
  let tree := Tree.node (node "+")
    #[Tree.node (node "*")
        #[Tree.leaf (node "a"), Tree.leaf (node "b")],
      Tree.node (node "-")
        #[Tree.leaf (node "c"),
          Tree.node (node "/")
            #[Tree.leaf (node "d"), Tree.leaf (node "e")]]]
  treeLayout tree

#diagram binaryTreeDiagram

/-- An unbalanced n-ary tree. -/
def naryTreeDiagram : Diagram SVG :=
  let node (label : String) (color : Color) : Diagram SVG :=
    Diagram.atop
      (.text label { fontSize := 9 })
      (.roundedRect 30 18 4 (fill := .solid { color }) (stroke := { color := Color.black, width := 1 }))
  let leaf (label : String) : Tree (Diagram SVG) := .leaf (node label (rgb!"#e8f4e8"))
  let tree := Tree.node (node "root" (rgb!"#ffdddd"))
    #[Tree.node (node "A" (rgb!"#ddddff"))
        #[leaf "A1", leaf "A2", leaf "A3"],
      leaf "B",
      Tree.node (node "C" (rgb!"#ddddff"))
        #[Tree.node (node "C1" (rgb!"#ffffdd"))
            #[leaf "C1a", leaf "C1b"],
          leaf "C2"]]
  treeLayout tree

#diagram naryTreeDiagram

/-- Left-to-right tree orientation. -/
def ltrTreeDiagram : Diagram SVG :=
  let node (label : String) : Diagram SVG :=
    Diagram.atop
      (.text label { fontSize := 9 })
      (.roundedRect 35 18 4 (fill := rgb!"#dcebff") (stroke := { color := Color.black, width := 1 }))
  let tree := Tree.node (node "main")
    #[Tree.node (node "parse")
        #[.leaf (node "lex"), .leaf (node "AST")],
      .leaf (node "eval"),
      .leaf (node "print")]
  treeLayout tree (config := {
    orientation := 0
    drawEdge := some fun parent child d => d.connectEdge (parent : Lean.Name) (child : Lean.Name)
  })

#diagram ltrTreeDiagram

/-- A sequent calculus proof tree for A ∧ B ⊢ B ∧ A. -/
def proofTreeDiagram : Diagram SVG :=
  let seq (s : String) : Diagram SVG := .text s { fontSize := 11 }
  let rule (s : String) : Diagram SVG := .text s { fontSize := 9 }
  let ax (s : String) : Tree (ProofNode SVG) := .leaf { diagram := seq s }
  -- The proof:
  --        A ∧ B ⊢ A ∧ B, B ∧ A    A ∧ B ⊢ A ∧ B, B ∧ A
  --        ――――――――――――――――――――――    ――――――――――――――――――――――
  --  ∧L    A ∧ B ⊢ B, B ∧ A         A ∧ B ⊢ A, B ∧ A      ∧L
  --        ―――――――――――――――――――――――――――――――――――――――――――――
  --  ∧R                    A ∧ B ⊢ B ∧ A
  let tree := Tree.node { diagram := seq "A ∧ B ⊢ B ∧ A", rule := some (rule "∧R") }
    #[Tree.node { diagram := seq "A ∧ B ⊢ B, B ∧ A", rule := some (rule "∧L") }
        #[ax "A ∧ B ⊢ A ∧ B, B ∧ A"],
      Tree.node { diagram := seq "A ∧ B ⊢ A, B ∧ A", rule := some (rule "∧L") }
        #[ax "A ∧ B ⊢ A ∧ B, B ∧ A"]]
  proofTree tree

#diagram proofTreeDiagram

def testVisual_binaryTree : IO Unit :=
  testVisualWrite "binary-tree.svg" binaryTreeDiagram

def testVisual_naryTree : IO Unit :=
  testVisualWrite "nary-tree.svg" naryTreeDiagram

def testVisual_ltrTree : IO Unit :=
  testVisualWrite "ltr-tree.svg" ltrTreeDiagram

def testVisual_proofTree : IO Unit :=
  testVisualWrite "proof-tree.svg" proofTreeDiagram

/-!
# Proof tree unit tests
-/

def testProofTree_singleNode : IO Unit := do
  let tree : Tree (ProofNode SVG) := .leaf { diagram := .text "A ⊢ A" { fontSize := 11 } }
  let d := proofTree tree
  let env := d.getEnvelope
  assertTrue (env[Vec2.east] > 0) "single proof node has positive east extent"
  assertTrue (env[Vec2.north] > 0) "single proof node has positive north extent"

def testProofTree_noRuleLabels : IO Unit := do
  let node (s : String) : ProofNode SVG := { diagram := .text s { fontSize := 11 } }
  let tree := Tree.node (node "A ⊢ B")
    #[.leaf (node "A, B ⊢ B")]
  let d := proofTree tree
  let env := d.getEnvelope
  -- Should still have extent (line is drawn even without label)
  assertTrue (env[Vec2.east] > 0) "proof tree without labels has positive east extent"
  assertTrue (env[Vec2.south] > 0) "proof tree without labels has positive south extent"

def testProofTree_scopeIsolation : IO Unit := do
  let node (s : String) : ProofNode SVG := { diagram := .text s { fontSize := 11 } }
  let tree := Tree.node (node "A ⊢ B") #[.leaf (node "A, B ⊢ B")]
  let d1 := proofTree tree
  let d2 := proofTree tree
  let combined := Diagram.compose d1 d2
  match validate combined with
  | .ok () => pure ()
  | .error errs => throw <| IO.userError s!"validation failed: {errs.map toString}"

def testTreeLayout_bottomUp : IO Unit := do
  let leaf := Tree.leaf (Diagram.circle 10 (fill := Color.blue) : Diagram SVG)
  let inner := Tree.binary (.circle 10 (fill := Color.green)) leaf leaf
  let tree := Tree.binary (.circle 10 (fill := Color.red)) inner leaf
  let config : TreeConfig SVG := { orientation := pi / 2, drawEdge := none }
  let d := treeLayout tree (config := config)
  let env := d.getEnvelope
  assertTrue (env[Vec2.east] > 0) "bottom-up tree has positive east extent"
  assertTrue (env[Vec2.north] > 0) "bottom-up tree has positive north extent"

def treeLayoutTests : List (String × IO Unit) := [
  ("TreeLayout/singleNode", testTreeLayout_singleNode),
  ("TreeLayout/binarySymmetric", testTreeLayout_binarySymmetric),
  ("TreeLayout/unbalancedNary", testTreeLayout_unbalancedNary),
  ("TreeLayout/noEdges", testTreeLayout_noEdges),
  ("TreeLayout/leftToRight", testTreeLayout_leftToRight),
  ("TreeLayout/nodeNames", testTreeLayout_nodeNames),
  ("TreeLayout/scopeIsolation", testTreeLayout_scopeIsolation),
  ("Scope/hidesNames", testScope_hidesNames),
  ("Scope/collectNamesEmpty", testScope_collectNamesEmpty),
  ("Scope/namedTreeExposesNames", testScope_namedTreeExposesNames),
  ("Scope/transparentEnvelope", testScope_transparentEnvelope),
  ("ProofTree/singleNode", testProofTree_singleNode),
  ("ProofTree/noRuleLabels", testProofTree_noRuleLabels),
  ("ProofTree/scopeIsolation", testProofTree_scopeIsolation),
  ("TreeLayout/bottomUp", testTreeLayout_bottomUp),
  ("Visual/binaryTree", testVisual_binaryTree),
  ("Visual/naryTree", testVisual_naryTree),
  ("Visual/ltrTree", testVisual_ltrTree),
  ("Visual/proofTree", testVisual_proofTree)
]
