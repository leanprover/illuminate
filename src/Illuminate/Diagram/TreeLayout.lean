/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/
module
public import Illuminate.Diagram.Arrow
import Lean.DocString.Syntax
public section

namespace Illuminate

/-- A rose tree with values at each node. -/
inductive Tree (α : Type) where
  /-- A node holding a value and an array of children. -/
  | node : α → Array (Tree α) → Tree α
deriving Repr, Inhabited

namespace Tree

variable {α : Type} {β : Type}

/-- Creates a leaf node (no children). -/
def leaf (a : α) : Tree α := .node a #[]

/-- Creates a binary node with two children. -/
def binary (a : α) (l r : Tree α) : Tree α := .node a #[l, r]

/-- Maps a function over all values in the tree. -/
def map (f : α → β) : Tree α → Tree β
  | .node a cs => .node (f a) (cs.map (map f))

/-- Returns the total number of nodes in the tree. -/
def size : Tree α → Nat
  | .node _ cs => 1 + cs.foldl (fun acc c => acc + c.size) 0

end Tree

/-- Configuration for tree layout. -/
structure TreeConfig (β : Type) [Backend β] where
  /-- Minimum gap between sibling subtrees along the sibling axis. -/
  siblingGap : Float := 20
  /-- Distance between depth levels along the depth axis. -/
  levelGap : Float := 40
  /-- Direction from root to children in radians (0 = left-to-right, 3π/2 = top-down). -/
  orientation : Float := 3 * pi / 2
  /-- Cross-axis alignment fraction (0 = near edge, 0.5 = center, 1 = far edge). -/
  siblingAlign : Float := 0.5
  /-- Callback to draw an edge between a parent and one child. -/
  drawEdge : Option (Lean.Name → Lean.Name → Diagram β → Diagram β) :=
    some fun parent child d => d.connectEdge (parent : Lean.Name) (child : Lean.Name)

/--
A node in a proof tree, combining a diagram with an optional rule label.

The {name (full := ProofNode.diagram)}`diagram` field is displayed at the node's position
(typically a sequent or judgment). The {name (full := ProofNode.rule)}`rule` field, if present,
is placed to the right of the inference line above this node.
-/
structure ProofNode (β : Type) [Backend β] where
  /-- The diagram displayed at this node. -/
  diagram : Diagram β
  /-- Optional rule name displayed to the right of the inference line. -/
  rule : Option (Diagram β) := none

/-- Configuration for proof tree layout. -/
structure ProofTreeConfig (β : Type) [Backend β] where
  /-- Horizontal gap between adjacent premise subtrees. -/
  siblingGap : Float := 20
  /-- Vertical gap between depth levels (premises, line, conclusion). -/
  levelGap : Float := 0
  /-- Gap between the inference line endpoint and the rule label. -/
  ruleLabelGap : Float := 5

/-!
# Buchheim-Junger-Leipert tree layout algorithm

C. Buchheim, M. Jünger, and S. Leipert, "Improving Walker's Algorithm to Run in Linear Time",
*Graph Drawing (GD 2002)*, LNCS 2528, pp. 344–353, Springer, 2002.
DOI: 10.1007/3-540-36151-0\_32

The algorithm computes x-coordinates (along the sibling axis) in two passes:
1. A bottom-up pass assigns preliminary positions and modifier sums.
2. A top-down pass accumulates modifiers into final absolute positions.

Contour tracing with thread pointers ensures O(n) time.
-/

namespace TreeLayout

/-- Internal annotated tree node used during layout computation. -/
private structure LayoutNode where
  /-- Index of this node in the flat node array. -/
  idx : Nat
  /-- Sibling-axis extent in the negative direction from origin. -/
  extentNeg : Float
  /-- Sibling-axis extent in the positive direction from origin. -/
  extentPos : Float
  /-- Depth-axis extent in the negative direction from origin. -/
  depthNeg : Float
  /-- Depth-axis extent in the positive direction from origin. -/
  depthPos : Float
  /-- Children indices. -/
  children : Array Nat
  /-- Preliminary x position (relative to parent). -/
  prelim : Float := 0
  /-- Modifier for subtree shift. -/
  modifier : Float := 0
  /-- Thread pointer for left contour. -/
  threadLeft : Option Nat := none
  /-- Thread pointer for right contour. -/
  threadRight : Option Nat := none
  /-- Ancestor pointer for the apportion step. -/
  ancestor : Nat
  /-- Change accumulator for shifting. -/
  change : Float := 0
  /-- Shift accumulator for shifting. -/
  shift : Float := 0
  /-- Number of this node among its siblings (0-indexed). -/
  number : Nat := 0
  /-- Depth level. -/
  depth : Nat := 0
deriving Inhabited

/-- Mutable state for the layout algorithm. -/
private structure LayoutState where
  /-- Flat array of layout nodes. -/
  nodes : Array LayoutNode
deriving Inhabited

/-- Gets a node from the state. -/
private def LayoutState.get (s : LayoutState) (i : Nat) : LayoutNode :=
  s.nodes[i]!

/-- Sets a node in the state. -/
private def LayoutState.set (s : LayoutState) (i : Nat) (n : LayoutNode) : LayoutState :=
  { s with nodes := s.nodes.set! i n }

/-- Modifies a node in the state. -/
private def LayoutState.modify (s : LayoutState) (i : Nat) (f : LayoutNode → LayoutNode) :
    LayoutState :=
  s.set i (f (s.get i))

variable {β : Type} [Backend β]

/-- Recursively flattens a tree into layout nodes and diagrams using pre-order traversal. -/
private partial def flattenGo (t : Tree (Diagram β)) (sibDir depthDir : Vec2)
    (depth number : Nat) (path : String)
    (nodes : Array LayoutNode) (diagrams : Array (Diagram β))
    (nodeNames : Array Lean.Name) (parentIdx : Option Nat) :
    Array LayoutNode × Array (Diagram β) × Array Lean.Name :=
  let .node diag children := t
  let idx := nodes.size
  let env := diag.getEnvelope
  let node : LayoutNode := {
    idx
    extentNeg := env[-sibDir]
    extentPos := env[sibDir]
    depthNeg := env[-depthDir]
    depthPos := env[depthDir]
    children := #[]
    ancestor := idx
    number
    depth
  }
  let nodes := nodes.push node
  let diagrams := diagrams.push diag
  let nodeNames := nodeNames.push (Lean.Name.mkSimple path)
  let nodes := match parentIdx with
    | some pi =>
      let parent := nodes[pi]!
      nodes.set! pi { parent with children := parent.children.push idx }
    | none => nodes
  (Array.range children.size).foldl (init := (nodes, diagrams, nodeNames))
    fun (ns, ds, names) childNum =>
      match children[childNum]? with
      | none => (ns, ds, names)
      | some child =>
        let childPath := s!"{path}_{childNum}"
        flattenGo child sibDir depthDir (depth + 1) childNum childPath ns ds names (some idx)

/-- Flattens a tree into layout nodes and diagrams. -/
private def flatten (tree : Tree (Diagram β)) (sibDir depthDir : Vec2) :
    Array LayoutNode × Array (Diagram β) × Array Lean.Name :=
  flattenGo tree sibDir depthDir 0 0 "node_0" #[] #[] #[] none

/-- Returns the leftmost child or thread pointer. -/
private def nextLeft (s : LayoutState) (i : Nat) : Option Nat :=
  let n := s.get i
  if n.children.isEmpty then n.threadLeft
  else n.children[0]?

/-- Returns the rightmost child or thread pointer. -/
private def nextRight (s : LayoutState) (i : Nat) : Option Nat :=
  let n := s.get i
  if n.children.isEmpty then n.threadRight
  else n.children.back?

/-- Computes the spacing needed between two adjacent nodes. -/
private def separation (s : LayoutState) (left right : Nat) (gap : Float) : Float :=
  let l := s.get left
  let r := s.get right
  l.extentPos + r.extentNeg + gap

/-- Moves subtrees apart to resolve overlap. -/
private def moveSubtree (s : LayoutState) (wl wr : Nat) (shiftAmt : Float) : LayoutState :=
  let nl := s.get wl
  let nr := s.get wr
  let subtrees := (nr.number - nl.number).toFloat
  if subtrees > 0 then
    let s := s.modify wr fun n => { n with
      change := n.change - shiftAmt / subtrees
      shift := n.shift + shiftAmt
      prelim := n.prelim + shiftAmt
      modifier := n.modifier + shiftAmt
    }
    s.modify wl fun n => { n with change := n.change + shiftAmt / subtrees }
  else s

/-- Executes accumulated shifts for children of a node. -/
private def executeShifts (s : LayoutState) (parent : Nat) : LayoutState := Id.run do
  let p := s.get parent
  let mut shiftAcc := 0.0
  let mut changeAcc := 0.0
  let mut st := s
  for i in [:p.children.size] do
    let j := p.children.size - 1 - i
    let ci := p.children[j]!
    let c := st.get ci
    let prelim := c.prelim + shiftAcc
    let modifier := c.modifier + shiftAcc
    changeAcc := changeAcc + c.change
    shiftAcc := shiftAcc + c.shift + changeAcc
    st := st.modify ci fun _ => { c with prelim, modifier }
  return st

/-- Finds the correct ancestor for the apportion step. -/
private def findAncestor (s : LayoutState) (vil parent defaultAncestor : Nat) : Nat :=
  let anc := (s.get vil).ancestor
  let p := s.get parent
  if p.children.any (· == anc) then anc else defaultAncestor

/-- The apportion step: threads contours together and shifts subtrees. -/
private def apportion (s : LayoutState) (v parent defaultAncestor : Nat) (gap : Float) :
    LayoutState × Nat := Id.run do
  let p := s.get parent
  let vn := s.get v
  let vNumber := vn.number
  if vNumber == 0 then return (s, defaultAncestor)
  let wIdx := p.children[vNumber - 1]!
  let mut vip := v
  let mut vop := v
  let mut vim := wIdx
  let mut vom := p.children[0]!
  let mut sipR := (s.get vip).modifier
  let mut sopR := (s.get vop).modifier
  let mut simL := (s.get vim).modifier
  let mut somL := (s.get vom).modifier
  let mut st := s
  let mut da := defaultAncestor
  let mut fuel := st.nodes.size + 1
  while fuel > 0 do
    fuel := fuel - 1
    let nrVim := nextRight st vim
    let nlVip := nextLeft st vip
    match nrVim, nlVip with
    | some vim', some vip' =>
      vim := vim'
      vip := vip'
      vom := match nextLeft st vom with | some x => x | none => vom
      vop := match nextRight st vop with | some x => x | none => vop
      st := st.modify vop fun n => { n with ancestor := v }
      let sep := (st.get vim).prelim + simL -
                 (st.get vip).prelim - sipR +
                 separation st vim vip gap
      if sep > 0 then
        let anc := findAncestor st vim parent da
        st := moveSubtree st anc v sep
        sipR := sipR + sep
        sopR := sopR + sep
      simL := simL + (st.get vim).modifier
      sipR := sipR + (st.get vip).modifier
      somL := somL + (st.get vom).modifier
      sopR := sopR + (st.get vop).modifier
    | _, _ => break
  let nrVim := nextRight st vim
  let nrVop := nextRight st vop
  if nrVim.isSome && nrVop.isNone then
    st := st.modify vop fun n => { n with threadRight := nrVim }
    st := st.modify vop fun n => { n with modifier := n.modifier + simL - sopR }
  let nlVip := nextLeft st vip
  let nlVom := nextLeft st vom
  if nlVip.isSome && nlVom.isNone then
    st := st.modify vom fun n => { n with threadLeft := nlVip }
    st := st.modify vom fun n => { n with modifier := n.modifier + sipR - somL }
    da := v
  return (st, da)

/-- First pass (post-order): computes preliminary x-coordinates. -/
private partial def firstWalk (s : LayoutState) (v : Nat) (leftSibling : Option Nat)
    (gap : Float) : LayoutState :=
  let node := s.get v
  if node.children.isEmpty then
    -- Leaf: position relative to left sibling
    match leftSibling with
    | some w => s.modify v fun n => { n with prelim := (s.get w).prelim + separation s w v gap }
    | none => s
  else
    let firstChild := node.children[0]!
    -- Recurse on each child, interleaving with apportion
    let (s, _) := (Array.range node.children.size).foldl (init := (s, firstChild))
      fun (acc, da) i =>
        let ci := node.children[i]!
        let leftSib := if i > 0 then some node.children[i - 1]! else none
        let acc := firstWalk acc ci leftSib gap
        apportion acc ci v da gap
    let s := executeShifts s v
    let lastChild := node.children.back!
    let midpoint := ((s.get firstChild).prelim + (s.get lastChild).prelim) / 2
    match leftSibling with
    | some w =>
      let prelim := (s.get w).prelim + separation s w v gap
      s.modify v fun n => { n with prelim, modifier := prelim - midpoint }
    | none =>
      s.modify v fun n => { n with prelim := midpoint }

/-- Second pass (pre-order): computes absolute x-coordinates. -/
private def secondWalk (s : LayoutState) (root : Nat) : Array (Float × Nat) := Id.run do
  let mut result : Array (Float × Nat) :=
    (Array.range s.nodes.size).map fun _ => (0.0, 0)
  let mut stack : List (Nat × Float) := [(root, 0)]
  for _ in [:s.nodes.size * 2] do
    match stack with
    | [] => break
    | (v, modSum) :: rest =>
      stack := rest
      let node := s.get v
      let x := node.prelim + modSum
      result := result.set! v (x, node.depth)
      let newMod := modSum + node.modifier
      for ci in node.children.toList.reverse do
        stack := (ci, newMod) :: stack
  return result

/-- Draws edges by traversing the tree structure. -/
private def drawEdges (tree : Tree (Diagram β)) (nodeNames : Array Lean.Name)
    (state : LayoutState)
    (drawEdge : Option (Lean.Name → Lean.Name → Diagram β → Diagram β))
    (composed : Diagram β) : Diagram β := Id.run do
  let mut result := composed
  let mut stack : List (Nat × Tree (Diagram β)) := [(0, tree)]
  for _ in [:state.nodes.size * 2] do
    match stack with
    | [] => break
    | (idx, .node _ children) :: rest =>
      stack := rest
      let parentName := nodeNames[idx]!
      let childIndices := state.nodes[idx]!.children
      if let some de := drawEdge then
        for ci in childIndices do
          result := de parentName nodeNames[ci]! result
      let mut childEntries : List (Nat × Tree (Diagram β)) := []
      for i in [:children.size] do
        if let (some ci, some child) := (childIndices[i]?, children[i]?) then
          childEntries := (ci, child) :: childEntries
      stack := childEntries.reverse ++ rest
  return result

/--
Runs the layout algorithm on a flat array of diagrams and returns the layout state and positions.
-/
private def runLayout (nodes : Array LayoutNode) (siblingGap : Float) :
    LayoutState × Array (Float × Nat) :=
  if nodes.isEmpty then ({ nodes := #[] }, #[])
  else
    let state : LayoutState := { nodes }
    let state := firstWalk state 0 none siblingGap
    let positions := secondWalk state 0
    (state, positions)

/-- Composes positioned node diagrams into a single diagram. -/
private def composeNodes (state : LayoutState) (positions : Array (Float × Nat))
    (diagrams : Array (Diagram β)) (nodeNames : Array Lean.Name)
    (depthDir sibDir : Vec2) (levelGap siblingAlign : Float) : Diagram β :=
  (Array.range state.nodes.size).foldl (init := (Diagram.empty : Diagram β))
    fun acc i =>
      let (sibOffset, depth) := positions[i]!
      let node := state.nodes[i]!
      match diagrams[i]? with
      | none => acc
      | some diag =>
        let depthOffset := depth.toFloat * levelGap
        let depthExtent := node.depthNeg + node.depthPos
        let alignOffset := (siblingAlign - 0.5) * depthExtent
        let pos := depthOffset • depthDir + sibOffset • sibDir + alignOffset • depthDir
        let named := Diagram.namedWithAnchors nodeNames[i]! diag
        let placed := Diagram.translate pos.x pos.y named
        .compose acc placed

end TreeLayout

/-!
# Public API
-/

variable {β : Type} [Backend β]

/--
Lays out a rose tree of diagrams using the Buchheim-Junger-Leipert algorithm.

Nodes are automatically named by path ({lit}`node_0` for the root, {lit}`node_0_0` for the first
child, {lit}`node_0_1_5` for the sixth child of the second child, etc.) with cardinal anchors.

When {name}`name` is {name}`none`, the result is wrapped in a {name}`Diagram.scope` so that
internal node names are invisible from outside and multiple trees can be composed without
conflicts. When a name is provided, the result is wrapped in {name}`Diagram.named` instead,
making the node names accessible under that namespace.
-/
def treeLayout (tree : Tree (Diagram β)) (name : Option Lean.Name := none) (config : TreeConfig β := {
      drawEdge := some fun parent child d => d.connectEdge (parent : Lean.Name) (child : Lean.Name)
    }) : Diagram β := Id.run do
  let depthDir := Vec2.dir config.orientation
  let sibDir : Vec2 := ⟨-depthDir.y, depthDir.x⟩
  let (nodes, diagrams, nodeNames) := TreeLayout.flatten tree sibDir depthDir
  if nodes.isEmpty then return Diagram.empty
  let (state, positions) := TreeLayout.runLayout nodes config.siblingGap
  let composed := TreeLayout.composeNodes state positions diagrams nodeNames
    depthDir sibDir config.levelGap config.siblingAlign
  let withEdges := TreeLayout.drawEdges tree nodeNames state config.drawEdge composed
  match name with
  | none => .scope withEdges
  | some n => .named n withEdges

/--
Lays out a proof tree with inference lines and optional rule labels.

Each node is a {name}`ProofNode` containing a diagram (typically a sequent or judgment) and an
optional rule label placed to the right of the inference line. The tree grows from premises
(leaves) toward the conclusion (root).

Premises are arranged horizontally with {name (full := ProofTreeConfig.siblingGap)}`siblingGap`
spacing. Inference lines span the wider of the premises or conclusion, with the rule label to
the right. Depth levels are separated by {name (full := ProofTreeConfig.levelGap)}`levelGap`.
-/
partial def proofTree (tree : Tree (ProofNode β))
    (config : ProofTreeConfig β := {}) : Diagram β :=
  go tree
where
  /-- Builds a horizontal inference line. -/
  ruleLine (width : Float) : Diagram β :=
    let half := width / 2
    Diagram.line ⟨-half, 0⟩ ⟨half, 0⟩ (stroke := .ofWidth 1)
  /-- Recursively lays out a proof tree node. -/
  go (t : Tree (ProofNode β)) : Diagram β :=
    let .node pn children := t
    let conclusion := pn.diagram
    if children.isEmpty then conclusion
    else
      -- Recursively lay out each child subtree
      let childDiags := children.map go |>.toList
      -- Place child subtrees side by side
      let premises := Diagram.hsep config.siblingGap childDiags
      let premisesEnv := premises.getEnvelope
      let conclusionEnv := conclusion.getEnvelope
      let premisesWidth := premisesEnv[Vec2.east] + premisesEnv[Vec2.west]
      let conclusionWidth := conclusionEnv[Vec2.east] + conclusionEnv[Vec2.west]
      let lineWidth := max premisesWidth conclusionWidth + 6
      let line := ruleLine lineWidth
      let namedLine := Diagram.namedWithAnchors `_rule line
      -- Stack: premises on top, then rule line, then conclusion
      let stacked := Diagram.vsep config.levelGap [premises, namedLine, conclusion]
      -- Place rule label to the right of the line
      match pn.rule with
      | none => Diagram.scopeNames stacked
      | some label =>
        let linePos := (stacked.find `_rule).origin
        let lineRight := lineWidth / 2
        let labelX := linePos.x + lineRight + config.ruleLabelGap +
          label.getEnvelope[Vec2.west]
        let labelY := linePos.y
        let placed := Diagram.translate labelX labelY label
        Diagram.scopeNames (Diagram.atop placed stacked)
