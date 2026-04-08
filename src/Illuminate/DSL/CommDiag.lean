/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/
module
public import Illuminate.Diagram.Placement
import Illuminate.Diagram.Arrow
import Lean.DocString.Syntax
public section


namespace Illuminate

/-!
# Types
-/

/-- Which side of an arrow to place a label on. -/
inductive LabelSide where
  /-- Places the label above the arrow. -/
  | above
  /-- Places the label below the arrow. -/
  | below
  /-- Places the label to the left of the arrow. -/
  | left
  /-- Places the label to the right of the arrow. -/
  | right
deriving Repr, BEq, Inhabited

/-- Internal representation of a node in the commutative diagram. -/
structure CDNode where
  /-- Unique name for anchor lookup. -/
  name : Lean.Name
  /-- Text displayed as the node label. -/
  label : String
  /-- Optional numeric tag for annotation. -/
  tag : Option Nat := none
deriving Repr, BEq

/-- A reference to a node in the commutative diagram DSL. -/
structure NodeRef where
  /-- The name of the referenced node. -/
  name : Lean.Name
deriving Repr, BEq, Inhabited

/-- Options for customising an arrow between nodes. -/
structure MorphismOpts where
  /-- Optional label for the arrow. -/
  label : Option String := none
  /-- Which side to place the label on. -/
  side : LabelSide := .above
  /-- Bend factor for curved arrows (0 = straight). -/
  bend : Float := 0
  /-- Optional numeric tag for annotation. -/
  tag : Option Nat := none
deriving Repr, BEq

/-- A morphism (arrow) between two named nodes. -/
structure Morphism where
  /-- Source node name. -/
  src : Lean.Name
  /-- Target node name. -/
  tgt : Lean.Name
  /-- Optional label for the arrow. -/
  label : Option String := none
  /-- Which side to place the label on. -/
  side : LabelSide := .above
  /-- Bend factor for curved arrows (0 = straight). -/
  bend : Float := 0
  /-- Optional numeric tag for annotation. -/
  tag : Option Nat := none
deriving Repr, BEq

/-!
# DSL state and monad
-/

/-- Accumulated state of the commutative diagram builder. -/
structure CommDiagState where
  /-- Counter for generating unique node names. -/
  nextId : Nat
  /-- Registered nodes. -/
  nodes : Array CDNode
  /-- Registered morphisms (arrows). -/
  morphisms : Array Morphism
  /-- Optional grid layout specification. -/
  gridSpec : Option (Array (Array (Option NodeRef)))
deriving Repr

/-- The commutative diagram builder monad. -/
abbrev CommDiagM := StateM CommDiagState

namespace CommDiagM

/-- Registers a node with a text label. -/
def node (label : String) (tag : Option Nat := none) : CommDiagM NodeRef := do
  let st ← get
  let name := Lean.Name.mkSimple s!"node_{st.nextId}"
  let cdNode : CDNode := { name := name, label := label, tag := tag }
  set { st with nextId := st.nextId + 1, nodes := st.nodes.push cdNode }
  return { name := name }

/-- Specifies the grid layout of nodes. -/
def grid (rows : Array (Array (Option NodeRef))) : CommDiagM Unit := do
  modify fun st => { st with gridSpec := some rows }

/-- Adds a plain arrow between two nodes. -/
def arrow (src tgt : NodeRef) : CommDiagM Unit := do
  modify fun st => { st with morphisms := st.morphisms.push {
    src := src.name, tgt := tgt.name
  }}

/-- Adds an arrow with options between two nodes. -/
def arrowWith (src tgt : NodeRef) (opts : MorphismOpts) : CommDiagM Unit := do
  modify fun st => { st with morphisms := st.morphisms.push {
    src := src.name, tgt := tgt.name,
    label := opts.label,
    side := opts.side, bend := opts.bend, tag := opts.tag
  }}

end CommDiagM

/-!
# Compilation: state → Diagram β
-/

namespace CommDiag

variable {β : Type} [Backend β]

/-- Default cell spacing for grid layout. -/
def cellSpacing : Float := 60

/-- Default node padding. -/
def nodePadding : Float := 8

/-- Arrow stroke style. -/
def arrowStroke : Stroke := .defaultArrow

/-- Default arrowhead configuration for commutative diagram arrows. -/
private def defaultArrowhead : Arrowhead := {}

/-- Shortens a line segment by {lit}`amount` at each end. -/
private def shortenSegment (a b : Vec2) (shortenSrc shortenTgt : Float) : Vec2 × Vec2 :=
  let dir := Vec2.sub b a
  let len := dir.length
  if len < 0.001 then (a, b)
  else
    let n := dir.normalize
    let a' := Vec2.add a (shortenSrc • n)
    let b' := Vec2.sub b (shortenTgt • n)
    (a', b')

/--
Builds the node layer: lays out nodes in a grid using the pict-style algebra.
Returns the node layer diagram.
-/
private def buildNodeLayer (st : CommDiagState) : Diagram β :=
  match st.gridSpec with
  | none =>
    -- No grid: just hcat all nodes
    let nodeDiags := st.nodes.toList.map fun cdNode =>
      let label := Diagram.text cdNode.label
      let boxed := Diagram.pad nodePadding label
      let named := Diagram.named cdNode.name boxed
      match cdNode.tag with
      | some t => Diagram.tag t named
      | none => named
    Diagram.hcat nodeDiags
  | some rows =>
    -- Build the grid with uniform cell sizing
    let gridRows := rows.map fun row =>
      row.map fun cell =>
        match cell with
        | none => none
        | some ref =>
          match st.nodes.find? (fun n => n.name == ref.name) with
          | none => none
          | some cdNode =>
            let label := Diagram.text cdNode.label
            let boxed := Diagram.pad nodePadding label
            -- Give each cell a uniform size envelope for grid alignment
            let sized := Diagram.withEnvelope (Envelope.ofRect (cellSpacing / 2) (cellSpacing / 2)) boxed
            let named := Diagram.named cdNode.name sized
            some (match cdNode.tag with
              | some t => Diagram.tag t named
              | none => named)
    Diagram.grid gridRows

/--
Builds an arrow diagram for a single morphism, resolving source and target
positions from the given base diagram.
-/
private def buildArrow (base : Diagram β) (morph : Morphism) : Diagram β :=
  let srcPos := (base.find morph.src).origin.toVec2
  let tgtPos := (base.find morph.tgt).origin.toVec2
  let dir := Vec2.sub tgtPos srcPos
  let dirNorm := dir.normalize
  let srcShrink := nodePadding + 2
  let tgtShrink := nodePadding + 2
  let (a, b) := shortenSegment srcPos tgtPos srcShrink tgtShrink
  if morph.bend == 0 then
    let shaft := Diagram.fromStroke (PathData.line a b) arrowStroke
    let (head, _) := ArrowDraw.drawArrowhead defaultArrowhead b (Vec2.sub b a) arrowStroke
    let arrow := Diagram.atop head shaft
    let arrow := match morph.label with
      | some labelExpr =>
        let mid : Vec2 := ⟨(a.x + b.x) / 2, (a.y + b.y) / 2⟩
        let offset := labelOffset morph.side dirNorm
        let labelPos := Vec2.add mid offset
        let labelDiag := Diagram.transform
          (Matrix.translate labelPos.x labelPos.y)
          (.text labelExpr { fontSize := (12 : Float) })
        Diagram.atop labelDiag arrow
      | none => arrow
    match morph.tag with
    | some t => Diagram.tag t arrow
    | none => arrow
  else
    let mid : Vec2 := ⟨(a.x + b.x) / 2, (a.y + b.y) / 2⟩
    let perp : Vec2 := ⟨-dirNorm.y, dirNorm.x⟩
    let bendOffset := morph.bend * dir.length * 0.3
    let ctrl := Vec2.add mid (bendOffset • perp)
    let c1 := a + (2 / 3 : Float) • (ctrl - a)
    let c2 := b + (2 / 3 : Float) • (ctrl - b)
    let shaft := Diagram.fromStroke
      (PathData.empty |>.moveTo a |>.curveTo c1 c2 b)
      arrowStroke
    let headDir := Vec2.sub b c2
    let (head, _) := ArrowDraw.drawArrowhead defaultArrowhead b headDir arrowStroke
    let arrow := Diagram.atop head shaft
    match morph.tag with
    | some t => Diagram.tag t arrow
    | none => arrow
where
  labelOffset (side : LabelSide) (dir : Vec2) : Vec2 :=
    let perp : Vec2 := ⟨-dir.y, dir.x⟩
    match side with
    | .above => (10.0 : Float) • perp
    | .below => (-10.0 : Float) • perp
    | .left => ⟨-12, 0⟩
    | .right => ⟨12, 0⟩

/--
Compiles a commutative diagram from the DSL state to a {lean}`Diagram β`.
Builds a node layer and resolves arrow positions eagerly.
-/
def compile (m : CommDiagM Unit) : Diagram β :=
  let initState : CommDiagState := {
    nextId := 0, nodes := #[], morphisms := #[], gridSpec := none
  }
  let (_, st) := StateT.run m initState
  let nodeLayer := buildNodeLayer st
  let arrowLayer := st.morphisms.foldl (init := Diagram.empty) fun acc morph =>
    Diagram.atop (buildArrow nodeLayer morph) acc
  Diagram.atop arrowLayer nodeLayer

end CommDiag

/-- Builds a commutative diagram from the DSL. -/
def commDiag {β : Type} [Backend β] (m : CommDiagM Unit) : Diagram β :=
  CommDiag.compile m
