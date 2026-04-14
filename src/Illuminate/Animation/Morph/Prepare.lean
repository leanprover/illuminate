/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/
module
public import Illuminate.Diagram.Placement
public import Illuminate.Animation.Morph.Types
import Illuminate.Animation.Morph.Style
public import Std.Data.HashSet.Basic
import Lean.DocString.Syntax
public section


namespace Illuminate

/-!
# Path transform and envelope helpers
-/

/-- Transforms a {name}`PathData` by applying a matrix to all path points via cubic conversion. -/
private def transformPathData (m : Matrix) (pd : PathData) : PathData :=
  let np := pathToCubics pd
  let results := np.subpaths.map fun sub =>
    let tSegs := sub.segments.map fun seg =>
      { p0 := m.apply seg.p0, c1 := m.apply seg.c1,
        c2 := m.apply seg.c2, p3 := m.apply seg.p3 }
    cubicsToPathData tSegs sub.closed
  results.foldl (fun acc r => ⟨acc.commands ++ r.commands⟩) PathData.empty

/--
Approximates an envelope as a closed polygon {name}`PathData` by sampling support-line
intersections at evenly spaced directions.
-/
private def envelopeToPathData {β : Type} [Backend β]
    (d : Diagram β) (samples : Nat := 64) : PathData :=
  match d.getEnvelope with
  | .empty => PathData.rect 0 0
  | .nonempty env =>
    let n := max samples 8
    let step := 2 * pi / n.toFloat
    let dirs := List.range n |>.map fun i =>
      let θ := i.toFloat * step
      let dir : Vec2 := ⟨Float.cos θ, Float.sin θ⟩
      (dir, env dir)
    let vertices := dirs.mapIdx fun i (d1, e1) =>
      let (d2, e2) := match dirs[((i : Nat) + 1) % n]? with
        | some v => v
        | none => (d1, e1)
      let det := d1.x * d2.y - d1.y * d2.x
      if det.abs < 1e-10 then e1 • d1
      else ⟨(e1 * d2.y - e2 * d1.y) / det, (d1.x * e2 - d2.x * e1) / det⟩
    match vertices with
    | [] => PathData.rect 0 0
    | p :: rest =>
      let pd := PathData.empty |>.moveTo p
      (rest.foldl (fun a pt => a.lineTo pt) pd).close

/--
Computes a semantic key for a {name}`PathData`: (signed area, centroid x, centroid y).

Small path changes produce small key changes, so clips with similar geometry
sort adjacently for better morph matching.
-/
private def pathDataKey (pd : PathData) : Float × Float × Float :=
  let np := pathToCubics pd
  let pts := np.subpaths.foldl (fun acc sub =>
    sub.segments.foldl (fun a seg => a.push seg.p3) acc) #[]
  if pts.size == 0 then (0, 0, 0)
  else
    let n := pts.size
    let crosses := pts.mapIdx fun i p =>
      let q := pts[(i + 1) % n]!
      p.x * q.y - q.x * p.y
    let area := crosses.foldl (fun a b => a + b) 0.0 / 2.0
    let cx := pts.foldl (fun a p => a + p.x) 0.0 / n.toFloat
    let cy := pts.foldl (fun a p => a + p.y) 0.0 / n.toFloat
    (area, cx, cy)

/-- Compares two {name}`PathData` values by (area, centroid) for canonical clip ordering. -/
private def pathDataLt (a b : PathData) : Bool :=
  let (a1, a2, a3) := pathDataKey a
  let (b1, b2, b3) := pathDataKey b
  a1 < b1 || (a1 == b1 && (a2 < b2 || (a2 == b2 && a3 < b3)))

/-!
# Skeleton type

A diagram with explicit references where matched named subdiagrams were extracted.
Arrows can still resolve names because the references remain in the tree.
-/

/-- A diagram skeleton with explicit references for extracted named subdiagrams. -/
inductive Skeleton (β : Type) where
  /-- A placeholder for an extracted named subdiagram, positioned at interpolated transforms. -/
  | ref : Lean.Name → Skeleton β
  /-- The empty skeleton. -/
  | empty
  /-- A core primitive leaf. -/
  | prim : CorePrimitive → Skeleton β
  /-- An affine transform wrapper. -/
  | transform : Matrix → Skeleton β → Skeleton β
  /-- Overlay of two skeletons. -/
  | compose : Skeleton β → Skeleton β → Skeleton β
  /-- Opacity wrapper. -/
  | cellophane : Float → Skeleton β → Skeleton β
  /-- Clip region wrapper. -/
  | clip : PathData → Skeleton β → Skeleton β
  /-- A deferred arrow between named anchors. -/
  | arrow : LineEnd → LineEnd → Stroke → Bool → Skeleton β → Skeleton β
  /-- A named sub-skeleton (for names that were NOT extracted). -/
  | named : Lean.Name → Skeleton β → Skeleton β
  /-- Envelope override. -/
  | withEnv : Envelope → Skeleton β → Skeleton β
  /-- Pass-through wrappers. -/
  | other : Diagram β → Skeleton β
deriving Inhabited

/-!
# Skeleton normalization

Normalizes wrappers to a canonical form so that structurally equivalent
skeletons match even when wrappers appear in different orders or are
unnecessarily nested.

Canonical form (outside → inside):
`cellophane? → clip* (sorted by area/centroid) → transform? → content`

Rules:
- Consecutive transforms collapse via {name}`Matrix.mul`.
- Consecutive cellophanes collapse via multiplication.
- Cellophane commutes with transform and clip (floats outward).
- Transform commutes with clip by transforming the clip path (sinks inward).
- Clips are sorted by a semantic key (area, centroid) for stable matching.
-/

/-- Inserts a transform at its canonical position in an already-normalized skeleton. -/
private def pushTransform {β : Type} (m : Matrix) : Skeleton β → Skeleton β
  | .cellophane α d => .cellophane α (pushTransform m d)
  | .clip pd d => .clip (transformPathData m pd) (pushTransform m d)
  | .transform m' d => .transform (Matrix.mul m m') d
  | d => .transform m d

/-- Inserts a clip at its canonical position in an already-normalized skeleton. -/
private partial def pushClip {β : Type} (pd : PathData) : Skeleton β → Skeleton β
  | .cellophane α d => .cellophane α (pushClip pd d)
  | .clip pd' d =>
    if pathDataLt pd pd' then .clip pd (.clip pd' d)
    else .clip pd' (pushClip pd d)
  | d => .clip pd d

/-- Inserts a cellophane at its canonical position in an already-normalized skeleton. -/
private def pushCellophane {β : Type} (α : Float) : Skeleton β → Skeleton β
  | .cellophane α' d => .cellophane (α * α') d
  | d => .cellophane α d

/-- Normalizes a skeleton to canonical wrapper order (bottom-up). -/
def normalizeSkeleton {β : Type} : Skeleton β → Skeleton β
  | .transform m d => pushTransform m (normalizeSkeleton d)
  | .cellophane α d => pushCellophane α (normalizeSkeleton d)
  | .clip pd d => pushClip pd (normalizeSkeleton d)
  | .compose a b => .compose (normalizeSkeleton a) (normalizeSkeleton b)
  | .arrow s t st ut d => .arrow s t st ut (normalizeSkeleton d)
  | .named n d => .named n (normalizeSkeleton d)
  | .withEnv e d => .withEnv e (normalizeSkeleton d)
  | other => other

/-!
# Diagram to Skeleton conversion

Walks a diagram, extracting named subdiagrams that have a counterpart in the
other diagram. Extracted names become {name}`Skeleton.ref` nodes; unmatched names
are kept as {name}`Skeleton.named` nodes.
-/

/-- Collects all names reachable from a diagram (for determining which names to extract). -/
private def collectNames {β : Type} (d : Diagram β) : List Lean.Name :=
  go d []
where
  go : Diagram β → List Lean.Name → List Lean.Name
    | .named name inner, acc => go inner (name :: acc)
    | .transform _ d, acc | .cellophane _ d, acc | .clip _ d, acc
    | .withEnv _ d, acc | .warning _ d, acc | .tag _ d, acc
    | .foreign _ d, acc | .scope d, acc | .showEnv _ _ _ d, acc => go d acc
    -- Do not recurse into arrow children; their names are handled by
    -- the recursive prepareMorph in matchSkeletons.
    | .arrow _ _ _ _ _, acc => acc
    | .compose a b, acc => go b (go a acc)
    | _, acc => acc

/-- Extracts a named subdiagram with its accumulated transform from a diagram. -/
private def extractOne {β : Type} (target : Lean.Name) (d : Diagram β) :
    Option (Matrix × Diagram β) :=
  go Matrix.identity d
where
  go (xform : Matrix) : Diagram β → Option (Matrix × Diagram β)
    | .named name inner =>
      if name == target then some (xform, inner)
      else go xform inner
    | .transform m d => go (Matrix.mul xform m) d
    | .compose a b => (go xform a).orElse fun _ => go xform b
    | .withEnv _ d | .warning _ d | .cellophane _ d | .clip _ d
    | .tag _ d | .foreign _ d | .scope d | .showEnv _ _ _ d => go xform d
    -- Do not recurse into arrow children (names handled by matchSkeletons).
    | .arrow _ _ _ _ _ => none
    | _ => none

/--
Converts a diagram to a skeleton, replacing matched names with {name}`Skeleton.ref` nodes.

Names present in {name}`matched` are extracted; all other structure is preserved.
-/
private def toSkeleton {β : Type} (d : Diagram β) (matched : Std.HashSet Lean.Name) :
    Skeleton β :=
  match d with
  | .empty => .empty
  | .prim cp => .prim cp
  | .named name inner =>
    if matched.contains name then .ref name
    else .named name (toSkeleton inner matched)
  | .transform m inner => .transform m (toSkeleton inner matched)
  | .compose a b => .compose (toSkeleton a matched) (toSkeleton b matched)
  | .cellophane α inner => .cellophane α (toSkeleton inner matched)
  | .clip pd inner => .clip pd (toSkeleton inner matched)
  | .arrow s t st ut inner => .arrow s t st ut (.other inner)
  | .withEnv e inner => .withEnv e (toSkeleton inner matched)
  | .warning _ inner => toSkeleton inner matched
  | .foreign _ inner => toSkeleton inner matched
  | .tag _ inner => toSkeleton inner matched
  | .scope inner => toSkeleton inner matched
  | .showEnv _ _ _ inner => toSkeleton inner matched

/-!
# Skeleton structural matching
-/

/-- Computes a bounding box from a diagram's envelope. -/
private def envelopeToBBox {β : Type} [Backend β] (d : Diagram β) : BBox :=
  match d.getEnvelope with
  | .empty => { hw := 0, hh := 0 }
  | .nonempty env =>
    { hw := (env Vec2.east + env Vec2.west) / 2,
      hh := (env Vec2.north + env Vec2.south) / 2 }

/-- Prepares a path morph node from two path primitives. -/
private def preparePath {β : Type} (pdA : PathData) (fillA : Fill) (strokeA : Stroke)
    (pdB : PathData) (fillB : Fill) (strokeB : Stroke)
    (envA envB : Envelope) : MorphNode β :=
  let npA := pathToCubics pdA
  let npB := pathToCubics pdB
  let subA := (npA.subpaths[0]?.getD default)
  let subB := (npB.subpaths[0]?.getD default)
  let closed := subA.closed || subB.closed
  let (segsA, segsB) := prepareSegments subA.segments subB.segments closed
  let rfA := fillA.resolve envA
  let rfB := fillB.resolve envB
  let (rfA, rfB) := match Morph.prepareFills rfA rfB with
    | some pair => pair
    | none => (rfA, rfB)
  .path segsA segsB closed rfA rfB strokeA strokeB

/-- Converts a skeleton to a diagram for cross-fade/fade nodes. -/
def skelToDiagram {β : Type} : Skeleton β → Diagram β
  | .ref _ | .empty => .empty
  | .prim cp => .prim cp
  | .transform m d => .transform m (skelToDiagram d)
  | .compose a b => .compose (skelToDiagram a) (skelToDiagram b)
  | .cellophane α d => .cellophane α (skelToDiagram d)
  | .clip pd d => .clip pd (skelToDiagram d)
  | .arrow s t st ut d => .arrow s t st ut (skelToDiagram d)
  | .named n d => .named n (skelToDiagram d)
  | .withEnv e d => .withEnv e (skelToDiagram d)
  | .other d => d

/--
A matched name pair with transforms and content, queued for recursive processing.
-/
structure MatchedPair (β : Type) where
  /-- The shared name. -/
  name : Lean.Name
  /-- World transform in the source diagram. -/
  xformA : Matrix
  /-- World transform in the target diagram. -/
  xformB : Matrix
  /-- Content of the named subdiagram in the source. -/
  contentA : Diagram β
  /-- Content of the named subdiagram in the target. -/
  contentB : Diagram β

/-!
# Morph preparation (mutually recursive)

The functions matchSkeletons, matchOneLevel, and prepareMorph are mutually
recursive: skeleton matching may invoke full morph preparation for arrow children
(which need their own name extraction and structural matching).
-/

mutual

/-- Structurally matches two skeletons, producing a {name}`MorphNode`. -/
partial def matchSkeletons {β : Type} [Backend β]
    (refXforms : Std.HashMap Lean.Name (Matrix × Matrix))
    (a b : Skeleton β) : MorphNode β :=
  let go := matchSkeletons refXforms
  match a, b with
  | .ref n, .ref _ =>
    .nameRef n
  | .ref _, .empty | .empty, .ref _ | .empty, .empty => .empty
  | .prim (.path pdA fillA strokeA), .prim (.path pdB fillB strokeB) =>
    preparePath pdA fillA strokeA pdB fillB strokeB
      (CorePrimitive.toEnvelope (.path pdA fillA strokeA))
      (CorePrimitive.toEnvelope (.path pdB fillB strokeB))
  | .prim (.text tA sA), .prim (.text tB sB) =>
    .text tA sA tB sB
      (envelopeToBBox (Diagram.prim (.text tA sA) : Diagram β))
      (envelopeToBBox (Diagram.prim (.text tB sB) : Diagram β))
  | .prim (.styledText lA aA), .prim (.styledText lB aB) =>
    .crossFade (skelToDiagram (.prim (.styledText lA aA))) (skelToDiagram (.prim (.styledText lB aB)))
  | .transform mA dA, .transform mB dB =>
    .transform mA mB (go dA dB)
  | .compose aL aR, .compose bL bR =>
    .compose (go aL bL) (go aR bR)
  | .cellophane αA dA, .cellophane αB dB =>
    .cellophane αA αB (go dA dB)
  | .clip pdA dA, .clip pdB dB =>
    let npA := pathToCubics pdA
    let npB := pathToCubics pdB
    let subA := (npA.subpaths[0]?.getD default)
    let subB := (npB.subpaths[0]?.getD default)
    let closed := subA.closed || subB.closed
    let (segsA, segsB) := prepareSegments subA.segments subB.segments closed
    .clip segsA segsB closed (go dA dB)
  | .arrow startA stopA strokeA utA dA, .arrow startB stopB strokeB utB dB =>
    -- Recursively prepare the arrow children with full name matching,
    -- keeping geometry inside the arrow for trace resolution and z-order.
    let childPlan := prepareMorph (skelToDiagram dA) (skelToDiagram dB)
    .arrow startA startB stopA stopB strokeA strokeB utA utB childPlan
  | .named nA dA, .named nB dB =>
    if nA == nB then .matched nA Matrix.identity Matrix.identity (go dA dB)
    else .crossFade (skelToDiagram (.named nA dA)) (skelToDiagram (.named nB dB))
  -- Pass-through: unwrap withEnv
  | .withEnv _ dA, other => go dA other
  | other, .withEnv _ dB => go other dB
  -- Fade in/out for mismatched empty/non-empty
  | .empty, other => .fadeIn (skelToDiagram other)
  | other, .empty => .fadeOut (skelToDiagram other)
  | .ref _, other => .fadeIn (skelToDiagram other)
  | other, .ref _ => .fadeOut (skelToDiagram other)
  -- Asymmetric wrappers: treat missing side as identity
  | .transform mA dA, other =>
    .transform mA Matrix.identity (go dA other)
  | other, .transform mB dB =>
    .transform Matrix.identity mB (go other dB)
  | .cellophane αA dA, other =>
    .cellophane αA 1.0 (go dA other)
  | other, .cellophane αB dB =>
    .cellophane 1.0 αB (go other dB)
  | .clip pdA dA, other =>
    let otherPd := envelopeToPathData (skelToDiagram other : Diagram β)
    let npA := pathToCubics pdA
    let npB := pathToCubics otherPd
    let subA := (npA.subpaths[0]?.getD default)
    let subB := (npB.subpaths[0]?.getD default)
    let closed := subA.closed || subB.closed
    let (segsA, segsB) := prepareSegments subA.segments subB.segments closed
    .clip segsA segsB closed (go dA other)
  | other, .clip pdB dB =>
    let otherPd := envelopeToPathData (skelToDiagram other : Diagram β)
    let npA := pathToCubics otherPd
    let npB := pathToCubics pdB
    let subA := (npA.subpaths[0]?.getD default)
    let subB := (npB.subpaths[0]?.getD default)
    let closed := subA.closed || subB.closed
    let (segsA, segsB) := prepareSegments subA.segments subB.segments closed
    .clip segsA segsB closed (go other dB)
  -- Fallback
  | x, y => .crossFade (skelToDiagram x) (skelToDiagram y)

/--
Processes one level of name matching: extracts shared names, builds skeletons,
structurally matches, and returns the residual morph node plus queued pairs.
-/
partial def matchOneLevel {β : Type} [Backend β] (a b : Diagram β) :
    MorphNode β × Array (MatchedPair β) := Id.run do
  let namesA := collectNames a
  let namesB := collectNames b
  let nameSetB : Std.HashSet Lean.Name := namesB.foldl (·.insert ·) {}
  let shared : Std.HashSet Lean.Name :=
    namesA.foldl (fun s n => if nameSetB.contains n then s.insert n else s) {}
  if shared.isEmpty then
    let skelA := normalizeSkeleton (toSkeleton a {})
    let skelB := normalizeSkeleton (toSkeleton b {})
    return (matchSkeletons {} skelA skelB, #[])
  -- Extract matched pairs first to build the transform map
  let mut pairs : Array (MatchedPair β) := #[]
  let mut refXforms : Std.HashMap Lean.Name (Matrix × Matrix) := {}
  for name in shared do
    match extractOne name a, extractOne name b with
    | some (xfA, cA), some (xfB, cB) =>
      pairs := pairs.push { name, xformA := xfA, xformB := xfB, contentA := cA, contentB := cB }
      refXforms := refXforms.insert name (xfA, xfB)
    | _, _ => pure ()
  -- Build skeletons with shared names extracted as refs
  let skelA := normalizeSkeleton (toSkeleton a shared)
  let skelB := normalizeSkeleton (toSkeleton b shared)
  let residual := matchSkeletons refXforms skelA skelB
  (residual, pairs)

/--
Prepares a morph plan between two diagrams.

Recursively extracts matched named subdiagrams, structurally matches residuals,
and queues matched pairs for further processing until no more shared names remain.
-/
partial def prepareMorph {β : Type} [Backend β] (a b : Diagram β) : MorphNode β :=
  let (residual, pairs) := matchOneLevel a b
  -- Process each matched pair recursively
  let matchedNodes := pairs.map fun p =>
    let child := prepareMorph p.contentA p.contentB
    MorphNode.matched p.name p.xformA p.xformB child
  -- Unmatched names from either side become fade-in/out via the residual
  -- (they show up as fadeIn/fadeOut in matchSkeletons when one side has
  -- content and the other has .empty/.ref)
  -- Compose matched nodes with the residual
  matchedNodes.foldl (fun acc n => .compose acc n) residual

end
