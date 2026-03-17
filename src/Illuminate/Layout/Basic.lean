/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/

import Illuminate.Geometry
import Illuminate.Style
import Illuminate.Diagram
import Illuminate.Render
import Illuminate.Layout.Measure


namespace Illuminate

-- ═══════════════════════════════════════════════════════════════
-- LayoutDiagram
-- ═══════════════════════════════════════════════════════════════

/-- A resolved name path (e.g. `A.east`). -/
abbrev NamePath := Lean.Name

/--
A diagram tree with deferred-node caching for fixed-point layout resolution.
Mirrors `Diagram` but adds a `valueSoFar` cache on deferred nodes.
-/
inductive LayoutDiagram (β : Type) where
  /-- The empty layout diagram. -/
  | empty : LayoutDiagram β
  /-- Wraps a single primitive shape. -/
  | prim : Primitive β → LayoutDiagram β
  /-- Attaches a numeric tag for hit-testing. -/
  | annotate : Nat → LayoutDiagram β → LayoutDiagram β
  /-- Names a sub-diagram for anchor lookup. -/
  | named : Lean.Name → LayoutDiagram β → LayoutDiagram β
  /-- Applies an affine transformation. -/
  | transform : Matrix → LayoutDiagram β → LayoutDiagram β
  /-- Overlays two layout diagrams. -/
  | compose : LayoutDiagram β → LayoutDiagram β → LayoutDiagram β
  /-- Overrides the envelope. -/
  | withEnv : Envelope → LayoutDiagram β → LayoutDiagram β
  /-- Embeds a validation warning. -/
  | warning : String → LayoutDiagram β → LayoutDiagram β
  /-- Applies a cascading draw configuration. -/
  | withConfig : DrawConfig → LayoutDiagram β → LayoutDiagram β
  /-- Wraps a sub-diagram with a given opacity. -/
  | cellophane : Float → LayoutDiagram β → LayoutDiagram β
  /-- Clips a sub-diagram to a path boundary. -/
  | clip : PathData → LayoutDiagram β → LayoutDiagram β
  /-- A deferred node awaiting anchor resolution.
      Stores the queried name, the callback, and optionally the resolved
      position and cached result from a previous iteration. -/
  | deferred : Lean.Name → (ResolvedConfig → Vec2 → LayoutDiagram β)
      → Option (Vec2 × LayoutDiagram β) → LayoutDiagram β

-- ═══════════════════════════════════════════════════════════════
-- Convert Diagram → LayoutDiagram
-- ═══════════════════════════════════════════════════════════════

/-- Converts a `Diagram` tree into a `LayoutDiagram` tree with empty caches. -/
def toLayout {β : Type} : Diagram β → LayoutDiagram β
  | .empty => .empty
  | .prim p => .prim p
  | .annotate tag d => .annotate tag (toLayout d)
  | .named name d => .named name (toLayout d)
  | .transform m d => .transform m (toLayout d)
  | .compose a b => .compose (toLayout a) (toLayout b)
  | .withEnv env d => .withEnv env (toLayout d)
  | .warning msg d => .warning msg (toLayout d)
  | .withConfig cfg d => .withConfig cfg (toLayout d)
  | .cellophane α d => .cellophane α (toLayout d)
  | .clip pd d => .clip pd (toLayout d)
  | .deferred name f => .deferred name (fun rc v => toLayout (f rc v)) none

-- ═══════════════════════════════════════════════════════════════
-- LayoutDiagram envelope
-- ═══════════════════════════════════════════════════════════════

namespace LayoutDiagram

variable {β : Type}

/-- Computes the envelope of a layout diagram. Deferred nodes use their cached value. -/
def getEnvelope : LayoutDiagram β → Envelope
  | .empty => Envelope.empty
  | .prim p => p.toEnvelope
  | .annotate _ d => d.getEnvelope
  | .named _ d => d.getEnvelope
  | .transform m d => Envelope.transform m d.getEnvelope
  | .compose a b => Envelope.union a.getEnvelope b.getEnvelope
  | .withEnv env _ => env
  | .warning _ d => d.getEnvelope
  | .withConfig _ d => d.getEnvelope
  | .cellophane _ d => d.getEnvelope
  | .clip _ d => d.getEnvelope
  | .deferred _ _ cache => match cache with
    | some (_, result) => result.getEnvelope
    | none => Envelope.empty

/--
Builds an envelope from a `MeasuredBox` and text anchor.
The box provides width, ascent, and descent; the anchor determines horizontal placement.
-/
private def envelopeOfMeasuredText (box : MeasuredBox) (anchor : TextAnchor) : Envelope :=
  let h := (box.ascent + box.descent) / 2
  match anchor with
  | .start => Envelope.ofBounds ⟨0, -h⟩ ⟨box.width, h⟩
  | .«end» => Envelope.ofBounds ⟨-box.width, -h⟩ ⟨0, h⟩
  | .middle => Envelope.ofRect (box.width / 2) h

/-- Computes the envelope of a layout diagram using monadic measurement for text and foreign primitives. -/
def getEnvelopeM {m : Type → Type} [Monad m] [LayoutMeasure β m]
    : LayoutDiagram β → m Envelope
  | .empty => pure Envelope.empty
  | .prim p => do
    match p with
    | .core (.path pd _ _) =>
      let pts := pd.commands.foldl (init := []) fun acc cmd =>
        match cmd with
        | .moveTo pt => pt :: acc
        | .lineTo pt => pt :: acc
        | .curveTo c1 c2 ep => ep :: c2 :: c1 :: acc
        | .closePath => acc
      pure (Envelope.ofVertices pts)
    | .core (.text s style) =>
      let fullStyle := style.resolve ResolvedConfig.defaults
      let box ← (LayoutMeasure.measureText (β := β) s fullStyle : m MeasuredBox)
      pure (envelopeOfMeasuredText box fullStyle.anchor)
    | .core (.image ref) =>
      pure (Envelope.ofRect (ref.width / 2) (ref.height / 2))
    | .foreign f _ =>
      LayoutMeasure.measureForeign f
  | .annotate _ d => d.getEnvelopeM
  | .named _ d => d.getEnvelopeM
  | .transform mat d => do
    let env ← d.getEnvelopeM
    pure (Envelope.transform mat env)
  | .compose a b => do
    let envA ← a.getEnvelopeM
    let envB ← b.getEnvelopeM
    pure (Envelope.union envA envB)
  | .withEnv env _ => pure env
  | .warning _ d => d.getEnvelopeM
  | .withConfig _ d => d.getEnvelopeM
  | .cellophane _ d => d.getEnvelopeM
  | .clip _ d => d.getEnvelopeM
  | .deferred _ _ cache => match cache with
    | some (_, result) => result.getEnvelopeM
    | none => pure Envelope.empty

-- ═══════════════════════════════════════════════════════════════
-- Collect names from LayoutDiagram
-- ═══════════════════════════════════════════════════════════════

/-- Collects all named anchor positions from a layout diagram tree. -/
def collectNames (d : LayoutDiagram β) (xform : Matrix) (pfx : NamePath)
    (acc : List (NamePath × Vec2)) : List (NamePath × Vec2) :=
  match d with
  | .empty => acc
  | .prim _ => acc
  | .annotate _ d => collectNames d xform pfx acc
  | .named name d =>
    let pos := Matrix.apply xform ⟨0, 0⟩
    let qualName := match pfx with
      | .anonymous => name
      | _ => pfx ++ name
    let acc := acc ++ [(qualName, pos)]
    collectNames d xform qualName acc
  | .transform m d =>
    collectNames d (Matrix.mul xform m) pfx acc
  | .compose a b =>
    let acc := collectNames a xform pfx acc
    collectNames b xform pfx acc
  | .withEnv _ d => collectNames d xform pfx acc
  | .warning _ d => collectNames d xform pfx acc
  | .withConfig _ d => collectNames d xform pfx acc
  | .cellophane _ d => collectNames d xform pfx acc
  | .clip _ d => collectNames d xform pfx acc
  | .deferred _ _ cache => match cache with
    | some (_, result) => collectNames result xform pfx acc
    | none => acc

-- ═══════════════════════════════════════════════════════════════
-- Compile LayoutDiagram → List DrawCmd
-- ═══════════════════════════════════════════════════════════════

/-- Compiles a layout diagram tree into a flat display list. -/
def compile (d : LayoutDiagram β) : List DrawCmd :=
  go d ResolvedConfig.defaults []
where
  go (d : LayoutDiagram β) (rc : ResolvedConfig) (acc : List DrawCmd) : List DrawCmd :=
    match d with
    | .empty => acc
    | .prim p =>
      match p with
      | .core (.path pd fill stroke) =>
        let fullFill := fill.resolve rc
        let fullStroke := stroke.resolve rc
        let acc := if fullFill.color.a > 0 then acc ++ [.fillPath pd fullFill] else acc
        if fullStroke.width > 0 && fullStroke.color.a > 0 then acc ++ [.strokePath pd fullStroke]
        else acc
      | .core (.text s style) =>
        let fullStyle := style.resolve rc
        acc ++ [.drawTextRun s fullStyle ⟨0, 0⟩]
      | .core (.image _) => acc
      | .foreign _ (some cp) =>
        go (.prim (.core cp)) rc acc
      | .foreign _ none => acc
    | .annotate tag d =>
      let inner := go d rc []
      acc ++ [.pushAnnotation tag] ++ inner ++ [.popAnnotation]
    | .named _ d => go d rc acc
    | .transform m d =>
      let inner := go d rc []
      acc ++ [.pushTransform m] ++ inner ++ [.popTransform]
    | .compose a b =>
      let acc := go a rc acc
      go b rc acc
    | .withEnv _ d => go d rc acc
    | .warning _ d => go d rc acc
    | .withConfig cfg d =>
      let rc' := cfg.resolve rc
      go d rc' acc
    | .cellophane α d =>
      let inner := go d rc []
      acc ++ [.pushOpacity α] ++ inner ++ [.popOpacity]
    | .clip pd d =>
      let clipId := acc.length
      let inner := go d rc []
      acc ++ [.pushClip pd clipId] ++ inner ++ [.popClip]
    | .deferred _ _ cache => match cache with
      | some (_, result) => go result rc acc
      | none => acc

-- ═══════════════════════════════════════════════════════════════
-- Resolve: fixed-point iteration
-- ═══════════════════════════════════════════════════════════════

/-- Looks up a name in a name table. -/
private def lookupName (name : NamePath) (table : List (NamePath × Vec2)) : Option Vec2 :=
  table.foldl (init := none) fun acc (n, p) =>
    if n == name then some p else acc

/--
Performs one resolution step: walks the tree, resolves deferred nodes
using the current name table, and returns the updated tree plus a flag
indicating whether anything changed. Tracks the accumulated transform
so deferred callbacks receive positions in their local coordinate frame.
-/
private def resolveStep (d : LayoutDiagram β) (table : List (NamePath × Vec2))
    (xform : Matrix := Matrix.identity)
    (rc : ResolvedConfig := ResolvedConfig.defaults) : LayoutDiagram β × Bool :=
  match d with
  | .empty => (.empty, false)
  | .prim p => (.prim p, false)
  | .annotate tag d =>
    let (d', c) := resolveStep d table xform rc
    (.annotate tag d', c)
  | .named name d =>
    let (d', c) := resolveStep d table xform rc
    (.named name d', c)
  | .transform m d =>
    let (d', c) := resolveStep d table (Matrix.mul xform m) rc
    (.transform m d', c)
  | .compose a b =>
    let (a', ca) := resolveStep a table xform rc
    let (b', cb) := resolveStep b table xform rc
    (.compose a' b', ca || cb)
  | .withEnv env d =>
    let (d', c) := resolveStep d table xform rc
    (.withEnv env d', c)
  | .warning msg d =>
    let (d', c) := resolveStep d table xform rc
    (.warning msg d', c)
  | .withConfig cfg d =>
    let rc' := cfg.resolve rc
    let (d', c) := resolveStep d table xform rc'
    (.withConfig cfg d', c)
  | .cellophane α d =>
    let (d', c) := resolveStep d table xform rc
    (.cellophane α d', c)
  | .clip pd d =>
    let (d', c) := resolveStep d table xform rc
    (.clip pd d', c)
  | .deferred name f cache =>
    match lookupName name table with
    | none => (.deferred name f cache, false)
    | some globalPos =>
      -- Convert global position to local coordinates
      let localPos := match Matrix.inverse xform with
        | some inv => Matrix.apply inv globalPos
        | none => globalPos
      match cache with
      | some (oldPos, oldResult) =>
        if oldPos == localPos then
          -- Same position: recurse into cached result to resolve inner deferreds
          let (result', c) := resolveStep oldResult table xform rc
          (.deferred name f (some (localPos, result')), c)
        else
          -- Position changed: re-call callback
          let newResult := f rc localPos
          (.deferred name f (some (localPos, newResult)), true)
      | none =>
        -- First resolution: call callback
        let newResult := f rc localPos
        (.deferred name f (some (localPos, newResult)), true)

/--
Iteratively resolves all deferred nodes in a layout diagram.
Runs up to `maxIter` iterations, stopping early if convergence is reached.
-/
def resolve (maxIter : Nat := 20) (d : LayoutDiagram β) : LayoutDiagram β :=
  go maxIter d
where
  go (remaining : Nat) (d : LayoutDiagram β) : LayoutDiagram β :=
    match remaining with
    | 0 => d
    | n + 1 =>
      let table := collectNames d Matrix.identity .anonymous []
      let (d', changed) := resolveStep d table
      if changed then go n d'
      else d'

/-- Looks up the global position of a named anchor. -/
def anchorPoint (name : NamePath) (d : LayoutDiagram β) : Option Vec2 :=
  let table := d.collectNames Matrix.identity .anonymous []
  table.foldl (init := none) fun acc (n, p) =>
    if n == name then some p else acc

/-- Collects named envelope entries from the tree. -/
private def collectEnvelopes (d : LayoutDiagram β) (acc : List (NamePath × Envelope))
    : List (NamePath × Envelope) :=
  match d with
  | .empty => acc
  | .prim _ => acc
  | .annotate _ d => collectEnvelopes d acc
  | .named name d =>
    let env := d.getEnvelope
    let acc := acc ++ [(name, env)]
    collectEnvelopes d acc
  | .transform _ d => collectEnvelopes d acc
  | .compose a b =>
    let acc := collectEnvelopes a acc
    collectEnvelopes b acc
  | .withEnv _ d => collectEnvelopes d acc
  | .warning _ d => collectEnvelopes d acc
  | .withConfig _ d => collectEnvelopes d acc
  | .cellophane _ d => collectEnvelopes d acc
  | .clip _ d => collectEnvelopes d acc
  | .deferred _ _ cache => match cache with
    | some (_, result) => collectEnvelopes result acc
    | none => acc

/-- Collects named envelope entries using monadic measurement. -/
private def collectEnvelopesM {m : Type → Type} [Monad m] [LayoutMeasure β m]
    (d : LayoutDiagram β) (acc : List (NamePath × Envelope))
    : m (List (NamePath × Envelope)) :=
  match d with
  | .empty => pure acc
  | .prim _ => pure acc
  | .annotate _ d => collectEnvelopesM d acc
  | .named name d => do
    let env ← d.getEnvelopeM
    let acc := acc ++ [(name, env)]
    collectEnvelopesM d acc
  | .transform _ d => collectEnvelopesM d acc
  | .compose a b => do
    let acc ← collectEnvelopesM a acc
    collectEnvelopesM b acc
  | .withEnv _ d => collectEnvelopesM d acc
  | .warning _ d => collectEnvelopesM d acc
  | .withConfig _ d => collectEnvelopesM d acc
  | .cellophane _ d => collectEnvelopesM d acc
  | .clip _ d => collectEnvelopesM d acc
  | .deferred _ _ cache => match cache with
    | some (_, result) => collectEnvelopesM result acc
    | none => pure acc

/-- Queries the envelope extent of a named sub-diagram in a given direction. -/
def envelopeIn (name : NamePath) (dir : Vec2) (d : LayoutDiagram β) : Option Float :=
  let envs := collectEnvelopes d []
  envs.foldl (init := none) fun acc (n, env) =>
    if n == name then some (env dir) else acc

/-- Queries the envelope extent of a named sub-diagram using monadic measurement. -/
def envelopeInM {m : Type → Type} [Monad m] [LayoutMeasure β m]
    (name : NamePath) (dir : Vec2) (d : LayoutDiagram β) : m (Option Float) := do
  let envs ← collectEnvelopesM d []
  pure (envs.foldl (init := none) fun acc (n, env) =>
    if n == name then some (env dir) else acc)

end LayoutDiagram

-- ═══════════════════════════════════════════════════════════════
-- Convenience: full pipeline
-- ═══════════════════════════════════════════════════════════════

/--
Renders a diagram to an SVG string using monadic measurement.
Handles deferred node resolution automatically via fixed-point iteration.
-/
def renderDiagramM {β : Type} {m : Type → Type} [Monad m] [LayoutMeasure β m]
    (d : Diagram β) (padding : Float := 2)
    (maxIter : Nat := 20) : m String := do
  let ld := toLayout d
  let resolved := ld.resolve maxIter
  let env ← resolved.getEnvelopeM
  let east := env Vec2.east
  let west := env Vec2.west
  let north := env Vec2.north
  let south := env Vec2.south
  let minX := -(west + padding)
  let minY := -(north + padding)
  let w := west + east + 2 * padding
  let h := north + south + 2 * padding
  let cmds := resolved.compile
  pure (Svg.render cmds (minX, minY, w, h))

/--
Renders a diagram to an SVG string using the default heuristic measurement.
Handles deferred node resolution automatically via fixed-point iteration.
-/
def renderDiagram {β : Type} [LayoutMeasure β Id] (d : Diagram β) (padding : Float := 2)
    (maxIter : Nat := 20) : String :=
  Id.run (renderDiagramM d (padding := padding) (maxIter := maxIter))
