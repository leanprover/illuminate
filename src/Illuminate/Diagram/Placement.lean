/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/

import Illuminate.Geometry
import Illuminate.Style
import Illuminate.Diagram.Basic


namespace Illuminate

-- ═══════════════════════════════════════════════════════════════
-- PathData bounds computation
-- ═══════════════════════════════════════════════════════════════

namespace PathData

/--
Computes the axis-aligned bounding box of a path.
Returns `(lo, hi)` where `lo` is the min corner and `hi` is the max corner.
For cubic Béziers, uses the control-point hull (conservative).
-/
private def extendBounds (acc : Option (Vec2 × Vec2)) (p : Vec2) : Option (Vec2 × Vec2) :=
  match acc with
  | none => some (p, p)
  | some (lo, hi) =>
    some (⟨Min.min lo.x p.x, Min.min lo.y p.y⟩,
          ⟨Max.max hi.x p.x, Max.max hi.y p.y⟩)

/-- Computes the axis-aligned bounding box of a path as `(lo, hi)` corners. -/
def bounds (pd : PathData) : Vec2 × Vec2 :=
  let acc := pd.commands.foldl (init := none) fun acc cmd =>
    match cmd with
    | .moveTo p => extendBounds acc p
    | .lineTo p => extendBounds acc p
    | .curveTo c1 c2 ep => extendBounds (extendBounds (extendBounds acc c1) c2) ep
    | .closePath => acc
  acc.getD (⟨0, 0⟩, ⟨0, 0⟩)

end PathData

-- ═══════════════════════════════════════════════════════════════
-- Envelope computation from primitives and diagrams
-- ═══════════════════════════════════════════════════════════════

namespace CorePrimitive

/--
Computes the envelope of a core primitive.
Text uses a placeholder size: `charCount * fontSize * 0.6` width, `fontSize` height.
Real text sizes come from the measurement pass (Layer 5).
-/
def toEnvelope : CorePrimitive → Envelope
  | .path pd _ _ =>
    let pts := pd.commands.foldl (init := []) fun acc cmd =>
      match cmd with
      | .moveTo p => p :: acc
      | .lineTo p => p :: acc
      | .curveTo c1 c2 ep => ep :: c2 :: c1 :: acc
      | .closePath => acc
    Envelope.ofVertices pts
  | .text s style =>
    let fontSize := style.fontSize.getD 16
    let anchor := style.anchor.getD .middle
    let lines := s.splitOn "\n"
    let nLines := Max.max 1 lines.length
    if nLines == 1 then
      let totalW := estimateTextWidth fontSize s
      let h := (fontSize * 0.75 + fontSize * 0.25) / 2
      match anchor with
      | .start => Envelope.ofBounds ⟨0, -h⟩ ⟨totalW, h⟩
      | .«end» => Envelope.ofBounds ⟨-totalW, -h⟩ ⟨0, h⟩
      | .middle => Envelope.ofRect (totalW / 2) h
    else
      -- Build a tight envelope as the union of per-line envelopes.
      -- Each line is centered at its own y offset, matching SVG tspan layout.
      let lineHeight := fontSize * 1.2
      let totalH := lineHeight * (nLines - 1).toFloat
      let halfLine := fontSize / 2
      -- Collect corners of each line's bounding box and build the
      -- envelope from all vertices. Envelopes are intrinsically convex
      -- (they describe intersections of half-planes), so this gives the
      -- tightest convex bound on the stacked rectangles.
      let vertices := lines.mapIdx fun i line =>
        let w := estimateTextWidth fontSize line
        let cy := totalH / 2 - i.toFloat * lineHeight
        let (left, right) := match anchor with
          | .start => (0.0, w)
          | .«end» => (-w, 0.0)
          | .middle => (-w / 2, w / 2)
        [⟨left, cy - halfLine⟩, ⟨right, cy - halfLine⟩,
         ⟨left, cy + halfLine⟩, ⟨right, cy + halfLine⟩]
      Envelope.ofVertices (vertices.flatten)
  | .image ref =>
    Envelope.ofRect (ref.width / 2) (ref.height / 2)

end CorePrimitive

namespace Primitive

variable {β : Type}

/--
Computes the envelope of a primitive. Foreign primitives with a core fallback
use the fallback's envelope; otherwise they have zero envelope.
-/
def toEnvelope : Primitive β → Envelope
  | .core cp => cp.toEnvelope
  | .foreign _ (some cp) => cp.toEnvelope
  | .foreign _ none => Envelope.empty

end Primitive

namespace Diagram

variable {β : Type}

-- ═══════════════════════════════════════════════════════════════
-- Envelope extraction
-- ═══════════════════════════════════════════════════════════════

/-- Computes the envelope of a diagram by recursive traversal. -/
def getEnvelope : Diagram β → Envelope
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
  | .deferred _ _ => Envelope.empty

/-- Names a diagram and adds cardinal anchors derived from its envelope. -/
def namedWithAnchors (n : Lean.Name) (d : Diagram β) : Diagram β :=
  let env := d.getEnvelope
  -- Envelope values are always positive extents from the origin.
  -- West and south anchors are negated to place them in negative x/y.
  let e := env Vec2.east
  let w := env Vec2.west
  let no := env Vec2.north
  let s := env Vec2.south
  withNameAndAnchors d n [
    (`north, ⟨0, no⟩), (`south, ⟨0, -s⟩),
    (`east, ⟨e, 0⟩), (`west, ⟨-w, 0⟩),
    (`northeast, ⟨e, no⟩), (`northwest, ⟨-w, no⟩),
    (`southeast, ⟨e, -s⟩), (`southwest, ⟨-w, -s⟩)
  ]

/-- Computes the total width of a diagram from its envelope. -/
def width (d : Diagram β) : Float :=
  let env := d.getEnvelope
  env Vec2.east + env Vec2.west

/-- Computes the total height of a diagram from its envelope. -/
def height (d : Diagram β) : Float :=
  let env := d.getEnvelope
  env Vec2.north + env Vec2.south

-- ═══════════════════════════════════════════════════════════════
-- Alignment types
-- ═══════════════════════════════════════════════════════════════

/-- Cross-axis alignment for elements arranged horizontally. -/
inductive HorizontalAlignment where
  /-- Aligns top edges of all elements. -/
  | top
  /-- Aligns vertical centers of all elements. -/
  | center
  /-- Aligns bottom edges of all elements. -/
  | bottom
deriving Repr, BEq, Inhabited

/-- Cross-axis alignment for elements arranged vertically. -/
inductive VerticalAlignment where
  /-- Aligns left edges of all elements. -/
  | left
  /-- Aligns horizontal centers of all elements. -/
  | center
  /-- Aligns right edges of all elements. -/
  | right
deriving Repr, BEq, Inhabited

-- ═══════════════════════════════════════════════════════════════
-- Spatial composition
-- ═══════════════════════════════════════════════════════════════

/--
Translates a diagram so the center of its envelope is at the origin.
The envelope is adjusted to match.
-/
private def centerOrigin (d : Diagram β) : Diagram β :=
  let env := d.getEnvelope
  let cx := (env Vec2.east - env Vec2.west) / 2
  let cy := (env Vec2.north - env Vec2.south) / 2
  if cx.abs < 0.001 && cy.abs < 0.001 then d
  else .transform (Matrix.translate (-cx) (-cy)) d

/-- Layers `b` atop `a`, sharing the origin. `a` is drawn first (behind), `b` on top. -/
def atop (a b : Diagram β) : Diagram β := .compose a b

/--
Places `b` beside `a` in direction `v` with the given gap.
The displacement is `envA(v) + envB(-v) + gap` in the direction `v`.
The result is re-centered at the origin.
-/
def beside (v : Vec2) (gap : Float := 0) (a b : Diagram β) : Diagram β :=
  let envA := a.getEnvelope
  let envB := b.getEnvelope
  let dist := envA v + envB (-v) + gap
  let offset := dist • v
  centerOrigin (.compose a (.transform (Matrix.translate offset.x offset.y) b))

/--
Places `b` to the right of `a` with a gap, aligning vertically according
to the envelope centers (or top/bottom edges).
-/
private def besideH (gap : Float) (align : HorizontalAlignment)
    (a b : Diagram β) : Diagram β :=
  let envA := a.getEnvelope
  let envB := b.getEnvelope
  let dx := envA Vec2.east + envB Vec2.west + gap
  let dy := match align with
    | .top => envA Vec2.north - envB Vec2.north
    | .center =>
      let centerA := (envA Vec2.north - envA Vec2.south) / 2
      let centerB := (envB Vec2.north - envB Vec2.south) / 2
      centerA - centerB
    | .bottom => envB Vec2.south - envA Vec2.south
  .compose a (.transform (Matrix.translate dx dy) b)

/--
Places `b` below `a` with a gap, aligning horizontally according
to the envelope centers (or left/right edges).
-/
private def besideV (gap : Float) (align : VerticalAlignment)
    (a b : Diagram β) : Diagram β :=
  let envA := a.getEnvelope
  let envB := b.getEnvelope
  let dy := -(envA Vec2.south + envB Vec2.north + gap)
  let dx := match align with
    | .left => envB Vec2.west - envA Vec2.west
    | .center =>
      let centerA := (envA Vec2.east - envA Vec2.west) / 2
      let centerB := (envB Vec2.east - envB Vec2.west) / 2
      centerA - centerB
    | .right => envA Vec2.east - envB Vec2.east
  .compose a (.transform (Matrix.translate dx dy) b)

/-- Places `b` to the right of `a` with zero gap. -/
def hjoin (a b : Diagram β) : Diagram β :=
  beside Vec2.east 0 a b

/-- Places `b` below `a` with zero gap. -/
def vjoin (a b : Diagram β) : Diagram β :=
  beside Vec2.south 0 a b

/-- Horizontal concatenation of a list of diagrams. The result is re-centered at the origin. -/
def hcat (ds : List (Diagram β)) (align : HorizontalAlignment := .center) : Diagram β :=
  match ds with
  | [] => .empty
  | [d] => d
  | d :: rest => centerOrigin (rest.foldl (besideH 0 align) d)

/-- Vertical concatenation of a list of diagrams. The result is re-centered at the origin. -/
def vcat (ds : List (Diagram β)) (align : VerticalAlignment := .center) : Diagram β :=
  match ds with
  | [] => .empty
  | [d] => d
  | d :: rest => centerOrigin (rest.foldl (besideV 0 align) d)

/-- Horizontal concatenation with spacing inserted between elements. -/
def hsep (spacing : Float) (ds : List (Diagram β))
    (align : HorizontalAlignment := .center) : Diagram β :=
  match ds with
  | [] => .empty
  | [d] => d
  | d :: rest => centerOrigin (rest.foldl (besideH spacing align) d)

/-- Vertical concatenation with spacing inserted between elements. -/
def vsep (spacing : Float) (ds : List (Diagram β))
    (align : VerticalAlignment := .center) : Diagram β :=
  match ds with
  | [] => .empty
  | [d] => d
  | d :: rest => centerOrigin (rest.foldl (besideV spacing align) d)

/--
Lay out diagrams in a grid with uniform cell sizes.
`None` entries are treated as empty cells.
-/
def grid (rows : Array (Array (Option (Diagram β))))
    (hSpacing : Float := 0) (vSpacing : Float := 0) : Diagram β :=
  if rows.isEmpty then .empty
  else
    let nCols := rows.foldl (init := 0) fun mx row => Max.max mx row.size
    -- Per-column widths: max width of any cell in that column
    let colWidths := Array.range nCols |>.map fun c =>
      rows.foldl (init := 0.0) fun mx row =>
        match row[c]? with
        | some (some d) =>
          let env := d.getEnvelope
          Max.max mx (env Vec2.east + env Vec2.west)
        | _ => mx
    -- Per-row heights: max height of any cell in that row
    let rowHeights := rows.map fun row =>
      row.foldl (init := 0.0) fun mh cell =>
        match cell with
        | some d =>
          let env := d.getEnvelope
          Max.max mh (env Vec2.north + env Vec2.south)
        | none => mh
    -- Build the grid row by row
    let diagramRows := rows.mapIdx fun r row =>
      let rh := rowHeights[r]?.getD 0
      let cells := Array.range nCols |>.map fun c =>
        let d := (row[c]?.getD none).getD .empty
        let cw := colWidths[c]?.getD 0
        let cellEnv := Envelope.ofRect (cw / 2) (rh / 2)
        Diagram.withEnv cellEnv d
      cells.toList
    let rowDiagrams := diagramRows.map fun cells =>
      hsep hSpacing cells
    vsep vSpacing rowDiagrams.toList

-- ═══════════════════════════════════════════════════════════════
-- Anchors
-- ═══════════════════════════════════════════════════════════════

/-- A zero-size named point at the current origin. -/
def anchor (name : Lean.Name) : Diagram β :=
  .named name .empty

-- ═══════════════════════════════════════════════════════════════
-- Padding
-- ═══════════════════════════════════════════════════════════════

/-- Uniform padding on all sides. -/
def pad (amount : Float) (d : Diagram β) : Diagram β :=
  let env := d.getEnvelope
  let padded : Envelope := fun v => env v + amount
  .withEnv padded d

/-- Pads only the right side (positive x). -/
def padRight (amount : Float) (d : Diagram β) : Diagram β :=
  let env := d.getEnvelope
  let padded : Envelope := fun v =>
    if v == Vec2.east then env v + amount else env v
  .withEnv padded d

/-- Pads only the left side (negative x). -/
def padLeft (amount : Float) (d : Diagram β) : Diagram β :=
  let env := d.getEnvelope
  let padded : Envelope := fun v =>
    if v == Vec2.west then env v + amount else env v
  .withEnv padded d

/-- Pads only the top (positive y). -/
def padTop (amount : Float) (d : Diagram β) : Diagram β :=
  let env := d.getEnvelope
  let padded : Envelope := fun v =>
    if v == Vec2.north then env v + amount else env v
  .withEnv padded d

/-- Pads only the bottom (negative y). -/
def padBottom (amount : Float) (d : Diagram β) : Diagram β :=
  let env := d.getEnvelope
  let padded : Envelope := fun v =>
    if v == Vec2.south then env v + amount else env v
  .withEnv padded d

/-- Pad horizontally and vertically by different amounts. -/
def padXY (px py : Float) (d : Diagram β) : Diagram β :=
  padLeft px (padRight px (padTop py (padBottom py d)))

/-- Pad left, right, top, bottom independently. -/
def padLRTB (l r t b : Float) (d : Diagram β) : Diagram β :=
  padLeft l (padRight r (padTop t (padBottom b d)))

-- ═══════════════════════════════════════════════════════════════
-- Envelope overrides
-- ═══════════════════════════════════════════════════════════════

/-- Replace the envelope of a diagram entirely. -/
def withEnvelope (env : Envelope) (d : Diagram β) : Diagram β :=
  .withEnv env d

/--
Renders content but contributes zero envelope to layout (invisible to composition). The dual of
`ghost`, which contributes envelope but renders nothing.
-/
def floating (d : Diagram β) : Diagram β :=
  .withEnv Envelope.empty d

/-- A blank diagram with the given envelope (invisible spacer). -/
def strut (env : Envelope) : Diagram β :=
  .withEnv env .empty

/-- Set the rightward (east) extent of the envelope. -/
def setEnvelopeRight (extent : Float) (d : Diagram β) : Diagram β :=
  let env := d.getEnvelope
  let adjusted : Envelope := fun v =>
    if v == Vec2.east then extent else env v
  .withEnv adjusted d

/-- Set the leftward (west) extent of the envelope. -/
def setEnvelopeLeft (extent : Float) (d : Diagram β) : Diagram β :=
  let env := d.getEnvelope
  let adjusted : Envelope := fun v =>
    if v == Vec2.west then extent else env v
  .withEnv adjusted d

/-- Set the upward (north) extent of the envelope. -/
def setEnvelopeTop (extent : Float) (d : Diagram β) : Diagram β :=
  let env := d.getEnvelope
  let adjusted : Envelope := fun v =>
    if v == Vec2.north then extent else env v
  .withEnv adjusted d

/-- Set the downward (south) extent of the envelope. -/
def setEnvelopeBottom (extent : Float) (d : Diagram β) : Diagram β :=
  let env := d.getEnvelope
  let adjusted : Envelope := fun v =>
    if v == Vec2.south then extent else env v
  .withEnv adjusted d

-- ═══════════════════════════════════════════════════════════════
-- Gap spacers
-- ═══════════════════════════════════════════════════════════════

/-- A horizontal spacer with the given width. -/
def hGap (width : Float) : Diagram β :=
  strut (Envelope.ofRect (width / 2) 0)

/-- A vertical spacer with the given height. -/
def vGap (height : Float) : Diagram β :=
  strut (Envelope.ofRect 0 (height / 2))

-- ═══════════════════════════════════════════════════════════════
-- Frame
-- ═══════════════════════════════════════════════════════════════

/--
Draws a stroked rectangle around the envelope of a diagram. The border is drawn behind the content.
-/
def frame (d : Diagram β) (stroke : Stroke := {})
    (padding : Float := 0) (cornerRadius : Float := 0) : Diagram β :=
  let env := d.getEnvelope
  let e := env Vec2.east + padding
  let w := env Vec2.west + padding
  let n := env Vec2.north + padding
  let s := env Vec2.south + padding
  let width := e + w
  let height := n + s
  let cx := (e - w) / 2
  let cy := (n - s) / 2
  let border :=
    if cornerRadius > 0 then
      Diagram.transform (Matrix.translate cx cy)
        (Diagram.fromStroke (PathData.roundedRect width height cornerRadius) stroke)
    else
      let tl : Vec2 := ⟨-w, n⟩
      let tr : Vec2 := ⟨e, n⟩
      let br : Vec2 := ⟨e, -s⟩
      let bl : Vec2 := ⟨-w, -s⟩
      Diagram.fromStroke
        (PathData.empty
          |>.moveTo tl
          |>.lineTo tr
          |>.lineTo br
          |>.lineTo bl
          |>.close)
        stroke
  let halfStroke := (stroke.width.getD 1.0) / 2
  Diagram.pad halfStroke (Diagram.compose border d)

/--
Draws a filled and stroked rectangle around the envelope of a diagram. The backdrop is drawn behind the content.
-/
def filledFrame (d : Diagram β) (fill : Fill := {}) (stroke : Stroke := {})
    (padding : Float := 0) (cornerRadius : Float := 0) : Diagram β :=
  let env := d.getEnvelope
  let e := env Vec2.east + padding
  let w := env Vec2.west + padding
  let n := env Vec2.north + padding
  let s := env Vec2.south + padding
  let width := e + w
  let height := n + s
  let cx := (e - w) / 2
  let cy := (n - s) / 2
  let border :=
    if cornerRadius > 0 then
      Diagram.transform (Matrix.translate cx cy)
        (Diagram.fromPath (PathData.roundedRect width height cornerRadius) fill stroke)
    else
      let tl : Vec2 := ⟨-w, n⟩
      let tr : Vec2 := ⟨e, n⟩
      let br : Vec2 := ⟨e, -s⟩
      let bl : Vec2 := ⟨-w, -s⟩
      Diagram.fromPath
        (PathData.empty
          |>.moveTo tl
          |>.lineTo tr
          |>.lineTo br
          |>.lineTo bl
          |>.close)
        fill stroke
  let halfStroke := (stroke.width.getD 1.0) / 2
  Diagram.pad halfStroke (Diagram.compose border d)

-- ═══════════════════════════════════════════════════════════════
-- Envelope visualization
-- ═══════════════════════════════════════════════════════════════

/--
Overlays a translucent polygon showing the diagram's envelope boundary.
Samples the envelope at `samples` evenly-spaced directions to build the polygon.
Useful for debugging layout and envelope behavior.
-/
def showEnvelope (d : Diagram β) (samples : Nat := 64)
    (color : Color := { r := 255, g := 153, b := 153 })
    (alpha : Float := 0.2) : Diagram β :=
  let env := d.getEnvelope
  let n := max samples 8
  let step := 2 * pi / n.toFloat
  -- Sample envelope directions and extents
  let dirs := List.range n |>.map fun i =>
    let θ := i.toFloat * step
    let dir : Vec2 := ⟨Float.cos θ, Float.sin θ⟩
    (dir, env dir)
  -- Compute boundary vertices as intersections of adjacent tangent lines.
  -- Each tangent line is { p | p · dᵢ = eᵢ }. The intersection of adjacent
  -- lines p · d₁ = e₁ and p · d₂ = e₂ gives a vertex of the convex shape.
  let vertices := dirs.mapIdx fun i (d1, e1) =>
    let (d2, e2) := match dirs[((i : Nat) + 1) % n]? with
      | some v => v
      | none => (d1, e1)
    -- Solve: d1.x * x + d1.y * y = e1, d2.x * x + d2.y * y = e2
    let det := d1.x * d2.y - d1.y * d2.x
    if det.abs < 1e-10 then
      -- Nearly parallel: fall back to polar point
      e1 • d1
    else
      ⟨(e1 * d2.y - e2 * d1.y) / det, (d1.x * e2 - d2.x * e1) / det⟩
  -- Build a closed polygon path
  let path := match vertices with
    | [] => PathData.empty
    | p :: rest =>
      let pd := PathData.empty |>.moveTo p
      let pd := rest.foldl (fun acc pt => acc.lineTo pt) pd
      pd.close
  let fillColor : Color := { color with a := alpha }
  let overlay : Diagram β := fromPath path
    (fill := { color := fillColor })
    (stroke := { width := (0 : Float) })
  Diagram.compose d overlay

/--
Overlays a small red X at the origin of the diagram's coordinate system.
Useful for debugging layout and positioning.
-/
def showOrigin (d : Diagram β) (size : Float := 5)
    (color : Color := Color.red) : Diagram β :=
  let stroke : Stroke := { color := color, width := (1.5 : Float) }
  let cross : Diagram β := Diagram.atop
    (Diagram.fromStroke (PathData.line ⟨-size, -size⟩ ⟨size, size⟩) stroke)
    (Diagram.fromStroke (PathData.line ⟨-size, size⟩ ⟨size, -size⟩) stroke)
  Diagram.atop d cross

-- ═══════════════════════════════════════════════════════════════
-- Aligned composition
-- ═══════════════════════════════════════════════════════════════

/-- A guide for alignment during composition. -/
inductive AlignGuide where
  /-- Aligns by a named anchor point. -/
  | anchor : Lean.Name → AlignGuide
  /-- Aligns by a fractional position (0 = bottom/left, 1 = top/right). -/
  | fraction : Float → AlignGuide

/--
Horizontal append with vertical alignment.
`.fraction 0` aligns bottoms, `.fraction 1` aligns tops,
`.fraction 0.5` aligns centers.

**Note:** `.anchor` alignment requires the name table from the layout pass and is
not yet implemented; it currently falls back to center alignment (`.fraction 0.5`).
-/
def hAppendAlign (guide : AlignGuide) (a b : Diagram β) : Diagram β :=
  match guide with
  | .fraction f =>
    let envA := a.getEnvelope
    let envB := b.getEnvelope
    -- Vertical extent
    let aBot := envA Vec2.south
    let aTop := envA Vec2.north
    let bBot := envB Vec2.south
    let bTop := envB Vec2.north
    -- The alignment point in each diagram's local coords
    -- fraction 0 = bottom, 1 = top
    let aY := -aBot + f * (aTop + aBot)  -- -aBot .. aTop
    let bY := -bBot + f * (bTop + bBot)
    let dy := aY - bY
    -- Horizontal placement: same as hAppend
    let dist := envA Vec2.east + envB Vec2.west
    .compose a (.transform (Matrix.translate dist dy) b)
  | .anchor _ =>
    -- Named anchor alignment requires the name table (Layer 5).
    -- For now, fall back to center alignment (same as fraction 0.5).
    let envA := a.getEnvelope
    let envB := b.getEnvelope
    let aBot := envA Vec2.south
    let aTop := envA Vec2.north
    let bBot := envB Vec2.south
    let bTop := envB Vec2.north
    let aY := -aBot + 0.5 * (aTop + aBot)
    let bY := -bBot + 0.5 * (bTop + bBot)
    let dy := aY - bY
    let dist := envA Vec2.east + envB Vec2.west
    .compose a (.transform (Matrix.translate dist dy) b)

-- ═══════════════════════════════════════════════════════════════
-- Convenience transforms
-- ═══════════════════════════════════════════════════════════════

/-- Scales a diagram uniformly by the given factor. -/
def scale (s : Float) (d : Diagram β) : Diagram β :=
  .transform (Matrix.uniformScale s) d

/-- Scales a diagram non-uniformly by separate x and y factors. -/
def scaleXY (sx sy : Float) (d : Diagram β) : Diagram β :=
  .transform (Matrix.scale sx sy) d

/-- Rotates a diagram counter-clockwise by `θ` radians. -/
def rotate (θ : Float) (d : Diagram β) : Diagram β :=
  .transform (Matrix.rotate θ) d

/-- Flips a diagram horizontally (mirrors across the y-axis). -/
def hflip (d : Diagram β) : Diagram β :=
  .transform (Matrix.scale (-1) 1) d

/-- Flips a diagram vertically (mirrors across the x-axis). -/
def vflip (d : Diagram β) : Diagram β :=
  .transform (Matrix.scale 1 (-1)) d

-- ═══════════════════════════════════════════════════════════════
-- Ghost and refocus
-- ═══════════════════════════════════════════════════════════════

/--
Contributes the diagram's envelope to layout but renders nothing (invisible spacer). The dual of
`floating`, which renders but has zero envelope.
-/
def ghost (d : Diagram β) : Diagram β :=
  strut d.getEnvelope

/-- Uses the envelope of `sub` for the combined diagram `d`. -/
def refocus (sub : Diagram β) (d : Diagram β) : Diagram β :=
  .withEnv sub.getEnvelope d

/-- Clips a sub-diagram to a circular region of the given radius. -/
def clipCircle (radius : Float) (d : Diagram β) : Diagram β :=
  .clip (PathData.circle radius) d

/-- Clips a sub-diagram to a rectangular region of the given dimensions. -/
def clipRect (width height : Float) (d : Diagram β) : Diagram β :=
  .clip (PathData.rect width height) d

/-- Places `overlay` at the position of a named anchor in `d`, on top of `d`. -/
def pinOver (anchorName : Lean.Name) (overlay : Diagram β) (d : Diagram β) : Diagram β :=
  .compose d (.deferred anchorName fun (_rc : ResolvedConfig) pos =>
    .transform (Matrix.translate pos.x pos.y) overlay)

/-- Places `underlay` at the position of a named anchor in `d`, beneath `d`. -/
def pinUnder (anchorName : Lean.Name) (underlay : Diagram β) (d : Diagram β) : Diagram β :=
  .compose (.deferred anchorName fun (_rc : ResolvedConfig) pos =>
    .transform (Matrix.translate pos.x pos.y) underlay) d

/-- Places a curly brace spanning the full width of a diagram below it, with a label. -/
def braceBelow (d : Diagram β) (label : Diagram β)
    (gap : Float := 3) (depth : Float := 8) : Diagram β :=
  vsep gap [d, curlyBrace d.width (depth := depth) (label := some label)]
