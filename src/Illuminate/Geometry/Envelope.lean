/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/

import Illuminate.Geometry.Length
import Illuminate.Geometry.Vec2
import Illuminate.Geometry.Matrix


namespace Illuminate

/--
An envelope maps a unit direction vector to a {name}`Length` extent of a shape
in that direction. Given direction $`v`, $`envelope(v)` returns a {name}`Length`
whose diagram-unit component gives the extent for layout and whose pixel component
gives the scale-invariant extent resolved at render time.
-/
inductive Envelope where
  /-- The empty envelope takes up no space. -/
  | empty
  /-- A nonempty envelope. -/
  | nonempty (env : Vec2 → Length)

instance : Hashable Envelope where
  hash
    | .empty => 0
    | .nonempty env =>
      -- The envelope is sampled in a few directions so the hash can
      -- distinguish more of them
      let sample (v : Vec2) : UInt64 := hash (env v)
      mixHash (mixHash (mixHash (sample .east) (sample .north))
                       (mixHash (sample .west) (sample .south)))
              (mixHash (sample ⟨1, 1⟩) (sample ⟨1, -1⟩))

instance : Coe (Vec2 → Length) Envelope where
  coe := .nonempty

instance : Coe (Vec2 → Float) Envelope where
  coe f := .nonempty (Length.ofDiag ∘ f)

instance : GetElem? Envelope Vec2 Length (fun _ _ => True) where
  getElem
    | .empty, _, _ => Length.zero
    | .nonempty env, v, _ => env v
  getElem?
    | .empty, _ => none
    | .nonempty env, v => some (env v)


namespace Envelope

/--
If {name}`env` is empty, the result is empty. Otherwise, the result is {name}`f` applied to the envelope
function.
-/
def modify (f : (Vec2 → Length) → (Vec2 → Length)) : (env : Envelope) → Envelope
  | .empty => .empty
  | .nonempty g => .nonempty (f g)

/-- Combines two envelopes with the provided function. -/
def combineWith (env1 env2 : Envelope) (f : (Vec2 → Length) → (Vec2 → Length) → (Vec2 → Length)) : Envelope :=
  match env1, env2 with
  | .empty, e2 => e2
  | e1, .empty => e1
  | .nonempty g, .nonempty h => .nonempty (f g h)

/--
Envelope of an axis-aligned rectangle centered at the origin,
with the given half-width and half-height.
-/
def ofRect (halfWidth halfHeight : Float) : Envelope := .nonempty fun v =>
  -- For an axis-aligned rect centered at origin with corners at (±hw, ±hh),
  -- the envelope in direction v is max over corners of (corner · v).
  -- For a unit vector v, this equals |v.x| * hw + |v.y| * hh.
  .ofDiag (v.x.abs * halfWidth + v.y.abs * halfHeight)

/--
Envelope of an axis-aligned rectangle specified by full width and height,
centered at the origin.
-/
def ofSize (width height : Float) : Envelope :=
  ofRect (width / 2) (height / 2)

/-- Envelope of a circle centered at the origin with the given radius. -/
def ofCircle (radius : Float) : Envelope := .nonempty fun _ => .ofDiag radius

/--
Transform an envelope by a matrix. The envelope of a transformed shape
in direction $`v` equals the original envelope queried at the inverse-
transformed direction, scaled appropriately.

For pure rotations: $`envelope'(v) = envelope(rotate(-θ) v)`.
For uniform scale $`s`: $`envelope'(v) = s · envelope(v)`.
For general affine transforms, we use the adjugate (transpose of cofactor matrix)
to transform the direction, then correct for the scaling.

Only the diagram-unit component is transformed; the pixel component is
preserved unchanged since screen pixels do not scale with diagram transforms.
-/
def transform (m : Matrix) (env : Envelope) : Envelope :=
  env.modify fun env' v =>
    -- We need to find max over all points p in the shape of (m·p) · v.
    -- (m·p) · v = p · (mᵀ · v) (for the linear part) + translation · v
    -- So envelope'(v) = env(normalize(mᵀ v)) · |mᵀ v| + t · v
    let mtv := Vec2.mk (m.a * v.x + m.c * v.y) (m.b * v.x + m.d * v.y)
    let mtvLen := mtv.length
    if nearZero mtvLen then Length.zero
    else
      let dir := (1 / mtvLen) • mtv
      let translationContrib := m.tx * v.x + m.ty * v.y
      let inner := env' dir
      -- Transform only the diag component; px stays unchanged
      ⟨mtvLen * inner.diag + translationContrib, inner.px⟩

/-- Union of two envelopes: the tightest envelope containing both shapes. -/
def union (e1 e2 : Envelope) : Envelope :=
  e1.combineWith e2 fun e1' e2' v =>
    Max.max (e1' v) (e2' v)

instance : Union Envelope where
  union := union

/--
Translate an envelope by a displacement vector.
$`envelope'(v) = envelope(v) + d · v`

Only the diagram-unit component is translated; the pixel component is unchanged.
-/
def translateBy (d : Vec2) (env : Envelope) : Envelope := env.modify fun env' v =>
  let inner := env' v
  ⟨inner.diag + d.dot v, inner.px⟩

/-- Envelope of an axis-aligned bounding box with corners {name}`lo` and {name}`hi`. -/
def ofBounds (lo hi : Vec2) : Envelope := .nonempty fun v =>
  let x := if v.x ≥ 0 then hi.x else lo.x
  let y := if v.y ≥ 0 then hi.y else lo.y
  .ofDiag (x * v.x + y * v.y)

/-- Envelope of a convex hull of vertices: $`max(v · pᵢ)` over all points. -/
def ofVertices (pts : List Vec2) : Envelope :=
  if pts.isEmpty then .empty
  else
    .nonempty fun v =>
      .ofDiag (pts.foldl (init := 0) fun acc p =>
        Max.max acc (p.dot v))

/-- Queries the extent in a given direction. -/
def query (env : Envelope) (direction : Vec2) : Option Length := env[direction]

end Envelope

/-- Cardinal anchor points derived from an envelope. -/
structure CardinalAnchors where
  /-- Topmost anchor point. -/
  north : Vec2
  /-- Bottommost anchor point. -/
  south : Vec2
  /-- Rightmost anchor point. -/
  east : Vec2
  /-- Leftmost anchor point. -/
  west : Vec2
  /-- Upper-right anchor point. -/
  northeast : Vec2
  /-- Upper-left anchor point. -/
  northwest : Vec2
  /-- Lower-right anchor point. -/
  southeast : Vec2
  /-- Lower-left anchor point. -/
  southwest : Vec2
  /-- Center anchor point. -/
  center : Vec2
deriving Repr

namespace CardinalAnchors

/--
Computes cardinal anchor points from an envelope.

Uses the diagram-unit component of each extent for anchor positions.
-/
def fromEnvelope (env : Envelope) : Option CardinalAnchors :=
  match env with
  | .empty => none
  | .nonempty env =>
    let n := (env Vec2.north).diag
    let s := (env Vec2.south).diag
    let e := (env Vec2.east).diag
    let w := (env Vec2.west).diag
    let ne := (env Vec2.northeast).diag
    let nw := (env Vec2.northwest).diag
    let se := (env Vec2.southeast).diag
    let sw := (env Vec2.southwest).diag
    let northPt : Vec2 := ⟨0, n⟩
    let southPt : Vec2 := ⟨0, -s⟩
    let eastPt : Vec2 := ⟨e, 0⟩
    let westPt : Vec2 := ⟨-w, 0⟩
    let centerX := (e - w) / 2
    let centerY := (n - s) / 2
    some {
      north := northPt
      south := southPt
      east := eastPt
      west := westPt
      northeast := ne • Vec2.northeast
      northwest := nw • Vec2.northwest
      southeast := se • Vec2.southeast
      southwest := sw • Vec2.southwest
      center := ⟨centerX, centerY⟩
    }
