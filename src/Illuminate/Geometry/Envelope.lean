import Illuminate.Geometry.Vec2
import Illuminate.Geometry.Matrix


namespace Illuminate

/--
An envelope maps a unit direction vector to a scalar extent of a shape
in that direction. Given direction `v`, `envelope v` returns the scalar `t`
such that the shape fits within the half-plane `{ p | p · v ≤ t }`.
-/
def Envelope := Vec2 → Float

namespace Envelope

/-- The empty envelope — zero extent in every direction. -/
def empty : Envelope := fun _ => 0

/--
Envelope of an axis-aligned rectangle centered at the origin,
with the given half-width and half-height.
-/
def ofRect (halfWidth halfHeight : Float) : Envelope := fun v =>
  -- For an axis-aligned rect centered at origin with corners at (±hw, ±hh),
  -- the envelope in direction v is max over corners of (corner · v).
  -- For a unit vector v, this equals |v.x| * hw + |v.y| * hh.
  v.x.abs * halfWidth + v.y.abs * halfHeight

/--
Envelope of an axis-aligned rectangle specified by full width and height,
centered at the origin.
-/
def ofSize (width height : Float) : Envelope :=
  ofRect (width / 2) (height / 2)

/-- Envelope of a circle centered at the origin with the given radius. -/
def ofCircle (radius : Float) : Envelope := fun _ => radius

/--
Transform an envelope by a matrix. The envelope of a transformed shape
in direction `v` equals the original envelope queried at the inverse-
transformed direction, scaled appropriately.

For pure rotations: `envelope'(v) = envelope(rotate(-θ) v)`.
For uniform scale `s`: `envelope'(v) = s · envelope(v)`.
For general affine transforms, we use the adjugate (transpose of cofactor matrix)
to transform the direction, then correct for the scaling.
-/
def transform (m : Matrix) (env : Envelope) : Envelope := fun v =>
  -- We need to find max over all points p in the shape of (m·p) · v.
  -- (m·p) · v = p · (mᵀ · v) (for the linear part) + translation · v
  -- So envelope'(v) = env(normalize(mᵀ v)) · |mᵀ v| + t · v
  let mtv := Vec2.mk (m.a * v.x + m.c * v.y) (m.b * v.x + m.d * v.y)
  let mtvLen := mtv.length
  if nearZero mtvLen then 0
  else
    let dir := (1 / mtvLen) • mtv
    let translationContrib := m.tx * v.x + m.ty * v.y
    mtvLen * env dir + translationContrib

/-- Union of two envelopes: the tightest envelope containing both shapes. -/
def union (e1 e2 : Envelope) : Envelope := fun v =>
  Max.max (e1 v) (e2 v)

/--
Translate an envelope by a displacement vector.
`envelope'(v) = envelope(v) + d · v`
-/
def translateBy (d : Vec2) (env : Envelope) : Envelope := fun v =>
  env v + d.dot v

/-- Envelope of an axis-aligned bounding box with corners `lo` and `hi`. -/
def ofBounds (lo hi : Vec2) : Envelope := fun v =>
  let x := if v.x ≥ 0 then hi.x else lo.x
  let y := if v.y ≥ 0 then hi.y else lo.y
  x * v.x + y * v.y

/-- Envelope of a convex hull of vertices: `max(v · pᵢ)` over all points. -/
def ofVertices (pts : List Vec2) : Envelope := fun v =>
  pts.foldl (fun acc p => Max.max acc (p.dot v)) 0

/-- Query the extent in a given direction. -/
def query (env : Envelope) (direction : Vec2) : Float := env direction

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

/-- Computes cardinal anchor points from an envelope. -/
def fromEnvelope (env : Envelope) : CardinalAnchors :=
  let n := env Vec2.north
  let s := env Vec2.south
  let e := env Vec2.east
  let w := env Vec2.west
  let ne := env Vec2.northeast
  let nw := env Vec2.northwest
  let se := env Vec2.southeast
  let sw := env Vec2.southwest
  let northPt : Vec2 := ⟨0, n⟩
  let southPt : Vec2 := ⟨0, -s⟩
  let eastPt : Vec2 := ⟨e, 0⟩
  let westPt : Vec2 := ⟨-w, 0⟩
  let centerX := (e - w) / 2
  let centerY := (n - s) / 2
  { north := northPt
    south := southPt
    east := eastPt
    west := westPt
    northeast := ne • Vec2.northeast
    northwest := nw • Vec2.northwest
    southeast := se • Vec2.southeast
    southwest := sw • Vec2.southwest
    center := ⟨centerX, centerY⟩ }

