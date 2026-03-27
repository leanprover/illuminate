/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/

import Lean.Elab.Term
import Illuminate.Geometry.Basic
import Illuminate.Geometry.Vec2
import Illuminate.Geometry.Envelope

namespace Illuminate

/-- An RGBA color with 8-bit channels and a floating-point alpha. -/
structure Color where
  /-- Red channel (0–255). -/
  r : UInt8
  /-- Green channel (0–255). -/
  g : UInt8
  /-- Blue channel (0–255). -/
  b : UInt8
  /-- Alpha (opacity), from 0.0 (transparent) to 1.0 (opaque). -/
  a : Float := 1.0
deriving Repr, BEq, Inhabited, Hashable

namespace Color

/-- Opaque black. -/
def black : Color := { r := 0, g := 0, b := 0 }
/-- Opaque white. -/
def white : Color := { r := 255, g := 255, b := 255 }
/-- Opaque red. -/
def red : Color := { r := 255, g := 0, b := 0 }
/-- Opaque green. -/
def green : Color := { r := 0, g := 128, b := 0 }
/-- Opaque blue. -/
def blue : Color := { r := 0, g := 0, b := 255 }
/-- Light gray. -/
def lightGray : Color := { r := 211, g := 211, b := 211 }
/-- Fully transparent black. -/
def transparent : Color := { r := 0, g := 0, b := 0, a := 0.0 }

/-- Creates an opaque color from red, green, and blue channel values (0–255). -/
def rgb (r g b : UInt8) : Color := { r, g, b }

/-- Creates a color from red, green, blue, and alpha values. Channels are 0–255, alpha is 0.0–1.0. -/
def rgba (r g b : UInt8) (a : Float) : Color := { r, g, b, a }

end Color

private def hexVal (c : Char) : Option Nat :=
  if '0' ≤ c ∧ c ≤ '9' then some (c.toNat - '0'.toNat)
  else if 'a' ≤ c ∧ c ≤ 'f' then some (c.toNat - 'a'.toNat + 10)
  else if 'A' ≤ c ∧ c ≤ 'F' then some (c.toNat - 'A'.toNat + 10)
  else none

private def parseHexPair (s : String.Slice) : Option UInt8 := do
  let p := s.startPos
  if h : p = s.endPos then none
  else
    let c1 := p.get h
    let p := p.next h
    if h : p = s.endPos then none
    else
      let c2 := p.get h
      let h ← hexVal c1
      let l ← hexVal c2
      return (h * 16 + l).toUInt8

/-- Parses a hex color string like {lit}`"#ffcc93"` or {lit}`"#ffcc9380"` into RGBA channel values. -/
def parseRgbHex (s : String) : Except String (UInt8 × UInt8 × UInt8 × Option UInt8) := do
  let len := if s.startsWith "#" then s.length - 1 else s.length
  let chars := s.dropPrefix "#"
  unless len == 6 || len == 8 do
    throw s!"expected 6 or 8 hex digits, got '{s}'"
  let r := chars.take 2
  let g := chars.drop 2 |>.take 2
  let b := chars.drop 4 |>.take 2
  let some r := parseHexPair r | throw "invalid hex digit in red channel"
  let some g := parseHexPair g | throw "invalid hex digit in green channel"
  let some b := parseHexPair b | throw "invalid hex digit in blue channel"
  if len == 8 then
    let a := chars.drop 6 |>.take 2
    let some a := parseHexPair a | throw "invalid hex digit in alpha channel"
    return (r, g, b, some a)
  else
    return (r, g, b, none)

/-- Builds a {name}`Color` syntax literal from parsed RGBA channels. -/
def rgbHexToSyntax (r g b : UInt8) (a? : Option UInt8) : Lean.MacroM Lean.Syntax := do
  let rLit := Lean.Syntax.mkNumLit (toString r.toNat)
  let gLit := Lean.Syntax.mkNumLit (toString g.toNat)
  let bLit := Lean.Syntax.mkNumLit (toString b.toNat)
  if let some a := a? then
    let aFloat := a.toFloat / 255.0
    let aStr := toString (Float.round (aFloat * 10000) / 10000)
    let aLit := Lean.Syntax.mkScientificLit aStr
    `({ r := $rLit, g := $gLit, b := $bLit, a := $aLit : Color })
  else
    `({ r := $rLit, g := $gLit, b := $bLit : Color })

/-- Parses a hex color literal like `rgb!"#ffcc93"` or `rgb!"#ffcc9380"` (with alpha). -/
scoped syntax (name := rgbLit) "rgb!" str : term

open Lean Elab Term in
elab_rules : term
  | `(rgb! $s:str) => do
    let str := s.getString
    let (r, g, b, a?) ← match parseRgbHex str with
      | .ok v => pure v
      | .error msg => throwErrorAt s msg
    let result ← liftMacroM <| rgbHexToSyntax r g b a?
    elabTerm result none

/-- Specifies the color of a solid fill. -/
structure FillSpec where
  /-- Fill color. -/
  color : Color := Color.lightGray
deriving Repr, BEq, Hashable

instance : Inhabited FillSpec := ⟨{ color := Color.lightGray }⟩

/-- A color stop in a gradient, positioned at a fractional offset along the gradient axis. -/
structure GradientStop where
  /-- Fractional position along the gradient (0.0 = start, 1.0 = end). -/
  offset : Float
  /-- Color at this stop. -/
  color : Color
deriving Inhabited, Repr, BEq, Hashable

/-- Controls how a gradient extends beyond its defined region. -/
inductive SpreadMethod where
  /-- Extends the terminal colors beyond the gradient bounds. -/
  | pad
  /-- Mirrors the gradient repeatedly. -/
  | reflect
  /-- Tiles the gradient repeatedly. -/
  | repeat
deriving Inhabited, Repr, BEq, Inhabited, Hashable

/--
A gradient fill specification with coordinates in diagram-local space.

Gradient coordinates are absolute in the diagram's local coordinate system and transform
with the diagram when affine transforms (translate, rotate, scale) are applied.
-/
inductive Gradient where
  /-- Linear gradient between two points. -/
  | linear (x1 y1 x2 y2 : Float) (stops : Array GradientStop)
      (spread : SpreadMethod := .pad)
  /--
  Radial gradient between two circles (SVG/Cairo two-circle model).

  The gradient radiates from the focal circle ({name}`fx`, {name}`fy`, {name}`fr`)
  to the outer circle ({name}`cx`, {name}`cy`, {name}`r`).
  -/
  | radial (cx cy r : Float) (fx fy fr : Float)
      (stops : Array GradientStop) (spread : SpreadMethod := .pad)
deriving Inhabited, Repr, BEq, Hashable

namespace Gradient

/-- Creates a vertical linear gradient spanning the given height, centered at the origin. -/
def vertical (height : Float) (stops : Array GradientStop)
    (spread : SpreadMethod := .pad) : Gradient :=
  .linear 0 (height / 2) 0 (-height / 2) stops spread

/-- Creates a horizontal linear gradient spanning the given width, centered at the origin. -/
def horizontal (width : Float) (stops : Array GradientStop)
    (spread : SpreadMethod := .pad) : Gradient :=
  .linear (-width / 2) 0 (width / 2) 0 stops spread

/-- Creates a centered radial gradient with the given radius. -/
def radialSymmetric (radius : Float) (stops : Array GradientStop)
    (spread : SpreadMethod := .pad) : Gradient :=
  .radial 0 0 radius 0 0 0 stops spread

end Gradient

/--
Resolved fill style for closed paths, with absolute gradient coordinates.

Used internally by the core primitive and draw command types. User-facing code should
use the {lit}`Fill` type instead, which resolves relative gradients against the shape's envelope.
-/
inductive ResolvedFill where
  /-- No fill — the interior is not rendered and not hittable. -/
  | none
  /-- Solid color fill — the interior is rendered and hittable, even if the color is fully transparent. -/
  | solid : FillSpec → ResolvedFill
  /-- Gradient fill with absolute coordinates — the interior is rendered and hittable. -/
  | gradient : Gradient → ResolvedFill
deriving Repr, BEq, Hashable

instance : Inhabited ResolvedFill := ⟨.solid default⟩

instance : Coe Color FillSpec := ⟨FillSpec.mk⟩

instance : Coe FillSpec ResolvedFill := ⟨.solid⟩

instance : Coe Gradient ResolvedFill := ⟨.gradient⟩

/--
User-facing fill style for closed paths.

Gradient variants specify direction and stops relative to the shape; absolute
coordinates are computed automatically from the shape's envelope when the fill
is resolved automatically by shape constructors.
-/
inductive Fill where
  /-- No fill — the interior is not rendered and not hittable. -/
  | none
  /-- Solid color fill. -/
  | solid : FillSpec → Fill
  /-- Linear gradient along a direction, sized to the shape's envelope. -/
  | linearGradient (dir : Vec2) (stops : Array GradientStop)
      (spread : SpreadMethod := .pad) : Fill
  /-- Radial gradient sized to the shape's inscribed circle. -/
  | radialGradient (stops : Array GradientStop) (center : Vec2 := Vec2.zero)
      (focal : Vec2 := Vec2.zero) (spread : SpreadMethod := .pad) : Fill
  /-- Pre-resolved fill with absolute gradient coordinates. -/
  | resolved : ResolvedFill → Fill
deriving Repr, BEq, Hashable

instance : Inhabited Fill := ⟨.solid default⟩

instance : Coe FillSpec Fill := ⟨.solid⟩

instance : Coe Color Fill := ⟨fun c => .solid c⟩

namespace Fill

/-- Creates a two-color linear gradient along the given direction. -/
def between (c1 c2 : Color) (dir : Vec2 := Vec2.north)
    (spread : SpreadMethod := .pad) : Fill :=
  .linearGradient dir #[{ offset := 0, color := c1 }, { offset := 1, color := c2 }] spread

/-- Creates a horizontal linear gradient (left to right). -/
def horizontalGradient (stops : Array GradientStop)
    (spread : SpreadMethod := .pad) : Fill :=
  .linearGradient Vec2.east stops spread

/-- Creates a vertical linear gradient (bottom to top). -/
def verticalGradient (stops : Array GradientStop)
    (spread : SpreadMethod := .pad) : Fill :=
  .linearGradient Vec2.north stops spread

/-- Creates a two-color radial gradient from inner to outer color. -/
def radialBetween (inner outer : Color) (center : Vec2 := Vec2.zero)
    (spread : SpreadMethod := .pad) : Fill :=
  .radialGradient #[{ offset := 0, color := inner }, { offset := 1, color := outer }]
    center Vec2.zero spread

/--
Resolves a user-facing fill against a shape's envelope, producing a {name}`ResolvedFill`
with absolute gradient coordinates.
-/
def resolve (env : Envelope) (f : Fill) : ResolvedFill :=
  match f with
  | .none => .none
  | .solid fs => .solid fs
  | .resolved rf => rf
  | .linearGradient dir stops spread =>
    let d := Vec2.normalize dir
    let fwd := env[d].diag
    let bwd := env[Vec2.neg d].diag
    let x1 := -bwd * d.x
    let y1 := -bwd * d.y
    let x2 := fwd * d.x
    let y2 := fwd * d.y
    .gradient (.linear x1 y1 x2 y2 stops spread)
  | .radialGradient stops center focal spread =>
    -- Compute the envelope's centroid as the midpoint of opposing extents
    let e := env[Vec2.east].diag
    let w := env[Vec2.west].diag
    let n := env[Vec2.north].diag
    let s := env[Vec2.south].diag
    let cx := center.x + (e - w) / 2
    let cy := center.y + (n - s) / 2
    let fx := focal.x + (e - w) / 2
    let fy := focal.y + (n - s) / 2
    -- Radius from centroid: max distance to any envelope boundary point
    let dirs := [Vec2.north, Vec2.northeast, Vec2.east, Vec2.southeast,
                 Vec2.south, Vec2.southwest, Vec2.west, Vec2.northwest]
    let centroid := Vec2.mk ((e - w) / 2) ((n - s) / 2)
    let radius := dirs.foldl (init := (0 : Float)) fun acc d =>
      let extent := env[d].diag
      let bx := extent * d.x
      let by_ := extent * d.y
      let dist := ((bx - centroid.x) * (bx - centroid.x) +
                   (by_ - centroid.y) * (by_ - centroid.y)).sqrt
      max acc dist
    .gradient (.radial cx cy radius fx fy 0 stops spread)

end Fill
