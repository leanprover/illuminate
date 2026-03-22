# Illuminate

Illuminate is a diagramming library for Lean. It provides a
compositional approach to constructing two-dimensional diagrams,
rendering them to SVG, and previewing them interactively in the Lean
InfoView.

**This library is experimental and subject to rapid change.** The API
is under active development. Types, function signatures, and module
structure may change without notice between versions.

## Core Concepts

### Composing Diagrams

Diagrams are built by combining smaller diagrams. Primitive shapes,
text, and images serve as the basic building blocks; composition
operators such as `hcat`, `beside`, and `pinOver` assemble them into
larger structures. Transformations, styling, and naming are applied to
any diagram regardless of how it was constructed.

The type `Diagram β` is parameterized by a foreign primitive type `β`,
which allows backends to embed their own rendering objects alongside
the built-in primitives. Pure geometric diagrams use `Empty` for this
parameter.

### Envelopes, Not Bounding Boxes

Most diagram libraries represent the spatial extent of a shape as an
axis-aligned bounding box. Illuminate uses _envelopes_ instead. An
envelope is a function from a unit direction vector to a scalar
extent:

```lean
def Envelope := Vec2 → Float
```

Given a direction `v`, an envelope returns the largest value `t` such
that the shape fits within the half-plane `{ p | p · v ≤ t }`. A
circle of radius 5 has the constant envelope `fun _ => 5`. A rectangle
has the envelope `fun v => |v.x| * halfWidth + |v.y| * halfHeight`.

This representation is more expressive than a bounding box. When two
shapes are placed side by side, the library queries each envelope in
the direction of composition to compute the exact displacement,
producing tighter layouts for nonrectangular shapes. Envelopes compose
naturally: the union of two envelopes is their pointwise maximum, and
affine transformations act on envelopes through the adjugate matrix.

### Names and Anchors

Any subdiagram can be given a hierarchical name with `Diagram.named`.
A named diagram automatically receives cardinal anchor points (north,
south, east, west, and the four intermediates) derived from its
envelope. Other parts of the diagram can refer to these anchors by
qualified name. The `connect` function, for instance, draws an arrow
between two named anchors, and `pinOver` places an overlay at a named
anchor's position.

### Cascading Configuration

Style properties such as stroke width, fill color, font size, and
arrowhead type propagate downward through the diagram via a cascading
configuration system. Each property is independently set to one of
three states: _inherit_ from the parent, _reset_ to the global
default, or _set_ to an explicit value.

Wrapping a subdiagram with `withFillColor Color.blue` sets the fill
for that subtree. Children inherit the setting unless they override
it. Resetting allows a subtree to escape an ancestor's styling and
return to the global default. This design means that reusable diagram
components can be inserted into any context without inheriting
unwanted styles.

### Deferred Layout

The final positions of named anchors are not known until the entire
diagram has been assembled. Operations that depend on anchor
positions, such as `connect` and `pinOver`, insert _deferred_ nodes
into the diagram. During rendering, the layout engine resolves these
nodes through fixed-point iteration: it repeatedly collects anchor
positions and evaluates deferred callbacks until all positions
converge. This means that authors can connect and annotate named
subdiagrams without manually computing coordinates.

### Validation

Every diagram renders to _something_. There are no partial failures or
exceptions during rendering. However, certain structural issues cause
degraded behavior: duplicate names make anchor resolution ambiguous,
references to missing names leave deferred nodes unresolved, and empty
paths produce invisible geometry. The resulting output in these cases
is unpredictable and unlikely to be what the author intended.

The `validate` function checks a diagram for these issues before
rendering, returning a list of errors such as duplicate names, empty
paths, and missing fallbacks for foreign primitives. Warnings embedded
in the diagram can be collected separately with `collectWarnings`.

## Building Diagrams

### Shapes

Illuminate provides a library of common shapes, each producing a
diagram with an appropriate envelope:

- `Diagram.rect` and `Diagram.roundedRect` produce axis-aligned
  rectangles.
- `Diagram.circle` and `Diagram.ellipse` produce circular and
  elliptical shapes.
- `Diagram.polygon` produces a regular polygon with a given number of
  sides.
- `Diagram.star` produces a star with alternating outer and inner
  vertices.
- `Diagram.text` produces a text label.
- `Diagram.line` produces a straight line segment.

All shapes are centered at the origin.

### Spatial Composition

The primary composition operations place diagrams adjacent to one
another, using envelopes to compute the necessary displacement:

- `Diagram.beside` places one diagram next to another in an arbitrary
  direction, with an optional gap.
- `Diagram.hcat` and `Diagram.vcat` concatenate a list of diagrams
  horizontally or vertically.
- `Diagram.hsep` and `Diagram.vsep` do the same with uniform spacing
  between elements.
- `Diagram.grid` lays out a two-dimensional array of diagrams with
  uniform cell sizes and configurable spacing.

Each of these accepts an optional alignment parameter controlling
cross-axis positioning.

### Overlay and Deferred Positioning

Two diagrams can be overlaid at the same origin with
`Diagram.compose`. For positioning relative to named anchors,
`Diagram.pinOver` and `Diagram.pinUnder` place an overlay or underlay
at the position of a named anchor within another diagram. Because
anchor positions are not known until layout, these operations are
resolved during the layout pass.

### Transforms and Modifiers

Standard affine transformations are available through `Diagram.scale`,
`Diagram.scaleXY`, `Diagram.rotate` (counterclockwise, in radians),
`Diagram.hflip`, and `Diagram.vflip`.

Padding operations expand the envelope without changing the visual
content. `Diagram.pad` adds uniform padding on all sides,
`Diagram.padXY` adds horizontal and vertical padding independently,
and `Diagram.padLRTB` controls each side separately.

The `frame` function draws a stroked rectangle around a diagram's
envelope, with optional corner rounding and padding.

### Envelope Manipulation

Several operations adjust a diagram's envelope without changing its
appearance:

- `floating` renders the diagram but contributes zero extent to
  layout, making it invisible to surrounding composition.
- `ghost` contributes the diagram's envelope to layout but renders
  nothing.
- `refocus` uses one subdiagram's envelope for a combined diagram.
- `strut` creates an invisible spacer with a given envelope.
- `hGap` and `vGap` create invisible spacers of a given width or
  height.

### Arrows and Connections

`Diagram.connect` draws a curved arrow between two named anchor
points. Each endpoint is described by a `LineEnd`, which specifies the
target anchor name, an optional departure or arrival angle, a pull
factor controlling the curvature of the Bezier control points, and an
optional arrowhead. Four arrowhead types are available: `latex` (open
two-line), `stealth` (filled), `triangle`, and `circle`.

### Style Helpers

Convenience functions such as `withFillColor`, `withStrokeColor`,
`withStrokeWidth`, `withFontSize`, `withFontFamily`, `withTextColor`,
and `withArrowhead` wrap a subdiagram in a configuration override.
Each has a corresponding `reset` variant (e.g., `resetFillColor`) that
reverts the property to the global default for that subtree.

## Domain-Specific Languages

As a demonstration of one way to use Illuminate, two domain-specific
diagram languages are included. These are in their infancy and are
probably not suitable for real use yet.

### Commutative Diagrams

The `CommDiag` module provides a monadic DSL for building commutative
diagrams of the kind used in algebra and category theory:

```lean
def mySquare : Diagram Empty :=
  commDiag do
    let a ← CommDiagM.node "A"
    let b ← CommDiagM.node "B"
    let c ← CommDiagM.node "C"
    let d ← CommDiagM.node "D"
    CommDiagM.grid #[#[some a, some b], #[some c, some d]]
    CommDiagM.arrowWith a b { label := some "f" }
    CommDiagM.arrowWith a c { label := some "g", side := .left }
    CommDiagM.arrowWith b d { label := some "h" }
    CommDiagM.arrowWith c d { label := some "k", side := .below }
```

Nodes are created with labels, arranged in a grid, and connected by
morphism arrows with optional labels and curvature.

### State Diagrams

The `StateDiagram` module builds DFA and NFA state diagrams with
configurable radius, spacing, colors, and font sizes. States are laid
out on a horizontal line and connected by straight or curved edges
with labeled transitions.

## Interactive Previews

### The `#diagram` Command

The `#diagram` command renders a diagram inline in the Lean 4
infoview. Hover over a `#diagram` line to see a live SVG preview:

```lean
#diagram Diagram.circle 30
```

### Parameterized Diagrams

Diagram functions can accept interactive parameters using gadget
types. The infoview renders appropriate controls (sliders, text
inputs, checkboxes) and re-evaluates the diagram when the user adjusts
them:

```lean
#diagram fun (n : Slider "Points" 3 12) (r : Slider "Radius" 10 50) =>
  Diagram.star n.toNat r (r * 0.4)
```

The `Slider` type produces a floating-point slider with a label,
minimum, and maximum. `TextInput` and `Checkbox` provide string and
boolean inputs, respectively. Each gadget type reduces to its
underlying value type (`Float`, `String`, or `Bool`), so the function
body uses the parameter as an ordinary value.

### The `#animate` Command

The `#animate` command builds a step-based animation and plays it in
the infoview with a play/pause button and scrub bar:

```lean
#animate
  [{ duration := 1.5 }, { duration := 2.0 }]
  (fun progress =>
    let t := Easing.easeInOut progress[0]
    let radius := Interpolate.interpolate 10.0 50.0 t
    Diagram.circle radius (fill := .solid { color := Color.red }))
```

Each step has a duration in seconds and optional flags:
`pause := true` stops playback until the user clicks, and
`loop := true` repeats the step continuously. The render function
receives a vector of per-step progress values in `[0, 1]`. Built-in
easing functions (`easeIn`, `easeOut`, `easeInOut`, `sineInOut`,
`backOut`) and interpolation (`Interpolate.interpolate` for `Float`,
`Vec2`, `Color`, `Matrix`) produce smooth transitions. Helper effects
like `fadeIn`, `fadeOut`, `crossFade`, `slide`, `animScale`, and
`animRotate` compose common animation patterns.

Compiled animations can also be rendered to standalone HTML files
(`CompiledAnimation.renderHTML`) or embedded in reveal.js
presentations (`CompiledAnimation.renderRevealHTML`).

## Module Overview

- `Illuminate.Diagram` contains the `Diagram` type, shapes, spatial
  algebra, and arrow routing.
- `Illuminate.Widget` provides the `#diagram` command, interactive
  parameter gadgets, and infoview integration.
- `Illuminate.Animation` provides the `#animate` command, step-based
  timeline with looping and pause steps, easing functions,
  interpolation, and compilation to frame-based playback.
- `Illuminate.DSL` includes the commutative diagram and state diagram
  builders.
- `Illuminate.Style` defines `Color`, `Fill`, `Stroke`, `TextStyle`,
  `DrawConfig`, and arrowhead types.
- `Illuminate.Geometry` provides `Vec2`, `Point`, `Matrix` (3x3 affine
  transforms), `Envelope`, and `PathData`.
- `Illuminate.Render` contains the `DrawCmd` display list and SVG
  backend.

The root import `import Illuminate` re-exports all modules.

## Development

Building requires [elan](https://github.com/leanprover/elan), which
manages Lean toolchains automatically via the `lean-toolchain` file.
The tests additionally require [uv](https://docs.astral.sh/uv/) and
[Docker](https://docs.docker.com/get-docker/) (on macOS,
[colima](https://github.com/abiosoft/colima) works).

Build the library with `lake build --wfail`. The project enables the
`missingDocs` linter, so all public declarations require docstrings.
The `--wfail` flag ensures warnings are treated as errors.

The test suite has two layers. `lake test --wfail` runs Lean unit
tests and writes SVG output files. `uv run test_playwright.py` then
runs two kinds of checks on those SVGs: structural DOM tests via
Playwright (headless Chromium), and pixel-level visual regression
tests that render each SVG via Inkscape inside a Docker container with
pinned fonts (`visual_tests/Dockerfile`). Both should pass before
submitting changes:

```sh
lake test --wfail && uv run test_playwright.py
```

### Type Checking Player JavaScript

The animation player and widget JavaScript in `player_js/` uses JSDoc
type annotations checked by TypeScript. React type definitions are
vendored in `vendored_js/`. To run the type checker:

```sh
npx tsc --noEmit -p player_js/jsconfig.json
```

### Updating Visual Baselines

Expected images live in `visual_tests/` as `*.expected.png` files.
When a visual test fails, compare the `*.actual.png` output against
the baseline. If the difference reflects an intentional change,
regenerate the baselines with:

```sh
UPDATE_BASELINES=1 uv run test_playwright.py
```

Review the updated baselines in the diff before committing.

## Acknowledgments

The design of Illuminate draws on three libraries in particular:
[Racket pict](https://docs.racket-lang.org/pict/),
[Haskell diagrams](https://diagrams.github.io/), and
[TikZ/PGF](https://github.com/pgf-tikz/pgf).
