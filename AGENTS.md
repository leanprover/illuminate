# Illuminate

A Lean 4 diagramming library with envelope-based spatial composition,
SVG rendering, and infoview preview via `#diagram`.

## General instructions

Always use the definition of `pi` in `Basic.lean`, importing if
necessary. Never define your own or use other workarounds.

## Prerequisites

- [elan](https://github.com/leanprover/elan) (manages Lean toolchains
  automatically via `lean-toolchain`)
- [uv](https://docs.astral.sh/uv/) (for Playwright visual tests)

## Building

```sh
lake build
```

This compiles the library (`src/`) and all dependencies. The build
must complete with **zero warnings** — `linter.missingDocs` is enabled
in `lakefile.lean`, so all public declarations require docstrings.

## Testing

Always run tests after every change before reporting success.

### Lean unit tests

```sh
lake test
```

This builds and runs the test executable. It also writes SVG files
(`smiley.svg`, `commdiag.svg`, `roundedrects.svg`) used by the visual
tests.

### Playwright visual regression tests

```sh
uv run test_playwright.py
```

Runs structural DOM tests and pixel-level visual regression tests
using headless Chromium. Playwright browsers are auto-installed on
first run.

To update expected baselines after intentional visual changes:

```sh
UPDATE_BASELINES=1 uv run test_playwright.py
```

Visual test files live in `visual_tests/`:

- `*.expected.png` — committed baselines (the ground truth)
- `*.actual.png` — generated each run, gitignored

**Important**: Never run `UPDATE_BASELINES=1` without explicit user
approval. If visual tests fail, investigate and fix the underlying
issue first. Only update baselines when the visual change is
intentional and the user has confirmed it.

### Running both

```sh
lake test && uv run test_playwright.py
```

## Project structure

```
src/Illuminate/          Library source
  Basic.lean             Foundational constants (pi)
  Vec2.lean              2D vector type
  Matrix.lean            3x3 affine transform matrix
  Envelope.lean          Envelope (direction -> extent) and CardinalAnchors
  PathData.lean          Path drawing commands (line, rect, circle, roundedRect)
  Style.lean             Color, Fill, Stroke, TextStyle, FontSpec
  MathExpr.lean          Math expression tree (atom, frac, sup, sub, etc.)
  Diagram.lean           Core diagram type and smart constructors
  Algebra.lean           Spatial composition (beside, hcat, vcat, grid, pad, frame)
  Arrow.lean             General-purpose curved arrow routing (LineEnd, Arrowhead, connect)
  Render.lean            DrawCmd display list and SVG backend
  FontMetrics.lean       Font measurement interface
  Renderable.lean        MeasuredBox type
  Layout.lean            Layout pass (name resolution, anchor lookup)
  Measure.lean           LayoutMeasure type class for monadic text/foreign measurement
  Validate.lean          Diagram validation
  Animation.lean         Animation framework
  CommDiag.lean          Commutative diagram DSL
  StateDiagram.lean      DFA/NFA state diagram builder
  Widget.lean            #diagram command for Lean infoview
src/Illuminate.lean      Root import (re-exports all modules)
test/Main.lean           Unit tests and #diagram previews
test_playwright.py       Playwright visual regression tests
visual_tests/            Expected and actual screenshot PNGs
lakefile.lean            Lake build configuration
lean-toolchain           Lean 4 toolchain pin (v4.28.0)
```

## Conventions

- `autoImplicit` is **false** — all variables must be explicitly
  bound.
- Docstrings use **indicative mood**: "Computes the envelope" not
  "Compute the envelope".
- **Single-line docstrings** go on one line:
  `/-- Computes the angle from `a`to`b`. -/`
- **Multi-line docstrings** use `/--` and `-/` on their own lines,
  body at column 0:

    ```lean
    /--
    Computes cubic Bézier control points for a line between two resolved endpoints.

    Each endpoint has an angle and a pull factor.
    -/
    def computeControlPoints ...
    ```

- **Field docstrings** inside structures are single-line, indented to
  match the field:
    ```lean
    structure Arrowhead where
      /-- Visual type of the arrowhead. -/
      type : ArrowType := .latex
      /-- Scaling factor for head length (1 = default 8px). -/
      length : Float := 1
    ```
- Deriving clauses go on a **separate line** from the last
  field/constructor, at column 0 (not indented):
    ```lean
    structure Arrowhead where
      /-- Visual type of the arrowhead. -/
      type : ArrowType := .latex
      /-- Scaling factor for head width (1 = default). -/
      width : Float := 1
    deriving Repr, BEq, Inhabited
    ```
- **No vertical alignment**: Do not pad field names or `:=` with extra
  spaces to align columns. Write `a := ...` not `a  := ...`.
- **No trailing `end`**: Do not put `end Namespace` at the end of a
  file. Lean 4 does not require it for file-level namespaces.
- `Diagram beta` is parameterized by a foreign primitive type; use
  `Empty` for pure geometric diagrams.
- The SVG backend applies a `scale(1,-1)` y-flip so that +y points up
  in diagram coordinates.
- Lean 4.28 has no `Float.pi`. Use `Illuminate.pi` from
  `src/Illuminate/Basic.lean`.

## `#diagram` previews

The test file includes `#diagram` commands that render diagrams inline
in the Lean infoview. Hover over any `#diagram` line in VS Code to see
a live SVG preview. These serve as both documentation and quick visual
checks during development.
