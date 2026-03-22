# Illuminate

A Lean 4 diagramming library with envelope-based spatial composition,
SVG rendering, and infoview preview via `#diagram`.

## General instructions

Always use the definition of `pi` in `Basic.lean`, importing if
necessary. Never define your own or use other workarounds.

## Prerequisites

- [elan](https://github.com/leanprover/elan) (manages Lean toolchains
  automatically via `lean-toolchain`)
- [uv](https://docs.astral.sh/uv/) (for Playwright structural tests
  and visual regression runner)
- [Docker](https://docs.docker.com/get-docker/) (for visual regression
  tests; on macOS, [colima](https://github.com/abiosoft/colima) works)

## Building

```sh
lake build --wfail
```

This compiles the library (`src/`) and all dependencies. The build
must complete with **zero warnings** — `linter.missingDocs` is enabled
in `lakefile.lean`, so all public declarations require docstrings. The
`--wfail` flag ensures warnings are treated as errors.

## Testing

Always run tests after every change before reporting success.

### Lean unit tests

```sh
lake test --wfail
```

This builds and runs the test executable. It also writes SVG files
(`smiley.svg`, `commdiag.svg`, `roundedrects.svg`) used by the visual
tests.

### Structural and visual regression tests

```sh
uv run test_playwright.py
```

Runs two kinds of tests:

- **Structural tests** use Playwright (headless Chromium) to inspect
  SVG DOM elements (element counts, text content, bounding boxes).
- **Visual regression tests** render SVGs to PNG via Inkscape inside a
  Docker container (`visual_tests/Dockerfile`) with bundled DejaVu
  fonts, then compare against committed baselines. The Docker image is
  built automatically on first run.

To update expected baselines after intentional visual changes:

```sh
UPDATE_BASELINES=1 uv run test_playwright.py
```

Visual test files live in `visual_tests/`:

- `*.expected.png` — committed baselines (the ground truth)
- `*.actual.png` — generated each run, gitignored
- `Dockerfile` — Inkscape + DejaVu fonts container for rendering
- `fonts/` — bundled DejaVu Sans and DejaVu Sans Mono (v2.37)
- `fonts.conf` — fontconfig rules mapping generic families to DejaVu
- `render.sh` — stdin→stdout SVG-to-PNG wrapper for Inkscape

**Important**: Never run `UPDATE_BASELINES=1` without explicit user
approval. If visual tests fail, investigate and fix the underlying
issue first. Only update baselines when the visual change is
intentional and the user has confirmed it.

### JavaScript type checking

The animation player and widget JavaScript in `player_js/` uses JSDoc
type annotations checked by TypeScript. React types are vendored in
`vendored_js/` (excluded from language stats via `.gitattributes`).

To run the type checker, use this command:

```sh
npx tsc --noEmit -p player_js/jsconfig.json
```

### Running both

```sh
lake test --wfail && uv run test_playwright.py
```

### Formatting

Before finishing work, run Prettier to format non-Lean files
(Markdown, Python, JSON, YAML):

```sh
npx prettier --write .
```

CI has a Prettier bot that auto-commits formatting fixes, but pushes
from `github-actions[bot]` do not re-trigger CI. Running Prettier
locally avoids this problem.

## Project structure

```
src/Illuminate/          Library source
  Geometry/
    Basic.lean           Foundational constants (pi)
    Vec2.lean            2D vector type (directions and offsets)
    Point.lean           2D point type (positions)
    Matrix.lean          3x3 affine transform matrix
    Envelope.lean        Envelope (direction -> extent)
    PathData.lean        Path drawing commands (line, rect, circle, roundedRect)
  Style/                 Color, Fill, Stroke, TextStyle, FontSpec, DrawConfig
  Diagram/
    Basic.lean           Core diagram type and smart constructors
    Placement.lean       Spatial composition (beside, hcat, vcat, grid, pad, frame)
    Arrow.lean           Curved arrow routing (LineEnd, Arrowhead, connect)
    Paper.lean           Piece-of-paper diagram element
    Compile.lean         Diagram → DrawCmd compiler and SVG renderer
    Validate.lean        Diagram validation (duplicate names, malformed paths)
  Render/
    DrawCmd.lean         Display list commands
    Svg.lean             SVG backend
  DSL/
    CommDiag.lean        Commutative diagram DSL
    StateDiagram.lean    DFA/NFA state diagram builder
  Widget.lean            #diagram command for Lean infoview
src/Illuminate.lean      Root import (re-exports all modules)
player_js/               Animation player and widget JavaScript (included via include_str)
  standalone.js          Standalone HTML player (play/pause/scrub)
  reveal.js              Reveal.js fragment-driven player
  diagram_widget.js      #diagram infoview widget
  animate_widget.js      #animate infoview widget
  jsconfig.json          TypeScript config for JSDoc type checking
  globals.d.ts           Type declarations for template placeholders
  infoview.d.ts          Type stub for @leanprover/infoview
vendored_js/             Vendored TypeScript type definitions (linguist-vendored)
  react/                 @types/react v19 (MIT)
  csstype/               csstype v3 (MIT)
test/Main.lean           Unit tests and #diagram previews
test_playwright.py       Structural and visual regression tests
visual_tests/            Baselines, Docker image, and bundled fonts
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
- **No trailing `end`**: A file must never end with `end Namespace`.
  The end of a file implicitly closes all open namespace blocks, so a
  trailing `end` is always redundant. Simply delete it.
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
