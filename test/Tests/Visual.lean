/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/

import Tests.Helpers
import Tests.AnimationExamples

set_option linter.missingDocs false

open Illuminate

/-!
# {lit}`#diagram` Previews

Hover to see SVG in the InfoView.
-/

-- Scale-invariant border: 1px stroke stays constant when the widget resizes
#diagram
  let c : Diagram SVG :=
    Diagram.circle 6
      (fill := rgb!"#e8f4e8")
      (stroke := { color := .black, width := .ofPx 1 }) |>.named `c
  c
    |>.connectEdge
      { point := `c, angle := some pi, pull := 2, arrowhead := some { type := .stealth } }
      { point := `c, angle := some (3 * pi / 2), pull := 2, arrowhead := some {} }
      (stroke := { color := .black, width := .ofPx 1 })
    |>.connectEdge
      { point := `c, angle := some 0, pull := 2, arrowhead := some { type := .stealth } }
      { point := `c, angle := some (pi / 2), pull := 2, arrowhead := some {} }
      (stroke := { color := .black, width := 1 })


-- Rounded rectangle
#diagram Diagram.roundedRect 80 40 8
  (fill := rgb!"#dcebff")
  (stroke := { color := .black, width := 1.5 })

-- Frame around text
#diagram (Diagram.pad 6 (.text "framed" { fontSize := 14 })).frame
  (stroke := { color := .black, width := 1 }) (padding := 2)

-- Horizontal concatenation
#diagram Diagram.hcat [
  Diagram.circle 15 (fill := Color.red),
  Diagram.rect 30 30 (fill := Color.green),
  Diagram.circle 15 (fill := Color.blue)
]

-- Vertical concatenation
#diagram Diagram.vcat [
  .text "Top" { fontSize := 12 },
  .rect 60 2 (fill := Color.black),
  .text "Bottom" { fontSize := 12 }
]

-- Grid layout
#diagram Diagram.grid #[
  #[some (.circle 10 (fill := Color.red)),
   some (.circle 10 (fill := Color.green))],
  #[some (.circle 10 (fill := Color.blue)),
   none]
]

open Diagram in
open Lean in
def labelDia (name : Name) (pull : Float) : Diagram SVG :=
  let start := roundedRect 25 15 0.4 (fill := rgb!"#641e84") (name := `start)
  let stop := roundedRect 20 15 0.4 (fill := rgb!"#218005") (name := `stop)
  let d := hsep 5 [start, stop] |>.named `boxes
  let angle := pi / 2
  d
    |>.connect
      { point := `boxes.start.north, pull, angle, arrowhead := some { type := .latex } }
      { point := `boxes.stop.north, pull, angle := some (- angle), arrowhead := some {} }
    |>.named name

def arrowBends : Diagram SVG :=
  let diagrams := List.range 19 |>.map fun i =>
    labelDia (`v ++ .num .anonymous i) (i.toFloat / 10.0)
  let len := diagrams.length
  let cols := max 1 len.toFloat.sqrt.toInt64.toNatClampNeg
  let rows := len / cols + (if cols ∣ len then 0 else 1)
  Diagram.vsep 5 <| List.range cols |>.map fun i =>
    Diagram.hsep 3 <| List.range rows |>.filterMap fun j =>
      diagrams[j * cols + i]?

#diagram arrowBends


open Diagram in
def arrowDemo (arrowhead : Arrowhead) : Diagram SVG :=
  vsep 10 <|
  List.range 7 |>.map fun l =>
    let length := Length.ofDiag (l.toFloat * 0.3)
    hsep 5 <|
    List.range 5 |>.map fun w =>
      let width := w.toFloat * 0.3
      let c1 := circle 0 (name := `c1)
      let c2 := circle 0 (name := `c2)
      hsep 15 [c1, c2]
        |>.connect `c1 { point := `c2, arrowhead := some { arrowhead with length, width } }
        |>.named (Lean.Name.mkSimple s!"{length.diag}x{width}")

#diagram arrowDemo {}

#diagram arrowDemo { type := .stealth }

#diagram arrowDemo { type := .circle }

#diagram arrowDemo { type := .triangle }

#diagram
  fun (rad : Slider "radius" 0 20 3)
      (sep : Slider "separation" 1 40 5)
      (red : Slider "red" 0 1 0.5)
      (green : Slider "green" 0 1 0.5)
      (blue : Slider "blue" 0 1 0.5)
      (count : Slider "count" 0 30 1) =>
    let red : Float := red
    let green : Float := green
    let blue : Float := blue
    let count : Float := count
    let r : UInt8 := (255.0 * red).toUInt8
    let g : UInt8 := (255.0 * green).toUInt8
    let b : UInt8 := (255.0 * blue).toUInt8
    let n : Nat := count.toUInt64.toNat
    let cols := n.toFloat.sqrt.ceil.toUInt64.toNat |> max 1
    let rows := (n + cols - 1) / cols
    .vsep sep <|
      List.range rows |>.map fun row =>
        let rowCount := min cols (n - row * cols)
        .hsep sep <| List.range rowCount |>.map fun _ =>
          .circle rad (fill := Color.rgb r g b)

/-!
# RoundedRect visual test
-/

def roundedRectsDiagram (pos : Slider "pos" 0 1 0.5) (pull : Slider "pull" 0 1 0.5) : Diagram SVG :=
  Diagram.hsep gap [node `left "Input", node `right "Output"]
    -- Forward arrow: left.north → right.north, arching upward
    |>.connect
      { point := `left.north, angle := some (pi / 2), pull }
      { point := `right.north, angle := some (-(pi / 2)), pull,
        arrowhead := some {} }
      (label := some { label := .text "forward" { fontSize := 11 }, pos })
    -- Backward arrow: right.south → left.south, arching downward
    |>.connect
      { point := `right.south, angle := some (-(pi / 2)), pull }
      { point := `left.south, angle := some (pi / 2), pull,
        arrowhead := some {} }
      (label := some { label := .text "backward" { fontSize := 11 }, pos })
where
  gap := 50
  node (name : Lean.Name) (label : String) : Diagram SVG :=
    Diagram.atop
      (Diagram.roundedRect 80 40 8 (fill := rgb!"#dcebff")
        (stroke := { color := Color.black, width := 1.5 }) (name := name))
      (.text label { fontSize := 14 })

#diagram roundedRectsDiagram

def testVisual_roundedRects : IO Unit :=
  testVisualWrite "roundedrects.svg" (roundedRectsDiagram (0.5 : Float) (0.5 : Float))

def testVisual_roundedRects_2_5 : IO Unit :=
  testVisualWrite "roundedrects_2_5.svg" (roundedRectsDiagram (0.2 : Float) (0.5 : Float))

def testVisual_roundedRects_2_10 : IO Unit :=
  testVisualWrite "roundedrects_2_10.svg" (roundedRectsDiagram (0.2 : Float) (1.0 : Float))

def testVisual_roundedRects_7_5 : IO Unit :=
  testVisualWrite "roundedrects_7_5.svg" (roundedRectsDiagram (0.7 : Float) (0.5 : Float))

def testVisual_roundedRects_7_2 : IO Unit :=
  testVisualWrite "roundedrects_7_2.svg" (roundedRectsDiagram (0.7 : Float) (0.2 : Float))


/-!
# Pipeline overview (from Lean reference manual)
-/

/-- Lean compilation pipeline: Code.lean → Syntax Tree → Core Type Theory → Executable -/
def pipelineDiagram : Diagram SVG :=
  let resultStyle : TextStyle := { fontSize := 20, bold := true }
  let result :=
    Diagram.hsep (align := .bottom) 8
      [.text "✔" { resultStyle with color := Color.green }, .text "/" resultStyle, .text "✖" resultStyle]
      |>.pad 8
      |>.namedWithAnchors `result
  let codeLabel :=
    Diagram.text "Code.lean"
      (style := { fontFamily := "monospace", fontSize := 12 })
      |>.pad 12
  let code :=
    Diagram.paper
      (name := `source)
      (label := some codeLabel)
      (width := some 80)
      (height := some 100)
      (fill := Color.white)
  Diagram.grid (hSpacing := 70) (vSpacing := 50) #[
    #[some code,                            none],
    #[some (box `stx "Syntax\nTree"),        none],
    #[some (box `core "Core Type\nTheory"), some (box `kernel "Core Type\nTheory\n(no recursion)")],
    #[some (box `exe "Executable"),         some result]
  ]
  -- Arrows with stealth arrowheads and upright labels
    |>.connect `source.south `stx.north
      (label := lbl "Parsing") (arrowhead := ah)
    |>.connect `stx.south `core.north
      (label := lbl "Elaboration") (arrowhead := ah)
    |>.connect `core.south `exe.north
      (label := lbl "Compilation") (arrowhead := ah)
    |>.connect `core.east `kernel.west
      (label := lbl "Recursion\nElimination") (arrowhead := ah)
  -- Self-loop on Syntax Tree for macro expansion (left side)
    |>.connect
      { point := `stx.west, shift := ⟨0, -10⟩, angle := some (pi + pi / 7), pull := 3.5 }
      { point := `stx.west, shift := ⟨0, 10⟩, angle := some (0 - pi / 7), pull := 3.5 }
      (label := lbl "Macro\nExpansion") (arrowhead := ah)
  -- Kernel check arrow
    |>.connect `kernel.south `result.north
      (label := lbl "Kernel\nCheck") (arrowhead := ah)
where
  ah : Arrowhead := { type := .stealth }
  lbl (s : String) : Option (Label SVG) :=
    some { label := .text s { fontSize := 10 }, upright := true }
  box (name : Lean.Name) (label : String) (fontFamily := "sans-serif") : Diagram SVG :=
    Diagram.text label { fontSize := 12, fontFamily }
      |>.pad 12
      |>.filledFrame
        (fill := Color.white)
        (stroke := { color := Color.black, width := 1 })
        (cornerRadius := 6)
      |>.namedWithAnchors name

#diagram pipelineDiagram

#diagram Diagram.text "a\nabcdef\nqf\nabcd\nabcdefg" |>.showEnvelope


def testVisual_pipeline : IO Unit :=
  testVisualWrite "pipeline.svg" pipelineDiagram (padding := 20)

/-!
# String memory layout (from Lean reference manual)
-/

/-- Memory layout of lean_string: m_header | m_size | m_capacity | m_length | m_data | '\0' -/
def stringLayoutDiagram : Diagram SVG :=
  let braceDepth := 12
  let braceGap := 4
  -- Build each field with its brace as a vertical unit, then hcat them
  let headerCol := fieldWithBrace `header "m_header" 90 braceDepth braceGap
    (txt "Lean object header")
  let sizeCol := fieldWithBrace `size "m_size" 70 braceDepth braceGap
    (twoLine "Byte count" "size_t")
  let capCol := fieldWithBrace `cap "m_capacity" 70 braceDepth braceGap
    (twoLine "Allocated space" "size_t")
  let lenCol := fieldWithBrace `len "m_length" 70 braceDepth braceGap
    (twoLine "Characters" "size_t")
  let dataCol := fieldWithBrace `data "m_data" 180 braceDepth braceGap
    (.vsep 1 [
      txt "String data",
      .hcat [.text "char" { fontSize := 8, fontFamily := "monospace" }, txt " array"]])
  let nulCol := field `nul "'\\0'" 30
  Diagram.hsep 0 [headerCol, sizeCol, capCol, lenCol, dataCol, nulCol] (align := .top)
where
  monoStyle : TextStyle := { fontSize := 10, fontFamily := "monospace" }
  txt (s : String) : Diagram SVG :=
    Diagram.text s { fontSize := 8, fontFamily := "sans-serif" }
  /-- Stacks a description line above a type line. -/
  twoLine (description typeLine : String) : Diagram SVG :=
    Diagram.vsep 1 [txt description, .text typeLine { fontSize := 8, fontFamily := "monospace" }]
  field (name : Lean.Name) (label : String) (w : Float) : Diagram SVG :=
    Diagram.atop
      ((Diagram.rect w 28 (fill := Color.white) (name := name)).padRight (-0.5) |>.padLeft (-0.5))
      (.text label monoStyle)
  /-- Builds a field box with a curly brace and label below it. -/
  fieldWithBrace (name : Lean.Name) (label : String) (w : Float)
      (braceDepth braceGap : Float) (braceLabel : Diagram SVG) : Diagram SVG :=
    let box := field name label w
    let brace := Diagram.curlyBrace (w - 8) (depth := braceDepth) (label := some braceLabel)
    let braceEnv := brace.getEnvelope
    let excess := braceEnv[Vec2.east].diag - w / 2
    let brace := if excess > 0 then brace |>.padLeft (-excess) |>.padRight (-excess) else brace
    Diagram.vsep braceGap [box, brace]

#diagram stringLayoutDiagram

def testVisual_stringLayout : IO Unit :=
  testVisualWrite "string-layout.svg" stringLayoutDiagram


/-!
# Lake workspace (from Lean reference manual)
-/

/-- Lake workspace hierarchy from the Lean reference manual. -/
def lakeWorkspaceDiagram : Diagram SVG :=
  let toolchain := mono "lean-toolchain"
  let rootPkg := borderedBox "Root package" <|
    items [
      "Package configuration file (lakefile.lean)",
      "Libraries",
      "Executables",
      "Manifest (lake-manifest.json)"
    ]
  let depItems := items ["Package configuration file", "Libraries", "Executables", "Artifacts"] 8
  let dep1 := borderedBox "Dependency 1" depItems 9 6
  let dep2 := borderedBox "Dependency 2" depItems 9 6
  let dots : Diagram SVG := .text "⋯" { fontSize := 14 }
  let packages := borderedBox "Packages" <|
    Diagram.vsep 8 [Diagram.hsep 12 [dep1, dep2], dots] (align := .left)
  let artifacts := borderedBox "Artifacts" <|
    items ["Built libraries", "Built executables"]
  let lakeDir := borderedBox "Lake Directory (.lake)" <|
    Diagram.vsep 10 [packages, artifacts] (align := .left)
  borderedBox "Workspace" <|
    Diagram.vsep 10 [toolchain, rootPkg, lakeDir] (align := .left)
where
  txt (s : String) (size : Float := 10) : Diagram SVG :=
    .text s { fontSize := size, anchor := TextAnchor.start }
  bold (s : String) (size : Float := 11) : Diagram SVG :=
    .text s { fontSize := size, bold := true, anchor := TextAnchor.start }
  mono (s : String) (size : Float := 10) : Diagram SVG :=
    .text s { fontSize := size, fontFamily := "monospace", anchor := TextAnchor.start }
  items (ss : List String) (size : Float := 10) : Diagram SVG :=
    Diagram.vsep 3 (ss.map fun s => txt s size) (align := .left)
  borderedBox (title : String) (content : Diagram SVG)
      (titleSize : Float := 11) (pad : Float := 8) : Diagram SVG :=
    Diagram.vsep 4 [bold title titleSize, content] (align := .left)
      |>.pad pad |>.frame (padding := 2) (cornerRadius := 4)

#diagram lakeWorkspaceDiagram

def testVisual_lakeWorkspace : IO Unit :=
  testVisualWrite "lake-workspace.svg" lakeWorkspaceDiagram

/-!
# Coercion chain (from Lean reference manual)
-/

/-- Coercion chain diagram from the Lean reference manual (coe-chain.tex). -/
def coeChainDiagram : Diagram SVG :=
  let spacing := 16
  -- Build from inside out: hcat items spanned by each brace, then vsep brace below
  -- Level 1: Coe* with CoeTC brace
  let level1 := Diagram.braceBelow (mono "Coe*") (mono "CoeTC")
  -- Level 2: add CoeOut* on the left, CoeOTC brace below
  let level2 := Diagram.braceBelow
    (Diagram.hsep spacing [mono "CoeOut*", level1] (align := .top))
    (mono "CoeOTC")
  -- Level 3: add CoeHead? on the left, CoeHTC brace below
  let level3 := Diagram.braceBelow
    (Diagram.hsep spacing [mono "CoeHead?", level2] (align := .top))
    (mono "CoeHTC")
  -- Level 4: add CoeTail? on the right, CoeHTCT brace below (named)
  let level4 := Diagram.braceBelow
    (Diagram.hsep spacing [level3, mono "CoeTail?"] (align := .top))
    (mono "CoeHTCT" |>.padBottom 3 |>.namedWithAnchors `CoeHTCT)
  -- CoeDep at same level as CoeHTCT label (bottom-aligned, named)
  let withCoeDep := Diagram.hsep 30
    [level4, mono "CoeDep" |>.padBottom 3 |>.namedWithAnchors `CoeDep] (align := .bottom)
  -- "or" and CoeT below, named for anchor resolution
  let orLabel : Diagram SVG :=
    Diagram.text "or" { fontSize := 10, italic := true } |>.pad 3 |>.namedWithAnchors `or
  let coeTLabel : Diagram SVG := mono "CoeT" (name := `CoeT)
  let lineStroke : Stroke := .ofWidth 1
  Diagram.vsep 12 [withCoeDep, orLabel, coeTLabel]
    |>.connectL `CoeHTCT.south `or.west (stroke := lineStroke)
    |>.connectL `CoeDep.south `or.east (stroke := lineStroke)
    |>.connectL `or.south `CoeT.north (stroke := lineStroke)
where
  mono (s : String) (name : Option Lean.Name := none) : Diagram SVG :=
    .text s { fontSize := 10, fontFamily := "monospace" } (name := name)

#diagram coeChainDiagram

def testVisual_coeChain : IO Unit :=
  testVisualWrite "coe-chain.svg" coeChainDiagram

#diagram (Diagram.circle 20 (fill := Color.transparent)).showEnvelope

#diagram (Diagram.rect 20 30 (fill := Color.transparent)).showEnvelope

#diagram (Diagram.roundedRect 20 30 3 (fill := Color.transparent)).showEnvelope

#diagram (Diagram.text "foo").showEnvelope

#diagram Diagram.hsep 30 [.circle 30 (fill := Color.transparent), .rect 10 50 (fill := Color.transparent)] |>.showEnvelope |>.showOrigin

#diagram Diagram.polygon 5 10

#diagram Diagram.paper (label := some <| Diagram.text "Code.lean" { fontSize := 12 })

#diagram Diagram.paper (width := some 80)

#diagram Diagram.paper (width := some 80) (height := some 60) (fill := Color.white)

open Diagram in
def paperTest : Diagram SVG :=
  vsep 20 [
    hsep 20 [
      paper (label := some <| text "Code"),
      paper (width := some 30)
    ],
    hsep 20 [
      paper (width := some 30) (height := some 20) (fill := Color.white),
      paper (width := some 20) (cornerFold := 0.75)
    ]
  ]

#diagram paperTest

/-- warning: #diagram: paper: cornerFold=1.500000 is outside 0–1, clamped to 1.000000 -/
#guard_msgs in
#diagram Diagram.paper (cornerFold := 1.5)

#diagram fun (fold : Slider "fold" 0 1 0.25) =>
  Diagram.paper (width := some 20) (cornerFold := fold)

/-!
# Stars with different point counts and dash patterns
-/

/-- Grid of stars: rows = point counts (1,2,3,5,6), columns = dash patterns. -/
def starsDiagram : Diagram SVG :=
  let pointCounts := [1, 2, 3, 5, 6]
  let dashes : List StrokeDash := [.solid, .dashed, .dotted, .dashDot]
  let dashLabels := ["solid", "dashed", "dotted", "dashDot"]
  let outerR := 20.0
  let innerR := 10.0
  let fill : Fill := rgb!"#ffe664"
  -- Column headers
  let headers := Diagram.hsep 20 (dashLabels.map fun l =>
    Diagram.withEnvelope (Envelope.ofRect 25 8)
      (.text l { fontSize := 9 }))
  -- Row label + stars for each point count
  let rows := pointCounts.map fun n =>
    let label := Diagram.withEnvelope (Envelope.ofRect 12 25)
      (Diagram.text s!"{n}pt" { fontSize := 9 })
    let cells := dashes.map fun dash =>
      Diagram.withEnvelope (Envelope.ofRect 25 25)
        (Diagram.star n outerR innerR
          (fill := fill)
          (stroke := { color := Color.black, width := 1.5, dash := dash }))
    Diagram.hsep 20 (label :: cells)
  -- Combine header row with star rows
  let headerRow := Diagram.hsep 20
    [Diagram.withEnvelope (Envelope.ofRect 12 8) .empty,
     headers]
  Diagram.vsep 15 (headerRow :: rows)

#diagram starsDiagram

def testVisual_stars : IO Unit :=
  testVisualWrite "stars.svg" starsDiagram
    (checks := [("stroke-dasharray", "has dashed strokes")])

/-- Three stars (7, 10, 13 points) with point5 of each connected by arrows. -/
def starAnchorsDiagram : Diagram SVG :=
  let fill : Fill := rgb!"#ffe664"
  let stroke : Stroke := { color := Color.black, width := 1.5 }
  let s7 := Diagram.star 7 30 15 (fill := fill) (stroke := stroke) (name := `star7)
  let s10 := Diagram.star 10 30 15 (fill := fill) (stroke := stroke) (name := `star10)
  let s13 := Diagram.star 13 30 15 (fill := fill) (stroke := stroke) (name := `star13)
  let stealth : Option Arrowhead := some { type := .stealth }
  Diagram.hsep 40 [s7, s10, s13]
    |>.connect
      { point := `star7.point5, angle := some 0.3, pull := 1 }
      { point := `star10.point3, arrowhead := stealth, angle := some 0, pull := 0.25 }
    |>.connect
      { point := `star10.point5, angle := some (3 * pi / 2), pull := 1 }
      { point := `star13.point3, arrowhead := stealth, angle := some 0, pull := 0.5 }

#diagram starAnchorsDiagram

def testVisual_starAnchors : IO Unit :=
  testVisualWrite "star-anchors.svg" starAnchorsDiagram

/-!
# Ellipse
-/

/-- Ellipses with various aspect ratios. -/
def ellipseDiagram : Diagram SVG :=
  let fill : Fill := rgb!"#b4d2ff"
  let stroke : Stroke := { color := Color.black, width := 1.5 }
  Diagram.hsep 15 [
    Diagram.ellipse 30 15 (fill := fill) (stroke := stroke),
    Diagram.ellipse 15 30 (fill := fill) (stroke := stroke),
    Diagram.ellipse 25 25 (fill := fill) (stroke := stroke),
    Diagram.ellipse 35 10 (fill := fill) (stroke := { stroke with dash := StrokeDash.dashed })
  ]

#diagram ellipseDiagram

def testVisual_ellipse : IO Unit :=
  testVisualWrite "ellipse.svg" ellipseDiagram

/-!
# Transforms: scale, rotate, hflip, vflip
-/

/-- A labeled arrow shape used to show transform effects. -/
private def arrowShape : Diagram SVG :=
  let path := PathData.empty
    |>.moveTo ⟨-15, -8⟩
    |>.lineTo ⟨5, -8⟩
    |>.lineTo ⟨5, -15⟩
    |>.lineTo ⟨15, 0⟩
    |>.lineTo ⟨5, 15⟩
    |>.lineTo ⟨5, 8⟩
    |>.lineTo ⟨-15, 8⟩
    |>.close
  Diagram.fromPath path
    (fill := rgb!"#64b464")
    (stroke := { color := Color.black, width := 1 })

/-- Demonstrates scale, rotate, hflip, vflip on an arrow shape. -/
def transformsDiagram : Diagram SVG :=
  let label (s : String) : Diagram SVG :=
    .text s { fontSize := 9 }
  Diagram.hsep 20 [
    Diagram.vsep 5 [label "original", arrowShape],
    Diagram.vsep 5 [label "scale 1.5", Diagram.scale 1.5 arrowShape],
    Diagram.vsep 5 [label "rotate 45°", Diagram.rotate (pi / 4) arrowShape],
    Diagram.vsep 5 [label "hflip", Diagram.hflip arrowShape],
    Diagram.vsep 5 [label "vflip", Diagram.vflip arrowShape],
    Diagram.vsep 5 [label "scaleXY 1.5 0.5", Diagram.scaleXY 1.5 0.5 arrowShape]
  ] (align := .top)

#diagram transformsDiagram

def testVisual_transforms : IO Unit :=
  testVisualWrite "transforms.svg" transformsDiagram
    (checks := [("matrix(", "has transform matrices")])

/-!
# Ghost and refocus
-/

/-- Demonstrates ghost and refocus. -/
def ghostRefocusDiagram : Diagram SVG :=
  let red : Fill := Color.red
  let blue : Fill := Color.blue
  let green : Fill := rgb!"#00a000cc"
  let label (s : String) : Diagram SVG :=
    .text s { fontSize := 9 }
  -- Row 1: normal vs ghost — ghost reserves space but draws nothing
  let box := Diagram.rect 30 30 (fill := red)
  let circ := Diagram.circle 15 (fill := blue)
  let normal := Diagram.hsep 5 [box, circ, box]
  let withGhost := Diagram.hsep 5 [box, Diagram.ghost circ, box]
  -- Row 2: refocus — combined diagram uses sub-diagram's envelope
  let big := Diagram.rect 60 40 (fill := green)
  let small := Diagram.circle 10 (fill := red)
  let combined := Diagram.atop big small
  let refocused := Diagram.refocus small combined
  Diagram.vsep 20 [
    Diagram.hsep 30 [
      Diagram.vsep 5 [label "normal", normal],
      Diagram.vsep 5 [label "ghost middle", withGhost]
    ],
    Diagram.hsep 30 [
      Diagram.vsep 5 [label "combined", combined |>.showEnvelope],
      Diagram.vsep 5 [label "refocus small", refocused |>.showEnvelope]
    ]
  ]

#diagram ghostRefocusDiagram

def testVisual_ghostRefocus : IO Unit :=
  testVisualWrite "ghost-refocus.svg" ghostRefocusDiagram

/-!
# Cellophane, clip, pinOver, pinUnder
-/

/-- Demonstrates cellophane (opacity), clip, pinOver, and pinUnder. -/
def cellophaneClipDiagram : Diagram SVG :=
  let label (s : String) : Diagram SVG :=
    .text s { fontSize := 9 }
  let red : Fill := Color.red
  let blue : Fill := Color.blue
  let green : Fill := rgb!"#00a000"
  -- Row 1: cellophane at different opacities
  let box := Diagram.rect 40 30 (fill := red)
  let celloRow := Diagram.hsep 15 [
    Diagram.vsep 5 [label "opacity 1.0", Diagram.cellophane 1.0 box],
    Diagram.vsep 5 [label "opacity 0.5", Diagram.cellophane 0.5 box],
    Diagram.vsep 5 [label "opacity 0.2", Diagram.cellophane 0.2 box]
  ] (align := .top)
  -- Row 2: clip
  let checker := Diagram.atop
    (Diagram.rect 60 60 (fill := blue))
    (Diagram.translate 10 10 (Diagram.rect 40 40 (fill := green)))
  let clipped := Diagram.clipCircle 25 checker
  let clipRow := Diagram.hsep 30 [
    Diagram.vsep 5 [label "unclipped", checker],
    Diagram.vsep 5 [label "clip circle r=25", clipped]
  ] (align := .top)
  -- Row 3: pinOver and pinUnder
  let base1 := Diagram.hsep 40 [
    Diagram.circle 20 (fill := blue) (name := `left),
    Diagram.circle 20 (fill := green) (name := `right)
  ]
  let dot := Diagram.circle 5 (fill := red)
  let pinned := base1
    |> Diagram.pinOver `left dot
    |> Diagram.pinOver `right dot
  let semiBlue : Fill := rgb!"#0000ff80"
  let semiGreen : Fill := rgb!"#00a00080"
  let base2 := Diagram.hsep 40 [
    Diagram.circle 20 (fill := semiBlue) (name := `left2),
    Diagram.circle 20 (fill := semiGreen) (name := `right2)
  ]
  let underDot := Diagram.circle 12 (fill := rgb!"#ffc800")
  let pinnedUnder := base2
    |> Diagram.pinUnder `left2 underDot
  let pinRow := Diagram.hsep 30 [
    Diagram.vsep 5 [label "pinOver (red dots)", pinned],
    Diagram.vsep 5 [label "pinUnder (yellow)", pinnedUnder]
  ] (align := .top)
  Diagram.vsep 25 [celloRow, clipRow, pinRow]

#diagram cellophaneClipDiagram

def testVisual_cellophaneClip : IO Unit :=
  testVisualWrite "cellophane-clip.svg" cellophaneClipDiagram
    (checks := [("opacity", "has opacity attributes"), ("clipPath", "has clip paths")])

/-!
# Trace visual tests
-/

/-- Circle with rays from four different directions. -/
def traceCircleDiagram : Diagram SVG :=
  let c := Diagram.circle 20 (fill := rgb!"#c8dcff")
    (stroke := { color := Color.black, width := 1 })
  c.showTraces [
    (⟨-40, 0⟩, ⟨1, 0⟩, .red),
    (⟨0, -40⟩, ⟨0.3, 1⟩, .green),
    (⟨30, 30⟩, ⟨-1, -0.8⟩, .blue),
    (⟨-7, -35⟩, ⟨0.15, 1⟩, .black)
  ]

#diagram traceCircleDiagram

def testVisual_traceCircle : IO Unit :=
  testVisualWrite "trace-circle.svg" traceCircleDiagram

/-- Rectangle with axis-aligned and diagonal rays. -/
def traceRectDiagram : Diagram SVG :=
  let r := Diagram.rect 40 30 (fill := rgb!"#ffe6c8")
    (stroke := { color := Color.black, width := 1 })
  r.showTraces [
    (⟨-40, 0⟩, ⟨1, 0⟩, .black),
    (⟨0, -35⟩, ⟨0, 1⟩, .red),
    (⟨-35, -25⟩, ⟨1, 0.7⟩, .blue),
    (⟨30, -20⟩, ⟨-0.8, 1⟩, .green)
  ]

#diagram traceRectDiagram

def testVisual_traceRect : IO Unit :=
  testVisualWrite "trace-rect.svg" traceRectDiagram

/-- Two overlapping shapes (circle + rect) with a ray hitting both. -/
def traceComposedDiagram : Diagram SVG :=
  let c := Diagram.circle 15 (fill := rgb!"#c8c8ff80")
  let r := Diagram.translate 10 0
    (Diagram.rect 30 20 (fill := rgb!"#ffc8c880"))
  let d := Diagram.compose c r
  d.showTraces [
    (⟨-30, 0⟩, ⟨1, 0⟩, .red),
    (⟨-25, -15⟩, ⟨1, 0.5⟩, rgb!"#008000")
  ]

#diagram traceComposedDiagram

def testVisual_traceComposed : IO Unit :=
  testVisualWrite "trace-composed.svg" traceComposedDiagram

/-- Rotated rectangle with trace rays. -/
def traceTransformedDiagram : Diagram SVG :=
  let r := Diagram.rotate (pi / 6)
    (Diagram.rect 40 20 (fill := rgb!"#dcffdc")
      (stroke := { color := Color.black, width := 1 }))
  r.showTraces [
    (⟨-35, 0⟩, ⟨1, 0⟩, .red),
    (⟨0, -30⟩, ⟨0, 1⟩, .red),
    (⟨-30, -20⟩, ⟨1, 1⟩, rgb!"#0000c8")
  ]

#diagram traceTransformedDiagram

def testVisual_traceTransformed : IO Unit :=
  testVisualWrite "trace-transformed.svg" traceTransformedDiagram

/-- Closed F-shaped path with an oblique ray crossing multiple edges. -/
def tracePathDiagram : Diagram SVG :=
  -- Outline of a capital letter F as a closed polygon
  let fPath := PathData.empty
    |>.moveTo ⟨0, 0⟩         -- bottom-left of vertical stroke
    |>.lineTo ⟨10, 0⟩        -- bottom-right of vertical stroke
    |>.lineTo ⟨10, 20⟩       -- up to crossbar
    |>.lineTo ⟨25, 20⟩       -- crossbar right
    |>.lineTo ⟨25, 25⟩       -- crossbar top
    |>.lineTo ⟨10, 25⟩       -- crossbar inner
    |>.lineTo ⟨10, 40⟩       -- up to top bar
    |>.lineTo ⟨30, 40⟩       -- top bar right
    |>.lineTo ⟨30, 45⟩       -- top edge
    |>.lineTo ⟨0, 45⟩        -- top-left
    |>.close                  -- back to origin
  let f := Diagram.fromPath fPath
    (fill := rgb!"#fff0c8")
    (stroke := { color := Color.black, width := 1 })
  f.showTraces [
    (⟨-1, -2⟩, ⟨1, 1.5⟩, .red),
    (⟨-5, 35⟩, ⟨1, -0.3⟩, .green),
    (⟨35, 2⟩, ⟨-0.7, 1⟩, .blue)
  ]

#diagram tracePathDiagram

def testVisual_tracePath : IO Unit :=
  testVisualWrite "trace-path.svg" tracePathDiagram

/-- Trace-based arrow connections between shapes at non-cardinal angles. -/
def traceConnectDiagram : Diagram SVG :=
  let circ := Diagram.circle 15
    (fill := rgb!"#c8dcff")
    (stroke := { color := Color.black, width := 1 })
    (name := `circ)
  let box := Diagram.roundedRect 40 25 4
    (fill := rgb!"#ffe6c8")
    (stroke := { color := Color.black, width := 1 })
    (name := `box)
  let ell := Diagram.ellipse 20 12
    (fill := rgb!"#dcffdc")
    (stroke := { color := Color.black, width := 1 })
    (name := `ell)
  -- Arrange in a triangle layout
  let top := Diagram.translate 0 50 circ
  let botLeft := Diagram.translate (-50) (-20) box
  let botRight := Diagram.translate 50 (-20) ell
  let d := Diagram.compose (Diagram.compose top botLeft) botRight
  d
    |>.connectEdge `circ (stop := { point := `box, arrowhead := some { type := .stealth } })
    |>.connectEdge `circ (stop := { point := `ell, arrowhead := some { type := .latex } })
    |>.connectEdge `box (stop := { point := `ell, arrowhead := some { type := .triangle } })

#diagram traceConnectDiagram

def testVisual_traceConnect : IO Unit :=
  testVisualWrite "trace-connect.svg" traceConnectDiagram

def traceConnectAngledDiagram (angle3 : Slider "Angle 3" 0 (2 * pi) pi) : Diagram SVG :=
  let circ := Diagram.circle 15
    (fill := rgb!"#c8dcff")
    (stroke := { color := Color.black, width := 1 })
    (name := `circ)
  let box := Diagram.roundedRect 40 25 4
    (fill := rgb!"#ffe6c8")
    (stroke := { color := Color.black, width := 1 })
    (name := `box)
  let ell := Diagram.star 5 20 12
    (fill := rgb!"#dcffdc")
    (stroke := { color := Color.black, width := 1 })
    (name := `ell)
  -- Arrange in a triangle layout
  let top := Diagram.translate 0 50 circ
  let botLeft := Diagram.translate (-50) (-20) box
  let botRight := Diagram.translate 50 (-20) ell
  let d := Diagram.compose (Diagram.compose top botLeft) botRight
  d
    |>.connectEdge {point := `circ, angle := some (1.5 * pi)} { point := `box, arrowhead := some { type := .stealth }, angle := some pi, pull := 3 }
    |>.connectEdge `circ { point := `ell, arrowhead := some { type := .latex } }
    |>.connectEdge {point := `box, pull := 1} { point := `ell, arrowhead := some { type := .triangle }, angle := some angle3, pull := 1.5 }

#diagram traceConnectAngledDiagram

def testVisual_traceConnectAngled : IO Unit :=
  testVisualWrite "trace-connect-angled.svg" (traceConnectAngledDiagram (1.2 * pi))


/-!
# Concentric circles with hit-test labels
-/

private def hueToColor (h : Float) : Color :=
  let h := h - h.floor  -- normalize to [0,1)
  let x := (1 - (h * 6 - (h * 6 / 2).floor * 2 - 1).abs)
  let (r, g, b) :=
    if h < 1.0 / 6 then (1.0, x, 0.0)
    else if h < 2.0 / 6 then (x, 1.0, 0.0)
    else if h < 3.0 / 6 then (0.0, 1.0, x)
    else if h < 4.0 / 6 then (0.0, x, 1.0)
    else if h < 5.0 / 6 then (x, 0.0, 1.0)
    else (1.0, 0.0, x)
  { r := (r * 255).toUInt8, g := (g * 255).toUInt8, b := (b * 255).toUInt8 }

def concentricCircles (n : Slider "circles" 0 20 5) : DiagramWithInfo :=
  let n : Nat := n.toUInt64.toNat |> max 1
  let maxRadius : Float := 10 * n.toFloat
  -- Build concentric circles from largest (back) to smallest (front)
  let diagram := (List.range n).foldl (init := (Diagram.empty : Diagram SVG)) fun acc i =>
    let radius := maxRadius * (n - i).toFloat / n.toFloat
    let color := hueToColor (i.toFloat / n.toFloat)
    let circle := Diagram.tag i
      (Diagram.circle radius (fill := color)
        (stroke := { color := Color.black, width := 1 }))
    Diagram.compose acc circle
  -- Build regions map
  let regions : Std.HashMap Nat String := (List.range n).foldl
    (init := .emptyWithCapacity 0) fun acc i =>
    acc.insert i s!"Circle {i + 1}"
  ⟨diagram, regions⟩

#diagram concentricCircles

/-- Three superimposed shapes; the middle one links to lean-lang.org. -/
def linkDemo : Diagram SVG :=
  let back := Diagram.rect 120 80
    (fill := rgb!"#c8dcff")
    (stroke := { color := Color.black, width := 1 })
  let middle := Diagram.foreign (.link "https://lean-lang.org")
    (Diagram.roundedRect 80 50 8
      (fill := rgb!"#ffdcb4")
      (stroke := { color := rgb!"#c86400", width := 2 }))
  let front := Diagram.circle 15
    (fill := rgb!"#b4ffb4")
    (stroke := { color := Color.black, width := 1 })
  Diagram.compose (Diagram.compose back middle) front

#diagram linkDemo

/-!
# Gradient demos
-/

def gradientRectDemo : Diagram SVG :=
  let linearRect := Diagram.rect 80 40
    (fill := .between (rgb!"#4285f4") (rgb!"#ea4335") .east)
    (stroke := { color := Color.black, width := 1 })
  let radialCircle := Diagram.circle 25
    (fill := .radialBetween .white (rgb!"#4285f4"))
    (stroke := { color := Color.black, width := 1 })
  Diagram.hsep 20 [linearRect, radialCircle]

#diagram gradientRectDemo

def testVisual_gradients : IO Unit :=
  testVisualWrite "gradients.svg" gradientRectDemo
    (checks := [
      ("<linearGradient", "has linear gradient def"),
      ("<radialGradient", "has radial gradient def"),
      ("url(#grad", "references gradient by ID")
    ])

/-!
# Pizza wedge demo
-/

def pizzaDemo : Diagram SVG :=
  let r := 50.0
  let crust : Stroke := { color := rgb!"#50280a", width := 1.5 }
  -- The pulled-out slice
  let sliceStart := pi / 3
  let sliceEnd := pi / 3 + pi / 4
  let sliceMid := (sliceStart + sliceEnd) / 2
  let pullDist := 14.0
  let sliceFill := Fill.radialBetween (rgb!"#ffdc50") (rgb!"#dc8c14")
  let slice := Diagram.wedge sliceStart sliceEnd r (fill := sliceFill) (stroke := crust)
  let slice := Diagram.move (.dir sliceMid) pullDist slice
  -- The main body (everything except the pulled slice, with a small gap)
  let gap := 0.04
  let bodyFill := Fill.radialBetween (rgb!"#fac83c") (rgb!"#d2820f")
  let body := Diagram.wedge (sliceEnd + gap) (sliceStart + 2 * pi - gap) r
    (fill := bodyFill) (stroke := crust)
  let pizza := Diagram.compose body slice
  -- Three ring wedges around the outside
  let ringR := r + 18
  let ringW := 10.0
  let ringStroke : Stroke := { color := rgb!"#28283c", width := 0.8 }
  let ring1 := Diagram.ringWedge (0) (2 * pi / 3 - 0.06) ringR (ringR + ringW)
    (fill := .between (rgb!"#64b4ff") (rgb!"#1e50c8") .east)
    (stroke := ringStroke)
  let ring2 := Diagram.ringWedge (2 * pi / 3 + 0.02) (4 * pi / 3 - 0.06) ringR (ringR + ringW)
    (fill := .between (rgb!"#ff78b4") (rgb!"#c82864") .north)
    (stroke := ringStroke)
  let ring3 := Diagram.ringWedge (4 * pi / 3 + 0.02) (2 * pi - 0.06) ringR (ringR + ringW)
    (fill := .radialGradient #[
      { offset := 0.6, color := rgb!"#64dc82" },
      { offset := 1, color := rgb!"#1e963c" }
    ])
    (stroke := ringStroke)
  [pizza, ring1, ring2, ring3].foldl Diagram.compose Diagram.empty

#diagram pizzaDemo

def testVisual_pizza : IO Unit :=
  testVisualWrite "pizza.svg" pizzaDemo
    (checks := [
      ("url(#grad", "has gradient references"),
      ("<path", "has path elements")
    ])

/-!
# Animation filmstrips
-/

/-- Growing circle: blue→red with easeInOut. -/
def filmstripGrowingCircle : Diagram SVG :=
  filmstrip (fun t =>
    let t := Easing.easeInOut t
    let radius := Interpolate.interpolate 10.0 50.0 t
    let color := Interpolate.interpolate Color.blue Color.red t
    Diagram.circle radius (fill := .solid { color })
      (stroke := { color := Color.black, width := 1 }))

def testVisual_filmstripGrowingCircle : IO Unit :=
  testVisualWrite "filmstrip-growing-circle.svg" filmstripGrowingCircle

/-- Rotating square with easeInOut. -/
def filmstripRotatingSquare : Diagram SVG :=
  filmstrip (fun t =>
    let t := Easing.easeInOut t
    animRotate
      (Diagram.rect 40 40
        (fill := .solid { color := { r := 100, g := 149, b := 237 } })
        (stroke := { color := Color.black, width := 1.5 }))
      0 (2 * pi) t)

def testVisual_filmstripRotatingSquare : IO Unit :=
  testVisualWrite "filmstrip-rotating-square.svg" filmstripRotatingSquare

/-- Animated clip shape: clip rectangle grows to reveal more of the red circle. -/
def filmstripAnimatedClip : Diagram SVG :=
  filmstrip (fun t =>
    let t := Easing.easeInOut t
    let clipSize := Interpolate.interpolate 20.0 60.0 t
    Diagram.clipRect clipSize clipSize
      (Diagram.circle 30 (fill := .solid { color := Color.red })))

def testVisual_filmstripAnimatedClip : IO Unit :=
  testVisualWrite "filmstrip-animated-clip.svg" filmstripAnimatedClip

/-!
# Morph filmstrips
-/

/-- Simple morph: rectangle to circle. -/
def filmstripMorphRectCircle : Diagram SVG :=
  let a := Diagram.rect 40 40 (fill := Color.blue) (stroke := { color := Color.black, width := 1 })
  let b := Diagram.circle 25 (fill := Color.red) (stroke := { color := Color.black, width := 1 })
  let m := a.morph b
  filmstrip (fun t => m.evaluate t)

def testVisual_filmstripMorphRectCircle : IO Unit :=
  testVisualWrite "filmstrip-morph-rect-circle.svg" filmstripMorphRectCircle

/-- Nested named diagram morph exercising recursive name matching and arrow morphing. -/
def filmstripMorphNested : Diagram SVG :=
  let (start, stop) := nestedMorphDiagrams
  let m := start.morph stop
  filmstrip (fun t => m.evaluate (Easing.easeInOut t))

def testVisual_filmstripMorphNested : IO Unit :=
  testVisualWrite "filmstrip-morph-nested.svg" filmstripMorphNested

/-- Filmstrip of the morph arrow tests (connect, connectL, connectEdge morphing). -/
def filmstripMorphArrows : Diagram SVG :=
  let (start, stop) := morphArrowTests
  let m := start.morph stop
  filmstrip (fun t => m.evaluate (Easing.easeInOut t))

def testVisual_filmstripMorphArrows : IO Unit :=
  testVisualWrite "filmstrip-morph-arrows.svg" filmstripMorphArrows

def visualTests : List (String × IO Unit) := [
  ("Visual/roundedRects", testVisual_roundedRects),
  ("Visual/roundedRects_2_5", testVisual_roundedRects_2_5),
  ("Visual/roundedRects_2_10", testVisual_roundedRects_2_10),
  ("Visual/roundedRects_7_2", testVisual_roundedRects_7_2),
  ("Visual/roundedRects_7_5", testVisual_roundedRects_7_5),
  ("Visual/pipeline", testVisual_pipeline),
  ("Visual/stringLayout", testVisual_stringLayout),
  ("Visual/lakeWorkspace", testVisual_lakeWorkspace),
  ("Visual/coeChain", testVisual_coeChain),
  ("Visual/stars", testVisual_stars),
  ("Visual/starAnchors", testVisual_starAnchors),
  ("Visual/ellipse", testVisual_ellipse),
  ("Visual/transforms", testVisual_transforms),
  ("Visual/ghostRefocus", testVisual_ghostRefocus),
  ("Visual/cellophaneClip", testVisual_cellophaneClip),
  ("Visual/traceCircle", testVisual_traceCircle),
  ("Visual/traceRect", testVisual_traceRect),
  ("Visual/traceComposed", testVisual_traceComposed),
  ("Visual/traceTransformed", testVisual_traceTransformed),
  ("Visual/tracePath", testVisual_tracePath),
  ("Visual/traceConnect", testVisual_traceConnect),
  ("Visual/traceConnectAngled", testVisual_traceConnectAngled),
  ("Visual/gradients", testVisual_gradients),
  ("Visual/pizza", testVisual_pizza),
  ("Visual/filmstripGrowingCircle", testVisual_filmstripGrowingCircle),
  ("Visual/filmstripRotatingSquare", testVisual_filmstripRotatingSquare),
  ("Visual/filmstripAnimatedClip", testVisual_filmstripAnimatedClip),
  ("Visual/filmstripMorphRectCircle", testVisual_filmstripMorphRectCircle),
  ("Visual/filmstripMorphNested", testVisual_filmstripMorphNested),
  ("Visual/filmstripMorphArrows", testVisual_filmstripMorphArrows)
]
