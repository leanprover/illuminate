/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/

import Tests.Helpers

set_option linter.missingDocs false

open Illuminate

-- ══════════════════════════════════════════════════════════════════
-- #diagram previews — hover to see SVG in the infoview
-- ══════════════════════════════════════════════════════════════════

-- Rounded rectangle
#diagram Diagram.roundedRect 80 40 8
  (fill := { color := some { r := 220, g := 235, b := 255 } })
  (stroke := { color := Color.black, width := (1.5 : Float) })

-- Frame around text
#diagram (Diagram.pad 6 (.text "framed" { fontSize := (14 : Float) })).frame
  (stroke := { color := Color.black, width := (1 : Float) }) (padding := 2)

-- Horizontal concatenation
#diagram Diagram.hcat [
  Diagram.circle 15 (fill := { color := Color.red }),
  Diagram.rect 30 30 (fill := { color := Color.green }),
  Diagram.circle 15 (fill := { color := Color.blue })
]

-- Vertical concatenation
#diagram Diagram.vcat [
  .text "Top" { fontSize := (12 : Float) },
  .rect 60 2 (fill := { color := Color.black }),
  .text "Bottom" { fontSize := (12 : Float) }
]

-- Grid layout
#diagram Diagram.grid #[
  #[some (Diagram.circle 10 (fill := { color := Color.red })),
   some (Diagram.circle 10 (fill := { color := Color.green }))],
  #[some (Diagram.circle 10 (fill := { color := Color.blue })),
   none]
]

open Diagram in
open Lean in
def labelDia (name : Name) (pull : Float) : Diagram Empty :=
  let start := roundedRect 25 15 0.4 (fill := { color := some {r := 100, g := 30, b := 132} }) (name := `start)
  let stop := roundedRect 20 15 0.4 (fill := { color := some {r := 33, g := 128, b := 5} }) (name := `stop)
  let d := hsep 5 [start, stop] |>.named `boxes
  let angle := pi / 2
  d
    |>.connect
      { point := `boxes.start.north, pull, angle, arrowhead := some { type := .latex } }
      { point := `boxes.stop.north, pull, angle := some (- angle), arrowhead := some {} }
    |>.named name

def arrowBends : Diagram Empty :=
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
def arrowDemo (arrowhead : Arrowhead) : Diagram Empty :=
  vsep 10 <|
  List.range 7 |>.map fun l =>
    let length := l.toFloat * 0.3
    hsep 5 <|
    List.range 5 |>.map fun w =>
      let width := w.toFloat * 0.3
      let c1 := circle 0 (name := `c1)
      let c2 := circle 0 (name := `c2)
      hsep 15 [c1, c2]
        |>.connect `c1 { point := `c2, arrowhead := some { arrowhead with length, width } }
        |>.named (Lean.Name.mkSimple s!"{length}x{width}")

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
          .circle rad (fill := {color := some { r, g, b } })

-- ══════════════════════════════════════════════════════════════════
-- RoundedRect visual test
-- ══════════════════════════════════════════════════════════════════

def roundedRectsDiagram (pos : Slider "pos" 0 1 0.5) (pull : Slider "pull" 0 1 0.5) : Diagram Empty :=
  Diagram.hsep gap [node `left "Input", node `right "Output"]
    -- Forward arrow: left.north → right.north, arching upward
    |>.connect
      { point := `left.north, angle := some (pi / 2), pull }
      { point := `right.north, angle := some (-(pi / 2)), pull,
        arrowhead := some {} }
      (label := some { label := .text "forward" { fontSize := (11 : Float) }, pos })
    -- Backward arrow: right.south → left.south, arching downward
    |>.connect
      { point := `right.south, angle := some (-(pi / 2)), pull }
      { point := `left.south, angle := some (pi / 2), pull,
        arrowhead := some {} }
      (label := some { label := .text "backward" { fontSize := (11 : Float) }, pos })
where
  gap := 50
  node (name : Lean.Name) (label : String) : Diagram Empty :=
    Diagram.atop
      (Diagram.roundedRect 80 40 8 (fill := { color := some { r := 220, g := 235, b := 255 } })
        (stroke := { color := Color.black, width := (1.5 : Float) }) (name := name))
      (.text label { fontSize := (14 : Float) })

#diagram roundedRectsDiagram

def testVisual_roundedRects : IO Unit :=
  testVisualWrite "roundedrects.svg" (roundedRectsDiagram (0.5 : Float) (0.5 : Float))

-- ══════════════════════════════════════════════════════════════════
-- Pipeline overview (from Lean reference manual)
-- ══════════════════════════════════════════════════════════════════

/-- Lean compilation pipeline: Code.lean → Syntax Tree → Core Type Theory → Executable -/
def pipelineDiagram : Diagram Empty :=
  let result :=
    Diagram.hsep (align := .bottom) 8 [.withTextColor .green (.text "✔"), .text "/", .text "✖"]
      |>.withFontBold true
      |>.withFontSize 20
      |>.pad 8
      |>.namedWithAnchors `result
  let codeLabel :=
    Diagram.text "Code.lean"
      (style := { fontFamily := "monospace", fontSize := some 12 })
      |>.pad 12
  let code :=
    Diagram.paper
      (name := `source)
      (label := some codeLabel)
      (width := some 80)
      (height := some 100)
      |>.withFillColor .white
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
  lbl (s : String) : Option (Label Empty) :=
    some { label := .text s { fontSize := (10 : Float) }, upright := true }
  box (name : Lean.Name) (label : String) (mono := false) : Diagram Empty :=
    Diagram.text label { fontSize := (12 : Float) }
      |> (if mono then Diagram.withFontFamily "monospace" else id)
      |>.pad 12
      |>.filledFrame (stroke := { color := Color.black, width := (1 : Float) }) (cornerRadius := 6)
      |>.withFillColor .white
      |>.namedWithAnchors name

#diagram pipelineDiagram

#diagram Diagram.text "a\nabcdef\nqf\nabcd\nabcdefg" |>.showEnvelope


def testVisual_pipeline : IO Unit :=
  testVisualWrite "pipeline.svg" pipelineDiagram (padding := 20)

-- ══════════════════════════════════════════════════════════════════
-- String memory layout (from Lean reference manual)
-- ══════════════════════════════════════════════════════════════════

/-- Memory layout of lean_string: m_header | m_size | m_capacity | m_length | m_data | '\0' -/
def stringLayoutDiagram : Diagram Empty :=
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
    (Diagram.vsep 1 [txt "String data",
      Diagram.hsep 3 [.text "char" { fontSize := (8 : Float) }, txt "array"]])
  let nulCol := field `nul "'\\0'" 30
  let styled := [headerCol, sizeCol, capCol, lenCol, dataCol, nulCol].map
    (·|>.withFillColor .white |>.withFontFamily "monospace" |>.withFontSize 10)
  Diagram.hsep 0 styled (align := .top)
where
  txt (s : String) : Diagram Empty :=
    Diagram.text s { fontSize := (8 : Float) } |>.withFontFamily "sans-serif"
  /-- Stacks a description line above a type line. -/
  twoLine (description typeLine : String) : Diagram Empty :=
    Diagram.vsep 1 [txt description, .text typeLine { fontSize := (8 : Float) }]
  field (name : Lean.Name) (label : String) (w : Float) : Diagram Empty :=
    Diagram.atop
      (Diagram.rect w 28 (name := name))
      (.text label)
  /-- Builds a field box with a curly brace and label below it. -/
  fieldWithBrace (name : Lean.Name) (label : String) (w : Float)
      (braceDepth braceGap : Float) (braceLabel : Diagram Empty) : Diagram Empty :=
    let box := field name label w
    let brace := Diagram.curlyBrace (w - 8) (depth := braceDepth) (label := some braceLabel)
      |>.withFontSize 8
    let braceEnv := brace.getEnvelope
    let excess := braceEnv Vec2.east - w / 2
    let brace := if excess > 0 then brace |>.padLeft (-excess) |>.padRight (-excess) else brace
    Diagram.vsep braceGap [box, brace]

#diagram stringLayoutDiagram

def testVisual_stringLayout : IO Unit :=
  testVisualWrite "string-layout.svg" stringLayoutDiagram


-- ══════════════════════════════════════════════════════════════════
-- Lake workspace (from Lean reference manual)
-- ══════════════════════════════════════════════════════════════════

/-- Lake workspace hierarchy from the Lean reference manual. -/
def lakeWorkspaceDiagram : Diagram Empty :=
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
  let dots : Diagram Empty := .text "⋯" { fontSize := (14 : Float) }
  let packages := borderedBox "Packages" <|
    Diagram.vsep 8 [Diagram.hsep 12 [dep1, dep2], dots] (align := .left)
  let artifacts := borderedBox "Artifacts" <|
    items ["Built libraries", "Built executables"]
  let lakeDir := borderedBox "Lake Directory (.lake)" <|
    Diagram.vsep 10 [packages, artifacts] (align := .left)
  borderedBox "Workspace" <|
    Diagram.vsep 10 [toolchain, rootPkg, lakeDir] (align := .left)
where
  txt (s : String) (size : Float := 10) : Diagram Empty :=
    .text s { fontSize := size, anchor := TextAnchor.start }
  bold (s : String) (size : Float := 11) : Diagram Empty :=
    .text s { fontSize := size, bold := true, anchor := TextAnchor.start }
  mono (s : String) (size : Float := 10) : Diagram Empty :=
    .text s { fontSize := size, fontFamily := "monospace", anchor := TextAnchor.start }
  items (ss : List String) (size : Float := 10) : Diagram Empty :=
    Diagram.vsep 3 (ss.map fun s => txt s size) (align := .left)
  borderedBox (title : String) (content : Diagram Empty)
      (titleSize : Float := 11) (pad : Float := 8) : Diagram Empty :=
    Diagram.vsep 4 [bold title titleSize, content] (align := .left)
      |>.pad pad |>.frame (padding := 2) (cornerRadius := 4)

#diagram lakeWorkspaceDiagram

def testVisual_lakeWorkspace : IO Unit :=
  testVisualWrite "lake-workspace.svg" lakeWorkspaceDiagram

-- ══════════════════════════════════════════════════════════════════
-- Coercion chain (from Lean reference manual)
-- ══════════════════════════════════════════════════════════════════

/-- Coercion chain diagram from the Lean reference manual (coe-chain.tex). -/
def coeChainDiagram : Diagram Empty :=
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
  let orLabel : Diagram Empty :=
    Diagram.text "or" { fontSize := (10 : Float), italic := true } |>.pad 3 |>.namedWithAnchors `or
  let coeTLabel : Diagram Empty := mono "CoeT" (name := `CoeT)
  let lineStroke : FullStroke := { color := Color.black, width := 1, lineCap := .butt, lineJoin := .miter, dash := .solid }
  Diagram.vsep 12 [withCoeDep, orLabel, coeTLabel]
    |>.connectL `CoeHTCT.south `or.west (stroke := lineStroke)
    |>.connectL `CoeDep.south `or.east (stroke := lineStroke)
    |>.connectL `or.south `CoeT.north (stroke := lineStroke)
where
  mono (s : String) (name : Option Lean.Name := none) : Diagram Empty :=
    .text s { fontSize := (10 : Float), fontFamily := "monospace" } (name := name)

#diagram coeChainDiagram

def testVisual_coeChain : IO Unit :=
  testVisualWrite "coe-chain.svg" coeChainDiagram

#diagram (Diagram.circle 20 (fill := {color := Color.transparent})).showEnvelope

#diagram (Diagram.rect 20 30 (fill := { color := Color.transparent } )).showEnvelope

#diagram (Diagram.roundedRect 20 30 3 (fill := { color := Color.transparent } )).showEnvelope

#diagram (Diagram.text "foo").showEnvelope

#diagram Diagram.hsep 30 [.circle 30, .rect 10 50] |>.withFillColor ⟨0, 0, 0, 0⟩ |>.showEnvelope |>.showOrigin

#diagram Diagram.polygon 5 10

#diagram Diagram.paper (label := some <| Diagram.text "Code.lean" { fontSize := (12 : Float) })

#diagram Diagram.paper (width := some 80)

#diagram Diagram.paper (width := some 80) (height := some 60) |>.withFillColor .white

open Diagram in
def paperTest : Diagram Empty :=
  vsep 20 [
    hsep 20 [
      paper (label := some <| text "Code"),
      paper (width := some 30)
    ],
    hsep 20 [
      withFillColor .white <|
      paper (width := some 30) (height := some 20),
      paper (width := some 20) (cornerFold := 0.75)
    ]
  ]

#diagram paperTest

/-- warning: #diagram: paper: cornerFold=1.500000 is outside 0–1, clamped to 1.000000 -/
#guard_msgs in
#diagram Diagram.paper (cornerFold := 1.5)

#diagram fun (fold : Slider "fold" 0 1 0.25) =>
  Diagram.paper (width := some 20) (cornerFold := fold)

-- ══════════════════════════════════════════════════════════════════
-- Stars with different point counts and dash patterns
-- ══════════════════════════════════════════════════════════════════

/-- Grid of stars: rows = point counts (1,2,3,5,6), columns = dash patterns. -/
def starsDiagram : Diagram Empty :=
  let pointCounts := [1, 2, 3, 5, 6]
  let dashes : List StrokeDash := [.solid, .dashed, .dotted, .dashDot]
  let dashLabels := ["solid", "dashed", "dotted", "dashDot"]
  let outerR := 20.0
  let innerR := 10.0
  let fill : Fill := { color := some { r := 255, g := 230, b := 100 } }
  -- Column headers
  let headers := Diagram.hsep 20 (dashLabels.map fun l =>
    Diagram.withEnvelope (Envelope.ofRect 25 8)
      (.text l { fontSize := (9 : Float) }))
  -- Row label + stars for each point count
  let rows := pointCounts.map fun n =>
    let label := Diagram.withEnvelope (Envelope.ofRect 12 25)
      (Diagram.text s!"{n}pt" { fontSize := (9 : Float) })
    let cells := dashes.map fun dash =>
      Diagram.withEnvelope (Envelope.ofRect 25 25)
        (Diagram.star n outerR innerR
          (fill := fill)
          (stroke := { color := Color.black, width := (1.5 : Float), dash := some dash }))
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
def starAnchorsDiagram : Diagram Empty :=
  let fill : Fill := { color := some { r := 255, g := 230, b := 100 } }
  let stroke : Stroke := { color := Color.black, width := (1.5 : Float) }
  let s7 := Diagram.star 7 30 15 (fill := fill) (stroke := stroke) (name := some `star7)
  let s10 := Diagram.star 10 30 15 (fill := fill) (stroke := stroke) (name := some `star10)
  let s13 := Diagram.star 13 30 15 (fill := fill) (stroke := stroke) (name := some `star13)
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

-- ══════════════════════════════════════════════════════════════════
-- Ellipse
-- ══════════════════════════════════════════════════════════════════

/-- Ellipses with various aspect ratios. -/
def ellipseDiagram : Diagram Empty :=
  let fill : Fill := { color := some { r := 180, g := 210, b := 255 } }
  let stroke : Stroke := { color := Color.black, width := (1.5 : Float) }
  Diagram.hsep 15 [
    Diagram.ellipse 30 15 (fill := fill) (stroke := stroke),
    Diagram.ellipse 15 30 (fill := fill) (stroke := stroke),
    Diagram.ellipse 25 25 (fill := fill) (stroke := stroke),
    Diagram.ellipse 35 10 (fill := fill) (stroke := { stroke with dash := StrokeDash.dashed })
  ]

#diagram ellipseDiagram

def testVisual_ellipse : IO Unit :=
  testVisualWrite "ellipse.svg" ellipseDiagram

-- ══════════════════════════════════════════════════════════════════
-- Transforms: scale, rotate, hflip, vflip
-- ══════════════════════════════════════════════════════════════════

/-- A labeled arrow shape used to show transform effects. -/
private def arrowShape : Diagram Empty :=
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
    (fill := { color := some { r := 100, g := 180, b := 100 } })
    (stroke := { color := Color.black, width := (1 : Float) })

/-- Demonstrates scale, rotate, hflip, vflip on an arrow shape. -/
def transformsDiagram : Diagram Empty :=
  let label (s : String) : Diagram Empty :=
    .text s { fontSize := (9 : Float) }
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

-- ══════════════════════════════════════════════════════════════════
-- Ghost and refocus
-- ══════════════════════════════════════════════════════════════════

/-- Demonstrates ghost and refocus. -/
def ghostRefocusDiagram : Diagram Empty :=
  let red : Fill := { color := Color.red }
  let blue : Fill := { color := Color.blue }
  let green : Fill := { color := some { r := 0, g := 160, b := 0, a := 0.8 } }
  let label (s : String) : Diagram Empty :=
    .text s { fontSize := (9 : Float) }
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

-- ══════════════════════════════════════════════════════════════════
-- Cellophane, clip, pinOver, pinUnder
-- ══════════════════════════════════════════════════════════════════

/-- Demonstrates cellophane (opacity), clip, pinOver, and pinUnder. -/
def cellophaneClipDiagram : Diagram Empty :=
  let label (s : String) : Diagram Empty :=
    .text s { fontSize := (9 : Float) }
  let red : Fill := { color := Color.red }
  let blue : Fill := { color := Color.blue }
  let green : Fill := { color := some { r := 0, g := 160, b := 0 } }
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
    (Diagram.transform (Matrix.translate 10 10)
      (Diagram.rect 40 40 (fill := green)))
  let clipped := Diagram.clipCircle 25 checker
  let clipRow := Diagram.hsep 30 [
    Diagram.vsep 5 [label "unclipped", checker],
    Diagram.vsep 5 [label "clip circle r=25", clipped]
  ] (align := .top)
  -- Row 3: pinOver and pinUnder
  let base1 := Diagram.hsep 40 [
    Diagram.circle 20 (fill := blue) (name := some `left),
    Diagram.circle 20 (fill := green) (name := some `right)
  ]
  let dot := Diagram.circle 5 (fill := red)
  let pinned := base1
    |> Diagram.pinOver `left dot
    |> Diagram.pinOver `right dot
  let semiBlue : Fill := { color := some { r := 0, g := 0, b := 255, a := 0.5 } }
  let semiGreen : Fill := { color := some { r := 0, g := 160, b := 0, a := 0.5 } }
  let base2 := Diagram.hsep 40 [
    Diagram.circle 20 (fill := semiBlue) (name := some `left2),
    Diagram.circle 20 (fill := semiGreen) (name := some `right2)
  ]
  let underDot := Diagram.circle 12 (fill := { color := some { r := 255, g := 200, b := 0 } })
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

def visualTests : List (String × IO Unit) := [
  ("Visual/roundedRects", testVisual_roundedRects),
  ("Visual/pipeline", testVisual_pipeline),
  ("Visual/stringLayout", testVisual_stringLayout),
  ("Visual/lakeWorkspace", testVisual_lakeWorkspace),
  ("Visual/coeChain", testVisual_coeChain),
  ("Visual/stars", testVisual_stars),
  ("Visual/starAnchors", testVisual_starAnchors),
  ("Visual/ellipse", testVisual_ellipse),
  ("Visual/transforms", testVisual_transforms),
  ("Visual/ghostRefocus", testVisual_ghostRefocus),
  ("Visual/cellophaneClip", testVisual_cellophaneClip)
]
