/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/
module
public import Illuminate.Render.DrawCmd
import Lean.DocString.Syntax
meta import Lean.Parser.Term
public section


namespace Illuminate

/-- SVG viewBox specification: the visible region of the coordinate space. -/
structure ViewBox where
  /-- Left edge of the visible region. -/
  minX : Float
  /-- Top edge of the visible region (in SVG coordinates, before y-flip). -/
  minY : Float
  /-- Width of the visible region. -/
  width : Float
  /-- Height of the visible region. -/
  height : Float
deriving Repr, BEq, Inhabited

/-- Fallback viewBox used when a diagram has no envelope. -/
def ViewBox.fallback : ViewBox :=
  { minX := -320, minY := -240, width := 640, height := 480 }

/--
Controls how a backend-specific foreign value renders to SVG.
Each backend type provides open and close tag strings.
-/
class BackendRender (β : Type) where
  /-- Renders the opening SVG element for a foreign value. -/
  renderOpen : β → String
  /-- Renders the closing SVG element for a foreign value. -/
  renderClose : β → String

instance : BackendRender Empty where
  renderOpen e := nomatch e
  renderClose e := nomatch e

/-- Escapes special XML characters in a string. -/
def escapeXml (s : String) : String :=
  s.foldl (init := "") fun acc c =>
    acc ++ match c with
    | '&' => "&amp;"
    | '<' => "&lt;"
    | '>' => "&gt;"
    | '"' => "&quot;"
    | '\'' => "&#39;"
    | c => c.toString

namespace Svg

/-- Formats a Float with reasonable precision, trimming trailing zeros. -/
def fmtNum (f : Float) : String :=
  let scaled := (f * 10000).round / 10000
  let s := toString scaled
  if !s.contains '.' then s
  else
    let trimmed := s.dropEndWhile '0' |>.dropEndWhile '.'
    if trimmed.isEmpty then "0" else trimmed.copy

/-- Converts a Color to an SVG color string. -/
private def colorToSvg (c : Color) : String :=
  s!"rgb({c.r},{c.g},{c.b})"

/-- Converts {name}`PathData` to an SVG {lit}`d` attribute string. -/
def pathDataToD (pd : PathData) : String :=
  pd.commands.foldl (init := "") fun acc cmd =>
    let seg := match cmd with
      | .moveTo p => s!"M{fmtNum p.x} {fmtNum p.y}"
      | .lineTo p => s!"L{fmtNum p.x} {fmtNum p.y}"
      | .curveTo c1 c2 ep =>
        s!"C{fmtNum c1.x} {fmtNum c1.y} {fmtNum c2.x} {fmtNum c2.y} {fmtNum ep.x} {fmtNum ep.y}"
      | .arcTo rx ry rot largeArc sweep ep =>
        let la := if largeArc then "1" else "0"
        let sw := if sweep then "1" else "0"
        s!"A{fmtNum rx} {fmtNum ry} {fmtNum rot} {la} {sw} {fmtNum ep.x} {fmtNum ep.y}"
      | .closePath => "Z"
    if acc.isEmpty then seg else acc ++ " " ++ seg

/-- Converts a {name}`LineCap` to its SVG string. -/
private def lineCapToSvg : LineCap → String
  | .butt   => "butt"
  | .round  => "round"
  | .square => "square"

/-- Converts a {name}`LineJoin` to its SVG string. -/
private def lineJoinToSvg : LineJoin → String
  | .miter => "miter"
  | .round => "round"
  | .bevel => "bevel"

/-- Converts a {name}`StrokeDash` to the value of an SVG {lit}`stroke-dasharray` attribute. -/
private def dashArrayValue (dash : StrokeDash) (w : Float) : String :=
  match dash with
  | .solid => "none"
  | .dashed => s!"{fmtNum (w * 4)} {fmtNum (w * 2)}"
  | .dotted => s!"{fmtNum w} {fmtNum w}"
  | .dashDot => s!"{fmtNum (w * 4)} {fmtNum w} {fmtNum w} {fmtNum w}"

/-- Converts a {name}`Matrix` to an SVG {lit}`transform` attribute value. -/
private def matrixToSvg (m : Matrix) : String :=
  s!"matrix({fmtNum m.a},{fmtNum m.c},{fmtNum m.b},{fmtNum m.d},{fmtNum m.tx},{fmtNum m.ty})"

/-- Converts a {name}`TextAnchor` to its SVG string. -/
private def anchorToSvg : TextAnchor → String
  | .start => "start"
  | .«end» => "end"
  | .middle => "middle"

/-!
# Gradient SVG helpers
-/

/-- Converts a {name}`SpreadMethod` to its SVG {lit}`spreadMethod` attribute value. -/
private def spreadToSvg : SpreadMethod → String
  | .pad => "pad"
  | .reflect => "reflect"
  | .repeat => "repeat"

/-- Renders a {name}`GradientStop` as an SVG {lit}`<stop>` element. -/
private def stopToSvg (s : GradientStop) : String :=
  s!"<stop offset=\"{fmtNum s.offset}\" stop-color=\"{colorToSvg s.color}\" stop-opacity=\"{fmtNum s.color.a}\"/>"

/-- Renders all stops of a gradient as concatenated SVG {lit}`<stop>` elements. -/
private def stopsToSvg (stops : Array GradientStop) : String :=
  stops.foldl (fun acc s => acc ++ stopToSvg s) ""

/-- Computes the SVG element ID for a gradient from its positional index and clip prefix. -/
def gradientIdOf (gi : Nat) (clipPrefix : String) : String :=
  s!"grad{clipPrefix}{gi}"


/-!
# Shared attribute extraction
-/

/--
Extracted SVG attributes for a draw command, used by both SVG rendering
and animation parameter extraction.

A single draw command may produce multiple SVG elements (e.g. a gradient
container plus its {lit}`<stop>` children, or a {lit}`<text>` plus its {lit}`<tspan>` children).
Each element gets its own {lit}`data-e` index so the animation player can patch it.
-/
structure CmdAttrInfo where
  /-- Number of SVG elements this command produces (0 for close tags, 1+ for elements). -/
  elemCount : Nat
  /-- Attribute name-value pairs. Text content uses the pseudo-attribute {lit}`"textContent"`. -/
  attrs : Array (String × String)
  /--
  Maps each entry in {name (full := CmdAttrInfo.attrs)}`attrs` to a local element index
  (0-based within this command). When empty, all attrs belong to element 0.
  -/
  attrElemMap : Array Nat := #[]
deriving Repr, Inhabited

/--
Extracts SVG attribute name-value pairs from a draw command.

This is the single source of truth for attribute values shared by
{name (scope := "Illuminate.Render.Svg")}`renderCmd` (SVG rendering) and animation parameter extraction.
Each draw command variant always produces the same number of attribute
pairs regardless of values, so frame-to-frame comparison by position is safe.
-/
def drawCmdAttrs {β : Type} [BackendRender β]
    (cmd : DrawCmd β) (clipPrefix : String := "") : CmdAttrInfo :=
  match cmd with
  | .fillPath pd fill gradIdx =>
    match fill with
    | .none => { elemCount := 0, attrs := #[] }
    | .solid fs => { elemCount := 1, attrs := #[
        ("d", pathDataToD pd),
        ("fill", colorToSvg fs.color),
        ("fill-opacity", fmtNum fs.color.a)] }
    | .gradient _ =>
      let fillVal := match gradIdx with
        | some gi => s!"url(#{gradientIdOf gi clipPrefix})"
        | none => ""
      { elemCount := 1, attrs := #[
        ("d", pathDataToD pd),
        ("fill", fillVal),
        ("fill-opacity", "1")] }
  | .strokePath pd stroke =>
    let w := stroke.width
    { elemCount := 1, attrs := #[
      ("d", pathDataToD pd),
      ("fill", "none"),
      ("stroke", colorToSvg stroke.color),
      ("stroke-width", fmtNum w),
      ("stroke-linecap", lineCapToSvg stroke.lineCap),
      ("stroke-linejoin", lineJoinToSvg stroke.lineJoin),
      ("stroke-opacity", fmtNum stroke.color.a),
      ("stroke-dasharray", dashArrayValue stroke.dash w)] }
  | .drawTextRun s style pos =>
    let lines := s.splitOn "\n"
    if lines.length <= 1 then
      -- Single-line: textContent lives on the outer <text>
      { elemCount := 1, attrs := #[
        ("textContent", s),
        ("x", fmtNum pos.x),
        ("y", fmtNum pos.y),
        ("font-family", style.fontFamily),
        ("font-size", fmtNum style.fontSize),
        ("font-weight", if style.bold then "bold" else "normal"),
        ("font-style", if style.italic then "italic" else "normal"),
        ("fill", colorToSvg style.color),
        ("text-anchor", anchorToSvg style.anchor),
        ("dominant-baseline", "central"),
        ("transform", "scale(1,-1)")] }
    else
      -- Multi-line: each <tspan> is a separate patchable element
      let lineHeight := style.fontSize * 1.2
      let totalH := lineHeight * (lines.length - 1).toFloat
      let startY := -totalH / 2
      let textAttrs : Array (String × String) := #[
        ("x", fmtNum pos.x),
        ("y", fmtNum pos.y),
        ("font-family", style.fontFamily),
        ("font-size", fmtNum style.fontSize),
        ("font-weight", if style.bold then "bold" else "normal"),
        ("font-style", if style.italic then "italic" else "normal"),
        ("fill", colorToSvg style.color),
        ("text-anchor", anchorToSvg style.anchor),
        ("dominant-baseline", "central"),
        ("transform", "scale(1,-1)")]
      let textElemMap := (Array.replicate textAttrs.size 0 : Array Nat)
      let (_, tspanAttrs, tspanElemMap) := lines.foldl
        (fun (i, attrs, emap) line =>
          let dy := if i == 0 then startY else lineHeight
          let newAttrs := #[
            ("x", fmtNum pos.x),
            ("dy", fmtNum dy),
            ("textContent", line)]
          let newEmap := Array.replicate newAttrs.size (i + 1)
          (i + 1, attrs ++ newAttrs, emap ++ newEmap))
        (0, #[], #[])
      { elemCount := 1 + lines.length,
        attrs := textAttrs ++ tspanAttrs,
        attrElemMap := textElemMap ++ tspanElemMap }
  | .drawStyledText lines anchor pos =>
    let textAttrs : Array (String × String) := #[
      ("x", fmtNum pos.x),
      ("y", fmtNum pos.y),
      ("text-anchor", anchorToSvg anchor),
      ("dominant-baseline", "central"),
      ("transform", "scale(1,-1)")]
    let textElemMap := (Array.replicate textAttrs.size 0 : Array Nat)
    -- Flatten all spans across all lines into a linear sequence of elements
    let allSpans := lines.foldl (fun acc lineSpans => acc ++ lineSpans) #[]
    let nLines := lines.size
    if nLines <= 1 then
      -- Single-line: tspans flow horizontally, no positional attrs
      let (_, spanAttrs, spanElemMap) := allSpans.foldl
        (fun (i, attrs, emap) (style, s) =>
          let weight := if style.bold then "bold" else "normal"
          let slant := if style.italic then "italic" else "normal"
          let newAttrs : Array (String × String) := #[
            ("alignment-baseline", "central"),
            ("font-family", escapeXml style.fontFamily),
            ("font-size", fmtNum style.fontSize),
            ("font-weight", weight),
            ("font-style", slant),
            ("fill", colorToSvg style.color),
            ("textContent", s)]
          let newEmap := Array.replicate newAttrs.size (i + 1)
          (i + 1, attrs ++ newAttrs, emap ++ newEmap))
        (0, #[], #[])
      { elemCount := 1 + allSpans.size,
        attrs := textAttrs ++ spanAttrs,
        attrElemMap := textElemMap ++ spanElemMap }
    else
      -- Multi-line: first span of each line gets x and dy
      let maxFontSize := lines.foldl (fun acc spans =>
        spans.foldl (fun a (st, _) => max a st.fontSize) acc) 0
      let lineHeight := maxFontSize * 1.2
      let totalH := lineHeight * (nLines - 1).toFloat
      let startY := -totalH / 2
      let (_, _, spanAttrs, spanElemMap) := lines.foldl
        (fun (lineIdx, elemOff, attrs, emap) lineSpans =>
          let dy := if lineIdx == 0 then startY else lineHeight
          let (_, elemOff', attrs', emap') := lineSpans.foldl
            (fun (spanIdx, eOff, acc, em) (style, s) =>
              let weight := if style.bold then "bold" else "normal"
              let slant := if style.italic then "italic" else "normal"
              let posAttrs : Array (String × String) :=
                if spanIdx == 0 then #[("x", fmtNum pos.x), ("dy", fmtNum dy)]
                else #[]
              let styleAttrs : Array (String × String) := #[
                ("alignment-baseline", "central"),
                ("font-family", escapeXml style.fontFamily),
                ("font-size", fmtNum style.fontSize),
                ("font-weight", weight),
                ("font-style", slant),
                ("fill", colorToSvg style.color),
                ("textContent", s)]
              let newAttrs := posAttrs ++ styleAttrs
              let newEmap := Array.replicate newAttrs.size eOff
              (spanIdx + 1, eOff + 1, acc ++ newAttrs, em ++ newEmap))
            (0, elemOff, attrs, emap)
          (lineIdx + 1, elemOff', attrs', emap'))
        (0, 1, #[], #[])
      { elemCount := 1 + allSpans.size,
        attrs := textAttrs ++ spanAttrs,
        attrElemMap := textElemMap ++ spanElemMap }
  | .pushTransform m => { elemCount := 1, attrs := #[("transform", matrixToSvg m)] }
  | .pushAnnotation tag => { elemCount := 1, attrs := #[("data-anno-id", toString tag)] }
  | .pushOpacity α => { elemCount := 1, attrs := #[("opacity", fmtNum α)] }
  | .pushClip pd clipId =>
    let cid := s!"clip{clipPrefix}{clipId}"
    { elemCount := 1, attrs := #[("d", pathDataToD pd), ("id", cid)] }
  | .pushForeign f => { elemCount := 1, attrs := #[("data-foreign", BackendRender.renderOpen f)] }
  | .popForeign f => { elemCount := 0, attrs := #[("data-foreign", BackendRender.renderClose f)] }
  | .defGradient gi g =>
    let stops := match g with
      | .linear _ _ _ _ stops _ | .radial _ _ _ _ _ _ stops _ => stops
    let gradAttrs := match g with
      | .linear x1 y1 x2 y2 _ spread => #[
          ("id", gradientIdOf gi clipPrefix),
          ("x1", fmtNum x1), ("y1", fmtNum y1),
          ("x2", fmtNum x2), ("y2", fmtNum y2),
          ("gradientUnits", "userSpaceOnUse"),
          ("spreadMethod", spreadToSvg spread)]
      | .radial cx cy r fx fy fr _ spread => #[
          ("id", gradientIdOf gi clipPrefix),
          ("cx", fmtNum cx), ("cy", fmtNum cy), ("r", fmtNum r),
          ("fx", fmtNum fx), ("fy", fmtNum fy), ("fr", fmtNum fr),
          ("gradientUnits", "userSpaceOnUse"),
          ("spreadMethod", spreadToSvg spread)]
    let gradElemMap := (Array.replicate gradAttrs.size 0 : Array Nat)
    -- Each <stop> is a separate patchable child element
    let (_, stopAttrs, stopElemMap) := stops.foldl
      (fun (i, attrs, emap) s =>
        let newAttrs : Array (String × String) := #[
          ("offset", fmtNum s.offset),
          ("stop-color", colorToSvg s.color),
          ("stop-opacity", fmtNum s.color.a)]
        let newEmap := Array.replicate newAttrs.size (i + 1)
        (i + 1, attrs ++ newAttrs, emap ++ newEmap))
      (0, #[], #[])
    { elemCount := 1 + stops.size,
      attrs := gradAttrs ++ stopAttrs,
      attrElemMap := gradElemMap ++ stopElemMap }
  | .popTransform | .popAnnotation | .popOpacity | .popClip => { elemCount := 0, attrs := #[] }

/-- Joins attribute pairs into an SVG attribute string with a leading space. -/
private def renderAttrs (attrs : Array (String × String)) : String :=
  attrs.foldl (fun acc (k, v) => acc ++ s!" {k}=\"{v}\"") ""

/-- Looks up an attribute value by name, returning {lit}`""` if absent. -/
private def findAttr (attrs : Array (String × String)) (name : String) : String :=
  match attrs.find? (·.1 == name) with
  | some (_, v) => v
  | none => ""

/--
Renders a single {name}`DrawCmd` to an SVG fragment. The {name}`clipPrefix` distinguishes
clip-path IDs across independently compiled diagrams on the same page.

When {name}`elemTag` is provided, the patchable element gets a {lit}`data-e` attribute
with that value so the animation player can locate it by explicit ID.
-/
def renderCmd {β : Type} [BackendRender β] (cmd : DrawCmd β)
    (clipPrefix : String := "") (elemTag : Option Nat := none) : String :=
  let info := drawCmdAttrs cmd clipPrefix
  let eAttr := match elemTag with
    | some idx => s!" data-e=\"{idx}\""
    | none => ""
  match cmd with
  | .fillPath _ .none _ => ""
  | .fillPath _ (.solid _) _ | .fillPath _ (.gradient _) _ =>
    s!"<path{eAttr}{renderAttrs info.attrs}/>"
  | .strokePath .. =>
    s!"<path{eAttr}{renderAttrs info.attrs}/>"
  | .drawTextRun s style pos =>
    let lines := s.splitOn "\n"
    if lines.length <= 1 then
      let textAttrs := info.attrs.filter (·.1 != "textContent")
      let attrStr := renderAttrs textAttrs
      s!"<text{eAttr}{attrStr}>{escapeXml s}</text>"
    else
      -- Multi-line: outer <text> has no textContent; each <tspan> is a patchable element
      let textAttrStr := renderAttrs #[
        ("x", fmtNum pos.x), ("y", fmtNum pos.y),
        ("font-family", style.fontFamily),
        ("font-size", fmtNum style.fontSize),
        ("font-weight", if style.bold then "bold" else "normal"),
        ("font-style", if style.italic then "italic" else "normal"),
        ("fill", colorToSvg style.color),
        ("text-anchor", anchorToSvg style.anchor),
        ("dominant-baseline", "central"),
        ("transform", "scale(1,-1)")]
      let lineHeight := style.fontSize * 1.2
      let totalH := lineHeight * (lines.length - 1).toFloat
      let startY := -totalH / 2
      let (_, tspans) := lines.foldl (fun (i, acc) line =>
        let dy := if i == 0 then startY else lineHeight
        let tspanEAttr := match elemTag with
          | some baseIdx => s!" data-e=\"{baseIdx + 1 + i}\""
          | none => ""
        let span := s!"<tspan{tspanEAttr} x=\"{fmtNum pos.x}\" dy=\"{fmtNum dy}\">{escapeXml line}</tspan>"
        (i + 1, acc ++ span)) (0, "")
      s!"<text{eAttr}{textAttrStr}>{tspans}</text>"
  | .drawStyledText lines _anchor pos =>
    let textAttrStr := renderAttrs #[
      ("x", fmtNum pos.x), ("y", fmtNum pos.y),
      ("text-anchor", anchorToSvg _anchor),
      ("dominant-baseline", "central"),
      ("transform", "scale(1,-1)")]
    let nLines := lines.size
    if nLines <= 1 then
      -- Single line: tspans flow horizontally
      let spans := lines[0]?.getD #[]
      let (_, tspans) := spans.foldl (fun (i, acc) (style, s) =>
        let weight := if style.bold then "bold" else "normal"
        let slant := if style.italic then "italic" else "normal"
        let tspanEAttr := match elemTag with
          | some baseIdx => s!" data-e=\"{baseIdx + 1 + i}\""
          | none => ""
        let span := s!"<tspan{tspanEAttr} alignment-baseline=\"central\" font-family=\"{escapeXml style.fontFamily}\" font-size=\"{fmtNum style.fontSize}\" font-weight=\"{weight}\" font-style=\"{slant}\" fill=\"{colorToSvg style.color}\">{escapeXml s}</tspan>"
        (i + 1, acc ++ span)) (0, "")
      s!"<text{eAttr}{textAttrStr} xml:space=\"preserve\">{tspans}</text>"
    else
      -- Multi-line: first tspan of each line gets x and dy
      let maxFontSize := lines.foldl (fun acc spans =>
        spans.foldl (fun a (st, _) => max a st.fontSize) acc) 0
      let lineHeight := maxFontSize * 1.2
      let totalH := lineHeight * (nLines - 1).toFloat
      let startY := -totalH / 2
      let (_, _, tspans) := lines.foldl (fun (lineIdx, elemOff, acc) spans =>
        let dy := if lineIdx == 0 then startY else lineHeight
        let (_, elemOff', lineStr) := spans.foldl (fun (spanIdx, eOff, sacc) (style, s) =>
          let weight := if style.bold then "bold" else "normal"
          let slant := if style.italic then "italic" else "normal"
          let posAttrs := if spanIdx == 0 then
            s!" x=\"{fmtNum pos.x}\" dy=\"{fmtNum dy}\""
          else ""
          let tspanEAttr := match elemTag with
            | some baseIdx => s!" data-e=\"{baseIdx + eOff}\""
            | none => ""
          let span := s!"<tspan{tspanEAttr}{posAttrs} alignment-baseline=\"central\" font-family=\"{escapeXml style.fontFamily}\" font-size=\"{fmtNum style.fontSize}\" font-weight=\"{weight}\" font-style=\"{slant}\" fill=\"{colorToSvg style.color}\">{escapeXml s}</tspan>"
          (spanIdx + 1, eOff + 1, sacc ++ span)) (0, elemOff, "")
        (lineIdx + 1, elemOff', acc ++ lineStr)) (0, 1, "")
      s!"<text{eAttr}{textAttrStr} xml:space=\"preserve\">{tspans}</text>"
  | .pushTransform .. | .pushAnnotation .. | .pushOpacity .. =>
    s!"<g{eAttr}{renderAttrs info.attrs}>"
  | .pushClip .. =>
    let d := findAttr info.attrs "d"
    let cid := findAttr info.attrs "id"
    -- The data-e tag goes on the inner <path>, which is the patchable element
    -- whose `d` attribute the animation player needs to update.
    s!"<defs><clipPath id=\"{cid}\"><path{eAttr} d=\"{d}\"/></clipPath></defs><g clip-path=\"url(#{cid})\">"
  | .pushForeign tag => BackendRender.renderOpen tag
  | .popForeign tag => BackendRender.renderClose tag
  | .defGradient gi g =>
    let stops := match g with
      | .linear _ _ _ _ stops _ | .radial _ _ _ _ _ _ stops _ => stops
    let gradAttrStr := match g with
      | .linear x1 y1 x2 y2 _ spread => renderAttrs #[
          ("id", gradientIdOf gi clipPrefix),
          ("x1", fmtNum x1), ("y1", fmtNum y1),
          ("x2", fmtNum x2), ("y2", fmtNum y2),
          ("gradientUnits", "userSpaceOnUse"),
          ("spreadMethod", spreadToSvg spread)]
      | .radial cx cy r fx fy fr _ spread => renderAttrs #[
          ("id", gradientIdOf gi clipPrefix),
          ("cx", fmtNum cx), ("cy", fmtNum cy), ("r", fmtNum r),
          ("fx", fmtNum fx), ("fy", fmtNum fy), ("fr", fmtNum fr),
          ("gradientUnits", "userSpaceOnUse"),
          ("spreadMethod", spreadToSvg spread)]
    -- Each <stop> is a patchable child element with its own data-e
    let (_, stopsStr) := stops.foldl (fun (i, acc) s =>
      let stopEAttr := match elemTag with
        | some baseIdx => s!" data-e=\"{baseIdx + 1 + i}\""
        | none => ""
      (i + 1, acc ++ s!"<stop{stopEAttr} offset=\"{fmtNum s.offset}\" stop-color=\"{colorToSvg s.color}\" stop-opacity=\"{fmtNum s.color.a}\"/>"))
      (0, "")
    match g with
    | .linear .. =>
      s!"<defs><linearGradient{eAttr}{gradAttrStr}>{stopsStr}</linearGradient></defs>"
    | .radial .. =>
      s!"<defs><radialGradient{eAttr}{gradAttrStr}>{stopsStr}</radialGradient></defs>"
  | .popTransform | .popAnnotation | .popOpacity | .popClip => "</g>"

/--
Renders an array of draw commands to a complete SVG document string.
The {name}`clipPrefix` distinguishes clip-path IDs when multiple SVGs share a page.

When {name}`emitElemIdx` is true, each element that produces SVG output
gets a {lit}`data-e` attribute with its element index. The animation player uses
these to locate patchable elements by explicit ID rather than fragile DOM-walk order.
-/
def render {β : Type} [BackendRender β] (cmds : Array (DrawCmd β)) (viewBox : ViewBox)
    (clipPrefix : String := "") (emitElemIdx : Bool := false) : String :=
  let header := s!"<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"{fmtNum viewBox.minX} {fmtNum viewBox.minY} {fmtNum viewBox.width} {fmtNum viewBox.height}\">"
  let (body, _) := cmds.foldl (init := ("", 0)) fun (acc, ei) cmd =>
    let ec := (drawCmdAttrs cmd clipPrefix).elemCount
    let tag := if emitElemIdx && ec > 0 then some ei else none
    let fragment := renderCmd cmd clipPrefix tag
    (acc ++ "\n" ++ fragment, ei + ec)
  -- Flip y-axis: SVG y points down, diagram y points up
  header ++ "\n<g transform=\"scale(1,-1)\">" ++ body ++ "\n</g>\n</svg>"
