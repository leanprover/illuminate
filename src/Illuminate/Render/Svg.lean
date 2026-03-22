/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/

import Illuminate.Geometry
import Illuminate.Style
import Illuminate.Render.DrawCmd


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
  else s.dropEndWhile '0' |>.dropEndWhile '.' |>.copy

/-- Converts a Color to an SVG color string. -/
private def colorToSvg (c : Color) : String :=
  s!"rgb({c.r},{c.g},{c.b})"

/-- Converts a Color's alpha to an SVG opacity attribute string. -/
private def opacityAttr (attrName : String) (c : Color) : String :=
  if c.a < 1.0 then s!" {attrName}=\"{fmtNum c.a}\""
  else ""

/-- Converts PathData to an SVG `d` attribute string. -/
def pathDataToD (pd : PathData) : String :=
  pd.commands.foldl (init := "") fun acc cmd =>
    let seg := match cmd with
      | .moveTo p => s!"M{fmtNum p.x} {fmtNum p.y}"
      | .lineTo p => s!"L{fmtNum p.x} {fmtNum p.y}"
      | .curveTo c1 c2 ep =>
        s!"C{fmtNum c1.x} {fmtNum c1.y} {fmtNum c2.x} {fmtNum c2.y} {fmtNum ep.x} {fmtNum ep.y}"
      | .closePath => "Z"
    if acc.isEmpty then seg else acc ++ " " ++ seg

/-- Converts a LineCap to its SVG string. -/
private def lineCapToSvg : LineCap → String
  | .butt   => "butt"
  | .round  => "round"
  | .square => "square"

/-- Converts a LineJoin to its SVG string. -/
private def lineJoinToSvg : LineJoin → String
  | .miter => "miter"
  | .round => "round"
  | .bevel => "bevel"

/-- Converts a StrokeDash to an SVG `stroke-dasharray` attribute string. -/
private def dashToSvg (dash : StrokeDash) (w : Float) : String :=
  match dash with
  | .solid => ""
  | .dashed => s!" stroke-dasharray=\"{fmtNum (w * 4)} {fmtNum (w * 2)}\""
  | .dotted => s!" stroke-dasharray=\"{fmtNum w} {fmtNum w}\""
  | .dashDot => s!" stroke-dasharray=\"{fmtNum (w * 4)} {fmtNum w} {fmtNum w} {fmtNum w}\""

/-- Converts a Matrix to an SVG `transform` attribute value. -/
private def matrixToSvg (m : Matrix) : String :=
  s!"matrix({fmtNum m.a},{fmtNum m.c},{fmtNum m.b},{fmtNum m.d},{fmtNum m.tx},{fmtNum m.ty})"

/-- Renders a single DrawCmd to an SVG fragment. The `clipPrefix` distinguishes
    clip-path IDs across independently compiled diagrams on the same page. -/
def renderCmd {β : Type} [BackendRender β] (cmd : DrawCmd β) (clipPrefix : String := "") : String :=
  match cmd with
  | .fillPath pd fill =>
    let d := pathDataToD pd
    match fill with
    | .none => ""
    | .solid fs =>
      let cs := colorToSvg fs.color
      let op := opacityAttr "fill-opacity" fs.color
      s!"<path d=\"{d}\" fill=\"{cs}\"{op}/>"
  | .strokePath pd stroke =>
    let d := pathDataToD pd
    let c := colorToSvg stroke.color
    let op := opacityAttr "stroke-opacity" stroke.color
    let cap := lineCapToSvg stroke.lineCap
    let join := lineJoinToSvg stroke.lineJoin
    let da := dashToSvg stroke.dash stroke.width
    s!"<path d=\"{d}\" fill=\"none\" stroke=\"{c}\" stroke-width=\"{fmtNum stroke.width}\" stroke-linecap=\"{cap}\" stroke-linejoin=\"{join}\"{op}{da}/>"
  | .drawTextRun s style pos =>
    let fontWeight := if style.bold then "bold" else "normal"
    let fontStyle := if style.italic then "italic" else "normal"
    let c := colorToSvg style.color
    let anchor := match style.anchor with
      | .start => "start"
      | .«end» => "end"
      | .middle => "middle"
    let attrs := s!"x=\"{fmtNum pos.x}\" y=\"{fmtNum pos.y}\" font-family=\"{style.fontFamily}\" font-size=\"{fmtNum style.fontSize}\" font-weight=\"{fontWeight}\" font-style=\"{fontStyle}\" fill=\"{c}\" text-anchor=\"{anchor}\" dominant-baseline=\"central\" transform=\"scale(1,-1)\""
    let lines := s.splitOn "\n"
    if lines.length <= 1 then
      s!"<text {attrs}>{escapeXml s}</text>"
    else
      let lineHeight := style.fontSize * 1.2
      let totalH := lineHeight * (lines.length - 1).toFloat
      let startY := -totalH / 2
      let (_, tspans) := lines.foldl (fun (i, acc) line =>
        let dy := if i == 0 then startY else lineHeight
        let span := s!"<tspan x=\"{fmtNum pos.x}\" dy=\"{fmtNum dy}\">{escapeXml line}</tspan>"
        (i + 1, acc ++ span)) (0, "")
      s!"<text {attrs}>{tspans}</text>"
  | .pushTransform m =>
    s!"<g transform=\"{matrixToSvg m}\">"
  | .popTransform => "</g>"
  | .pushAnnotation tag =>
    s!"<g data-anno-id=\"{tag}\">"
  | .popAnnotation => "</g>"
  | .pushOpacity α =>
    s!"<g opacity=\"{fmtNum α}\">"
  | .popOpacity => "</g>"
  | .pushClip pd clipId =>
    let d := pathDataToD pd
    let cid := s!"clip{clipPrefix}{clipId}"
    s!"<defs><clipPath id=\"{cid}\"><path d=\"{d}\"/></clipPath></defs><g clip-path=\"url(#{cid})\">"
  | .popClip => "</g>"
  | .pushForeign tag => BackendRender.renderOpen tag
  | .popForeign tag => BackendRender.renderClose tag

/-- Renders an array of draw commands to a complete SVG document string.
    The `clipPrefix` distinguishes clip-path IDs when multiple SVGs share a page. -/
def render {β : Type} [BackendRender β] (cmds : Array (DrawCmd β)) (viewBox : ViewBox)
    (clipPrefix : String := "") : String :=
  let header := s!"<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"{fmtNum viewBox.minX} {fmtNum viewBox.minY} {fmtNum viewBox.width} {fmtNum viewBox.height}\">"
  let body := cmds.foldl (fun acc cmd => acc ++ "\n" ++ renderCmd cmd clipPrefix) ""
  -- Flip y-axis: SVG y points down, diagram y points up
  header ++ "\n<g transform=\"scale(1,-1)\">" ++ body ++ "\n</g>\n</svg>"
