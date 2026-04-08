/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/
module
public import Illuminate.Geometry.PathData
public import Illuminate.Render.DrawCmd
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
-/
structure CmdAttrInfo where
  /-- Whether this command produces an SVG DOM element (vs. a close tag or nothing). -/
  producesElement : Bool
  /-- Attribute name-value pairs. Text content uses the pseudo-attribute {lit}`"textContent"`. -/
  attrs : Array (String × String)
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
    | .none => ⟨false, #[]⟩
    | .solid fs => ⟨true, #[
        ("d", pathDataToD pd),
        ("fill", colorToSvg fs.color),
        ("fill-opacity", fmtNum fs.color.a)]⟩
    | .gradient _ =>
      let fillVal := match gradIdx with
        | some gi => s!"url(#{gradientIdOf gi clipPrefix})"
        | none => ""
      ⟨true, #[
        ("d", pathDataToD pd),
        ("fill", fillVal),
        ("fill-opacity", "1")]⟩
  | .strokePath pd stroke =>
    let w := stroke.width
    ⟨true, #[
      ("d", pathDataToD pd),
      ("fill", "none"),
      ("stroke", colorToSvg stroke.color),
      ("stroke-width", fmtNum w),
      ("stroke-linecap", lineCapToSvg stroke.lineCap),
      ("stroke-linejoin", lineJoinToSvg stroke.lineJoin),
      ("stroke-opacity", fmtNum stroke.color.a),
      ("stroke-dasharray", dashArrayValue stroke.dash w)]⟩
  | .drawTextRun s style pos => ⟨true, #[
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
      ("transform", "scale(1,-1)")]⟩
  | .pushTransform m => ⟨true, #[("transform", matrixToSvg m)]⟩
  | .pushAnnotation tag => ⟨true, #[("data-anno-id", toString tag)]⟩
  | .pushOpacity α => ⟨true, #[("opacity", fmtNum α)]⟩
  | .pushClip pd clipId =>
    let cid := s!"clip{clipPrefix}{clipId}"
    ⟨true, #[("d", pathDataToD pd), ("id", cid)]⟩
  | .pushForeign f => ⟨true, #[("data-foreign", BackendRender.renderOpen f)]⟩
  | .popForeign f => ⟨false, #[("data-foreign", BackendRender.renderClose f)]⟩
  | .defGradient gi g =>
    -- Gradient defs are emitted inside <defs> wrappers in the body. The animation
    -- walker skips <defs> but indexes the gradient element inside, making these
    -- attributes patchable.
    match g with
    | .linear x1 y1 x2 y2 _ spread => ⟨true, #[
        ("id", gradientIdOf gi clipPrefix),
        ("x1", fmtNum x1), ("y1", fmtNum y1),
        ("x2", fmtNum x2), ("y2", fmtNum y2),
        ("gradientUnits", "userSpaceOnUse"),
        ("spreadMethod", spreadToSvg spread)]⟩
    | .radial cx cy r fx fy fr _ spread => ⟨true, #[
        ("id", gradientIdOf gi clipPrefix),
        ("cx", fmtNum cx), ("cy", fmtNum cy), ("r", fmtNum r),
        ("fx", fmtNum fx), ("fy", fmtNum fy), ("fr", fmtNum fr),
        ("gradientUnits", "userSpaceOnUse"),
        ("spreadMethod", spreadToSvg spread)]⟩
  | .popTransform | .popAnnotation | .popOpacity | .popClip => ⟨false, #[]⟩

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
    let textAttrs := info.attrs.filter (·.1 != "textContent")
    let attrStr := renderAttrs textAttrs
    let lines := s.splitOn "\n"
    if lines.length <= 1 then
      s!"<text{eAttr}{attrStr}>{escapeXml s}</text>"
    else
      let lineHeight := style.fontSize * 1.2
      let totalH := lineHeight * (lines.length - 1).toFloat
      let startY := -totalH / 2
      let (_, tspans) := lines.foldl (fun (i, acc) line =>
        let dy := if i == 0 then startY else lineHeight
        let span := s!"<tspan x=\"{fmtNum pos.x}\" dy=\"{fmtNum dy}\">{escapeXml line}</tspan>"
        (i + 1, acc ++ span)) (0, "")
      s!"<text{eAttr}{attrStr}>{tspans}</text>"
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
  | .defGradient _ g =>
    let stopsStr := match g with
      | .linear _ _ _ _ stops _ | .radial _ _ _ _ _ _ stops _ => stopsToSvg stops
    match g with
    | .linear .. =>
      s!"<defs><linearGradient{eAttr}{renderAttrs info.attrs}>{stopsStr}</linearGradient></defs>"
    | .radial .. =>
      s!"<defs><radialGradient{eAttr}{renderAttrs info.attrs}>{stopsStr}</radialGradient></defs>"
  | .popTransform | .popAnnotation | .popOpacity | .popClip => "</g>"

/--
Renders an array of draw commands to a complete SVG document string.
The {name}`clipPrefix` distinguishes clip-path IDs when multiple SVGs share a page.

When {name}`emitElemIdx` is true, each element that {name (full := CmdAttrInfo.producesElement)}`producesElement`
gets a {lit}`data-e` attribute with its element index. The animation player uses
these to locate patchable elements by explicit ID rather than fragile DOM-walk order.
-/
def render {β : Type} [BackendRender β] (cmds : Array (DrawCmd β)) (viewBox : ViewBox)
    (clipPrefix : String := "") (emitElemIdx : Bool := false) : String :=
  let header := s!"<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"{fmtNum viewBox.minX} {fmtNum viewBox.minY} {fmtNum viewBox.width} {fmtNum viewBox.height}\">"
  let (body, _) := cmds.foldl (init := ("", 0)) fun (acc, ei) cmd =>
    let produces := (drawCmdAttrs cmd clipPrefix).producesElement
    let tag := if emitElemIdx && produces then some ei else none
    let fragment := renderCmd cmd clipPrefix tag
    (acc ++ "\n" ++ fragment, if produces then ei + 1 else ei)
  -- Flip y-axis: SVG y points down, diagram y points up
  header ++ "\n<g transform=\"scale(1,-1)\">" ++ body ++ "\n</g>\n</svg>"
