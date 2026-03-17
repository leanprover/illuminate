/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/

import Illuminate.Geometry
import Illuminate.Style
import Illuminate.Diagram


namespace Illuminate

/-- Configuration for a state diagram (DFA/NFA). -/
structure StateDiagramConfig where
  /-- Radius of each state circle. -/
  radius : Float := 20
  /-- Horizontal spacing between state centers. -/
  spacing : Float := 90
  /-- Fill color for normal states. -/
  stateColor : Color := { r := 230, g := 240, b := 255 }
  /-- Fill color for accepting states. -/
  acceptColor : Color := { r := 200, g := 255, b := 200 }
  /-- Stroke width for circles and arrows. -/
  lineWidth : Float := 1.5
  /-- Font size for state labels. -/
  fontSize : Float := 14
  /-- Font size for transition labels. -/
  labelSize : Float := 12
deriving Repr, BEq

namespace StateDiagramConfig

variable {β : Type}

private def stateStroke (cfg : StateDiagramConfig) : Stroke :=
  { color := Color.black, width := (cfg.lineWidth : Float) }

private def arrowStroke (cfg : StateDiagramConfig) : FullStroke :=
  { color := Color.black, width := cfg.lineWidth, lineCap := .round, lineJoin := .miter, dash := .solid }

private def labelStyle (cfg : StateDiagramConfig) : TextStyle :=
  { fontSize := (cfg.fontSize : Float) }

private def transStyle (cfg : StateDiagramConfig) : TextStyle :=
  { fontSize := (cfg.labelSize : Float), bold := true }

private def posOf (cfg : StateDiagramConfig) (i : Nat) : Vec2 :=
  ⟨i.toFloat * cfg.spacing, 0⟩

/-- Builds a default arrowhead configuration for this state diagram. -/
private def defaultArrowhead (_ : StateDiagramConfig) : Arrowhead := {}

/-- Draws a normal (non-accepting) state at position `i` with the given label. -/
def state (cfg : StateDiagramConfig) (i : Nat) (label : String) : Diagram β :=
  Diagram.transform (Matrix.translate (cfg.posOf i).x (cfg.posOf i).y)
    (Diagram.compose
      (Diagram.circle cfg.radius (fill := { color := cfg.stateColor }) (stroke := cfg.stateStroke))
      (.text label cfg.labelStyle))

/-- Draws an accepting state (double circle) at position `i` with the given label. -/
def accept (cfg : StateDiagramConfig) (i : Nat) (label : String) : Diagram β :=
  let transparentAccept : Color := { cfg.acceptColor with a := 0 }
  Diagram.transform (Matrix.translate (cfg.posOf i).x (cfg.posOf i).y)
    (Diagram.compose
      (Diagram.compose
        (Diagram.circle cfg.radius (fill := { color := cfg.acceptColor }) (stroke := cfg.stateStroke))
        (Diagram.circle (cfg.radius - 4)
          (fill := { color := transparentAccept })
          (stroke := cfg.stateStroke)))
      (.text label cfg.labelStyle))

/-- Draws a start arrow pointing into state `i`. -/
def start (cfg : StateDiagramConfig) (i : Nat) : Diagram β :=
  let target := cfg.posOf i
  let a : Vec2 := ⟨target.x - 40, target.y⟩
  let b : Vec2 := ⟨target.x - (cfg.radius + 2), target.y⟩
  let srcEnd : LineEnd := { point := .anonymous }
  let tgtEnd : LineEnd := { point := .anonymous, arrowhead := some cfg.defaultArrowhead }
  ArrowDraw.drawLine a b srcEnd tgtEnd cfg.arrowStroke

/-- Draws a straight labeled edge from state `i` to state `j`. -/
def edge (cfg : StateDiagramConfig) (i j : Nat) (label : String) : Diagram β :=
  let a := Vec2.add (cfg.posOf i) ⟨cfg.radius + 2, 0⟩
  let b := Vec2.sub (cfg.posOf j) ⟨cfg.radius + 2, 0⟩
  let srcEnd : LineEnd := { point := .anonymous }
  let tgtEnd : LineEnd := { point := .anonymous, arrowhead := some cfg.defaultArrowhead }
  let arrow := ArrowDraw.drawLine a b srcEnd tgtEnd cfg.arrowStroke
  let mid : Vec2 := ⟨(a.x + b.x) / 2, (a.y + b.y) / 2⟩
  let lbl := Diagram.transform (Matrix.translate mid.x (mid.y + 8))
    (.text label cfg.transStyle)
  Diagram.compose arrow lbl

/-- Draws a curved labeled arc from state `i` to state `j`, bending up or down. -/
def arc (cfg : StateDiagramConfig) (i j : Nat) (label : String) (bendY : Float) : Diagram β :=
  let r := cfg.radius + 2
  let srcCenter := cfg.posOf i
  let tgtCenter := cfg.posOf j
  let mid := (srcCenter.x + tgtCenter.x) / 2
  let ctrl : Vec2 := ⟨mid, bendY⟩
  -- Place endpoints on circle boundaries, pointing toward/from the control point
  let srcDir := (Vec2.sub ctrl srcCenter).normalize
  let src := Vec2.add srcCenter (r • srcDir)
  let tgtDir := (Vec2.sub ctrl tgtCenter).normalize
  let tgt := Vec2.add tgtCenter (r • tgtDir)
  -- Compute departure/arrival angles from the quadratic control point
  let srcAngle := Float.atan2 (ctrl.y - src.y) (ctrl.x - src.x)
  let tgtAngle := Float.atan2 (tgt.y - ctrl.y) (tgt.x - ctrl.x)
  let srcEnd : LineEnd := { point := .anonymous, angle := some srcAngle, pull := 0.33 }
  let tgtEnd : LineEnd :=
    { point := .anonymous, angle := some tgtAngle, pull := 0.33,
      arrowhead := some cfg.defaultArrowhead }
  let arrow := ArrowDraw.drawLine src tgt srcEnd tgtEnd cfg.arrowStroke
  -- Place label at the actual cubic Bézier midpoint (t=0.5)
  let (c1, c2) := ArrowDraw.computeControlPoints src tgt srcAngle tgtAngle 0.33 0.33
  -- Cubic Bézier at t=0.5: Bernstein basis values [(1-t)³, 3(1-t)²t, 3(1-t)t², t³]
  let curveMid : Vec2 :=
    ⟨0.125 * src.x + 0.375 * c1.x + 0.375 * c2.x + 0.125 * tgt.x,
     0.125 * src.y + 0.375 * c1.y + 0.375 * c2.y + 0.125 * tgt.y⟩
  let labelOff := if bendY > 0 then 8.0 else -8.0
  let lbl := Diagram.transform (Matrix.translate curveMid.x (curveMid.y + labelOff))
    (.text label cfg.transStyle)
  Diagram.compose arrow lbl

/-- Draws a self-loop on state `i` with the given label. -/
def loop (cfg : StateDiagramConfig) (i : Nat) (label : String) : Diagram β :=
  let cx := (cfg.posOf i).x
  let loopR := 14.0
  let top := cfg.radius + 2
  let a : Vec2 := ⟨cx - loopR * 0.7, top⟩
  let b : Vec2 := ⟨cx + loopR * 0.7, top⟩
  let c1 : Vec2 := ⟨cx - loopR, top + loopR * 2.2⟩
  let c2 : Vec2 := ⟨cx + loopR, top + loopR * 2.2⟩
  -- Build shaft manually (self-loop geometry doesn't map to angle+pull)
  let arrowStrokeOverride : Stroke := { color := cfg.arrowStroke.color, width := cfg.arrowStroke.width, lineCap := cfg.arrowStroke.lineCap, lineJoin := cfg.arrowStroke.lineJoin, dash := cfg.arrowStroke.dash }
  let shaft := Diagram.fromStroke
    (PathData.empty |>.moveTo a |>.curveTo c1 c2 b) arrowStrokeOverride
  let (head, _) := ArrowDraw.drawArrowhead cfg.defaultArrowhead b (b - c2) cfg.arrowStroke
  let lbl := Diagram.transform (Matrix.translate cx (top + loopR * 2.2 + 6))
    (.text label cfg.transStyle)
  Diagram.compose (Diagram.compose shaft head) lbl

end StateDiagramConfig

/-- Overlays a list of diagrams into a single diagram. -/
def overlay {β : Type} (ds : List (Diagram β)) : Diagram β :=
  ds.foldl Diagram.compose Diagram.empty
