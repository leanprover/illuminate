/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/

import Illuminate

set_option linter.missingDocs false

open Illuminate

/-!
# Animation Examples

Hover over {lit}`#animate` to see playback in the InfoView, with play/pause button and scrub bar.
-/

-- Growing circle: holds, grows from 10 to 50 while changing color, holds
#animate (fps := 120)
  [{ duration := 0.5 }, { duration := 1.0 }, { duration := 0.5 }]
  (fun progress =>
    let t := Easing.easeInOut progress[1]
    let radius := Interpolate.interpolate 10.0 50.0 t
    let color := Interpolate.interpolate Color.blue Color.red t
    Diagram.atop (Diagram.rect 100 100) (Diagram.circle radius (fill := .solid { color })))

-- Fade in title, then slide in subtitle
#animate (fps := 30)
  [{ duration := 1.0 }, { duration := 1.0 }]
  (fun progress =>
    let title := fadeIn (Diagram.text "Hello" { fontSize := 24, bold := true }) progress[0]
    let sub := slide
      (fadeIn (Diagram.text "World" { fontSize := 16 }) progress[1])
      ⟨0, -30⟩ ⟨0, 0⟩ progress[1]
    Diagram.vcat [title, sub])

-- Array highlight: a cursor moves across 5 cells
#animate (fps := 30)
  [{ duration := 0.5 }, { duration := 0.5 }, { duration := 0.5 },
   { duration := 0.5 }, { duration := 0.5 }]
  (fun progress =>
    let cells : List (Diagram SVG) := List.range 5 |>.map fun i =>
      let t := progress[i]?.getD 0
      let color := Interpolate.interpolate Color.lightGray Color.red t
      Diagram.rect 30 30 (fill := .solid { color })
        (stroke := { color := Color.black, width := 1 })
    Diagram.hsep 4 cells)

-- Rotating square with easing
#animate (fps := 30)
  [{ duration := 2.0 }]
  (fun progress =>
    let t := Easing.easeInOut progress[0]
    animRotate
      (Diagram.rect 40 40
        (fill := .solid { color := { r := 100, g := 149, b := 237 } })
        (stroke := { color := Color.black, width := 1.5 }))
      0 (2 * pi) t)

-- Slide show mock with pause steps (click to advance)
#animate (fps := 30)
  [{ duration := 0, pause := true },
   { duration := 0.3 },
   { duration := 0, pause := true },
   { duration := 0.3 },
   { duration := 0, pause := true },
   { duration := 0.3 },
   { duration := 0, pause := true }]
  (fun progress =>
    let title := Diagram.text "My Talk" { fontSize := 20, bold := true }
    let bullet1 := fadeIn (Diagram.text "• First point" { fontSize := 14 }) progress[1]
    let bullet2 := fadeIn (Diagram.text "• Second point" { fontSize := 14 }) progress[3]
    let bullet3 := fadeIn (Diagram.text "• Third point" { fontSize := 14 }) progress[5]
    Diagram.vcat [title, bullet1, bullet2, bullet3] (align := .left))

-- Parallel effects: color and position animate together in one step
#animate
  [{ duration := 2.0 }]
  (fun progress =>
    let t := Easing.easeOut progress[0]
    let color := Interpolate.interpolate Color.blue Color.red t
    let pos := Interpolate.interpolate (0 : Float) 80 t
    Diagram.translate pos 0
      (Diagram.circle 15 (fill := .solid { color })))

-- Scale bounce: circle grows with overshoot easing
#animate
  [{ duration := 1.5 }]
  (fun progress =>
    let t := Easing.backOut progress[0]
    animScale
      (Diagram.circle 20 (fill := .solid { color := Color.green })
        (stroke := { color := Color.black, width := 1 }))
      0.1 1.0 t)

-- Bouncing ball with squash deformation, then bounces away to the right
#animate
  [{ duration := 1.0, loop := true, pause := true }, { duration := 1.5 }]
  (fun progress =>
    let t0 := progress[0]
    let t1 := Easing.easeIn progress[1]
    -- Step 0: bouncing in place. Step 1: bounce rightward and fade out.
    let arc := 4 * t0 * (1 - t0)
    let bounceY := arc * 80
    -- In step 1, add a rightward arc that also goes up
    let exitArc := 4 * t1 * (1 - t1)
    let exitX := t1 * 150
    let exitY := exitArc * 120
    let y := bounceY + exitY
    let x := exitX
    -- Squash only during the bounce (step 0)
    let nearBottom := (1 - arc) * (1 - t1)
    let squash := nearBottom * nearBottom
    let sx := 1 + squash * 0.3
    let sy := 1 - squash * 0.25
    -- Fade out during step 1
    let opacity := 1 - t1
    let ball := Diagram.ellipse (15 * sx) (15 * sy)
      (fill := .solid { color := { r := 230, g := 80, b := 50 } })
      (stroke := { color := Color.black, width := 1.2 })
    let shadow := Diagram.ellipse (12 * sx) 3
      (fill := .solid { color := { r := 0, g := 0, b := 0, a := 0.15 * (1 - arc * 0.7) * opacity } })

    Diagram.compose
       (Diagram.translate x 1.5 shadow)
      (fadeIn (Diagram.translate x (15 * sy + y) ball) opacity))

-- Cross-fade between two shapes
#animate
  [{ duration := 0.5 }, { duration := 1.5 }, { duration := 0.5 }]
  (fun progress =>
    let t := Easing.easeInOut progress[1]
    let sq := Diagram.rect 40 40
      (fill := .solid { color := Color.blue })
      (stroke := { color := Color.black, width := 1 })
    let circ := Diagram.circle 25
      (fill := .solid { color := Color.red })
      (stroke := { color := Color.black, width := 1 })
    crossFade sq circ t)

-- Gradient spotlight: radial gradient focal point orbits inside a drifting ellipse
#animate
  [{ duration := 3.0, loop := true }]
  (fun progress =>
    let t := progress[0]
    let θ := t * 2 * pi
    -- Focal point orbits in a small circle inside the ellipse
    let fx := 20 * Float.cos θ
    let fy := 12 * Float.sin θ
    let stops : Array GradientStop := #[
      { offset := 0, color := Color.white },
      { offset := 0.4, color := { r := 255, g := 220, b := 100 } },
      { offset := 1, color := { r := 30, g := 60, b := 120 } }
    ]
    let grad := Gradient.radial 0 0 50 fx fy 0 stops
    let ellipseShape := Diagram.ellipse 50 35
      (fill := .gradient grad)
      (stroke := { color := { r := 30, g := 60, b := 120 }, width := (2 : Float) })
    -- Drift the whole ellipse in a gentle figure-eight
    let dx := 30 * Float.sin θ
    let dy := 15 * Float.sin (2 * θ)
    Diagram.transform (Matrix.translate dx dy) ellipseShape)
