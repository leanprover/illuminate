/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/

import Illuminate.Diagram.Basic
import Illuminate.Render.Svg


namespace Illuminate

/-- Backend-specific foreign type for SVG features. -/
inductive SVG where
  /-- Wraps content in a hyperlink. -/
  | link (href : String) : SVG
deriving Repr, BEq, Inhabited, Hashable

instance : Backend SVG where
  envelope _ e := e
  trace _ t := t
  strokeTrace _ st := st
  compile val inner := (#[DrawCmd.pushForeign val] ++ inner).push (.popForeign val)

instance : BackendRender SVG where
  renderOpen
    | .link href => s!"<a href=\"{escapeXml href}\">"
  renderClose
    | .link _ => "</a>"
