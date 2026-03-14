import Illuminate.Style.Color
import Illuminate.Style.Text
import Illuminate.Style.Stroke
import Illuminate.Style.Arrow

namespace Illuminate


/-- A three-valued configuration field: inherit from parent, reset to default, or set explicitly. -/
inductive ConfigVal (α : Type) where
  /-- Inherits the value from the parent configuration. -/
  | inherit
  /-- Resets to the global default, ignoring the parent. -/
  | reset
  /-- Sets an explicit value, overriding the parent. -/
  | set : α → ConfigVal α
deriving Repr, BEq

instance {α : Type} : Inhabited (ConfigVal α) where
  default := .inherit

/--
A cascading draw configuration where properties propagate downward through the diagram.
Each field uses `ConfigVal` to independently inherit, reset, or override.
-/
structure DrawConfig where
  /-- Stroke width in diagram units. -/
  strokeWidth : ConfigVal Float := .inherit
  /-- Stroke color. -/
  strokeColor : ConfigVal Color := .inherit
  /-- Stroke line cap style. -/
  strokeLineCap : ConfigVal LineCap := .inherit
  /-- Stroke line join style. -/
  strokeLineJoin : ConfigVal LineJoin := .inherit
  /-- Fill color. -/
  fillColor : ConfigVal Color := .inherit
  /-- Font family name. -/
  fontFamily : ConfigVal String := .inherit
  /-- Font size in diagram units. -/
  fontSize : ConfigVal Float := .inherit
  /-- Whether text is bold. -/
  fontBold : ConfigVal Bool := .inherit
  /-- Whether text is italic. -/
  fontItalic : ConfigVal Bool := .inherit
  /-- Text fill color. -/
  textColor : ConfigVal Color := .inherit
  /-- Default arrowhead style (`none` means no arrowhead). -/
  arrowhead : ConfigVal (Option Arrowhead) := .inherit
  /-- Whether arrow labels stay upright instead of rotating to follow the arrow. -/
  labelUpright : ConfigVal Bool := .inherit
deriving Repr, BEq, Inhabited

/-- A fully-resolved draw configuration with concrete values for every field. -/
structure ResolvedConfig where
  /-- Resolved stroke width. -/
  strokeWidth : Float
  /-- Resolved stroke color. -/
  strokeColor : Color
  /-- Resolved stroke line cap style. -/
  strokeLineCap : LineCap
  /-- Resolved stroke line join style. -/
  strokeLineJoin : LineJoin
  /-- Resolved fill color. -/
  fillColor : Color
  /-- Resolved font family name. -/
  fontFamily : String
  /-- Resolved font size. -/
  fontSize : Float
  /-- Resolved bold flag. -/
  fontBold : Bool
  /-- Resolved italic flag. -/
  fontItalic : Bool
  /-- Resolved text color. -/
  textColor : Color
  /-- Resolved arrowhead style (`none` means no arrowhead). -/
  arrowhead : Option Arrowhead
  /-- Whether arrow labels stay upright. -/
  labelUpright : Bool
deriving Repr, BEq, Inhabited

namespace ResolvedConfig

/-- The global default resolved configuration. -/
def defaults : ResolvedConfig where
  strokeWidth := 1.0
  strokeColor := Color.black
  strokeLineCap := .butt
  strokeLineJoin := .miter
  fillColor := Color.lightGray
  fontFamily := "sans-serif"
  fontSize := 16
  fontBold := false
  fontItalic := false
  textColor := Color.black
  arrowhead := none
  labelUpright := false

/-- Converts the resolved config to a `Stroke` value. -/
def toStroke (rc : ResolvedConfig) : Stroke where
  color := rc.strokeColor
  width := rc.strokeWidth
  lineCap := rc.strokeLineCap
  lineJoin := rc.strokeLineJoin

/-- Converts the resolved config to a `Fill` value. -/
def toFill (rc : ResolvedConfig) : Fill where
  color := rc.fillColor

/-- Converts the resolved config to a `TextStyle` value. -/
def toTextStyle (rc : ResolvedConfig) : TextStyle where
  fontFamily := rc.fontFamily
  fontSize := rc.fontSize
  bold := rc.fontBold
  italic := rc.fontItalic
  color := rc.textColor

end ResolvedConfig

namespace DrawConfig

/-- Resolves a single `ConfigVal` field given the parent value and the global default. -/
private def resolveField {α : Type} (cv : ConfigVal α) (parent dflt : α) : α :=
  match cv with
  | .inherit => parent
  | .reset => dflt
  | .set v => v

/-- Merges this draw config into a parent resolved config, producing a new resolved config. -/
def resolve (cfg : DrawConfig) (parent : ResolvedConfig) : ResolvedConfig :=
  let d := ResolvedConfig.defaults
  { strokeWidth := resolveField cfg.strokeWidth parent.strokeWidth d.strokeWidth
    strokeColor := resolveField cfg.strokeColor parent.strokeColor d.strokeColor
    strokeLineCap := resolveField cfg.strokeLineCap parent.strokeLineCap d.strokeLineCap
    strokeLineJoin := resolveField cfg.strokeLineJoin parent.strokeLineJoin d.strokeLineJoin
    fillColor := resolveField cfg.fillColor parent.fillColor d.fillColor
    fontFamily := resolveField cfg.fontFamily parent.fontFamily d.fontFamily
    fontSize := resolveField cfg.fontSize parent.fontSize d.fontSize
    fontBold := resolveField cfg.fontBold parent.fontBold d.fontBold
    fontItalic := resolveField cfg.fontItalic parent.fontItalic d.fontItalic
    textColor := resolveField cfg.textColor parent.textColor d.textColor
    arrowhead := resolveField cfg.arrowhead parent.arrowhead d.arrowhead
    labelUpright := resolveField cfg.labelUpright parent.labelUpright d.labelUpright }

/-- A draw config that resets all fields to their global defaults. -/
def resetAll : DrawConfig where
  strokeWidth := .reset
  strokeColor := .reset
  strokeLineCap := .reset
  strokeLineJoin := .reset
  fillColor := .reset
  fontFamily := .reset
  fontSize := .reset
  fontBold := .reset
  fontItalic := .reset
  textColor := .reset
  arrowhead := .reset
  labelUpright := .reset
