
namespace Illuminate

/-- Measurement result for a primitive whose size depends on fonts or the backend. -/
structure MeasuredBox where
  /-- Total horizontal extent. -/
  width : Float
  /-- Distance from baseline to top of the bounding box. -/
  ascent : Float
  /-- Distance from baseline to bottom of the bounding box. -/
  descent : Float
  /-- Offset from top to the text baseline. -/
  baseline : Float
  /-- Offset from top to the math axis; equals baseline for plain text. -/
  mathAxis : Float
deriving Repr, BEq, Inhabited

