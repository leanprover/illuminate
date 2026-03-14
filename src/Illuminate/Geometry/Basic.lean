
namespace Illuminate

/-- The mathematical constant π, computed as `Float.acos (-1.0)`. -/
def pi : Float := Float.acos (-1.0)

/-- Tests whether a float is near zero (absolute value below 1e-12). -/
def nearZero (f : Float) : Bool := f.abs < 1e-12
