package org.opensolid

final case class Sign(value: Int) {
  def unary_- : Sign = Sign(-value)
  
  def *(that: Sign): Sign = Sign(value * that.value)

  def *(handedness: Handedness): Handedness = Handedness(value * handedness.value)

  def *(unitVector: UnitVector2d): UnitVector2d = {
    UnitVector2d(value * unitVector.x, value * unitVector.y)
  }
  
  def *(unitVector: UnitVector3d): UnitVector3d = {
    UnitVector3d(value * unitVector.x, value * unitVector.y, value * unitVector.z)
  }
}

object Sign {
  def of(value: Double): Sign = Sign(value.signum)

  val Negative: Sign = Sign(-1)
  val None: Sign = Sign(0)
  val Positive: Sign = Sign(1)
}
