package org.opensolid

final case class Sign(value: Int) {
  def unary_- : Sign = Sign(-value)
  
  def *(that: Sign): Sign = Sign(value * that.value)

  def *(handedness: Handedness): Handedness = Handedness(value * handedness.value)

  def *(direction: Direction2d): Direction2d = {
    Direction2d(value * direction.x, value * direction.y)
  }
  
  def *(direction: Direction3d): Direction3d = {
    Direction3d(value * direction.x, value * direction.y, value * direction.z)
  }
}

object Sign {
  def of(value: Double): Sign = Sign(value.signum)

  val Negative: Sign = Sign(-1)

  val None: Sign = Sign(0)

  val Positive: Sign = Sign(1)
}
