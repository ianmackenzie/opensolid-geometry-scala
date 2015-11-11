package org.opensolid.core

final case class Sign(value: Int) extends AnyVal {
  def unary_- : Sign = Sign(-value)
  
  def *(that: Sign): Sign = Sign(this.value * that.value)

  def *(value: Double): Double = this.value * value

  def *(handedness: Handedness): Handedness = handedness * this

  def *(vector: Vector2d): Vector2d = vector * this

  def *(vector: Vector3d): Vector3d = vector * this

  def *(direction: Direction2d): Direction2d = direction * this
  
  def *(direction: Direction3d): Direction3d = direction * this
}

object Sign {
  def of(value: Double): Sign = Sign(value.signum)

  val Negative: Sign = Sign(-1)

  val None: Sign = Sign(0)

  val Positive: Sign = Sign(1)
}
