package org.opensolid

final case class Handedness(value: Int) extends AnyVal {
  def sign = Sign(value)
  def unary_- = Handedness(-value)
  def *(that: Handedness) = Handedness(value * that.value)
  def transformedBy(transformation: Transformation) = transformation.transform(this)
}

object Handedness {
  def fromSign(sign: Sign) = Handedness(sign.value)
  def fromSignOf(value: Double) = Handedness(value.signum)

  val Left = Handedness(-1)
  val None = Handedness(0)
  val Right = Handedness(1)
}
