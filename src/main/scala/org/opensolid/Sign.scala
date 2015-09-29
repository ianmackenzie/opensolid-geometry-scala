package org.opensolid

final case class Sign(value: Int) extends AnyVal {
  def *(that: Sign) = Sign(value * that.value)
  def unary_- = Sign(-value)
}

object Sign {
  def of(value: Double) = Sign(value.signum)

  val Negative = Sign(-1)
  val None = Sign(0)
  val Positive = Sign(1)
}
