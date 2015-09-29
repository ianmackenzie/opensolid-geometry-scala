package org.opensolid

final case class Interval(lowerBound: Double, upperBound: Double) extends Bounded1d {
  override def bounds: Interval = this

  def width: Double = upperBound - lowerBound
  def median: Double = lowerBound + 0.5 * width

  def *(value: Double): Interval = {
    if (value >= 0.0) {
      Interval(value * lowerBound, value * upperBound)
    } else {
      Interval(value * upperBound, value * lowerBound)
    }
  }
}

object Interval {
  def apply(value: Double): Interval = Interval(value, value)

  val Empty: Interval = Interval(Double.PositiveInfinity, Double.NegativeInfinity)
  val Whole: Interval = Interval(Double.NegativeInfinity, Double.PositiveInfinity)
  val Unit: Interval = Interval(0.0, 1.0)
}
