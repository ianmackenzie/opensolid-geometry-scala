package org.opensolid

final case class Interval(lowerBound: Double, upperBound: Double) extends Bounded1d {
  def width: Double = upperBound - lowerBound
  def median: Double = lowerBound + 0.5 * width
  override def bounds: Interval = this
}

object Interval {
  def apply(value: Double): Interval = Interval(value, value)

  val Empty = Interval(Double.PositiveInfinity, Double.NegativeInfinity)
  val Whole = Interval(Double.NegativeInfinity, Double.PositiveInfinity)
  val Unit = Interval(0.0, 1.0)
}
