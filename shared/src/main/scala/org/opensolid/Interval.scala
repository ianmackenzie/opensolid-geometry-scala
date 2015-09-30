package org.opensolid

final case class Interval(val lowerBound: Double, val upperBound: Double) extends Bounded1d {
  def this(value: Double) = this(value, value)

  override def bounds: Interval = this

  def isEmpty: Boolean = upperBound < lowerBound;
  def width: Double = upperBound - lowerBound
  def median: Double = lowerBound + 0.5 * width

  def unary_- : Interval = Interval(-upperBound, -lowerBound)
  
  def negated: Interval = -this

  def *(value: Double): Interval = {
    if (value >= 0.0) {
      Interval(value * lowerBound, value * upperBound)
    } else {
      Interval(value * upperBound, value * lowerBound)
    }
  }

  def multipliedBy(value: Double): Interval = this * value
}

object Interval {
  def apply(value: Double): Interval = new Interval(value)

  val Empty: Interval = new Interval(Double.PositiveInfinity, Double.NegativeInfinity)
  val Whole: Interval = new Interval(Double.NegativeInfinity, Double.PositiveInfinity)
  val Unit: Interval = new Interval(0.0, 1.0)
}
