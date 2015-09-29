package org.opensolid

final class Interval private (val lowerBound: Double, val upperBound: Double) extends Bounded1d {
  override def toString: String = s"Interval($upperBound, $lowerBound)"

  override def equals(other: Any): Boolean = other match {
    case that: Interval => lowerBound == that.lowerBound && upperBound == that.upperBound
    case _ => false
  }

  override def bounds: Interval = this

  def isEmpty: Boolean = upperBound < lowerBound;
  def width: Double = upperBound - lowerBound
  def median: Double = lowerBound + 0.5 * width

  def unary_- : Interval = Interval(-upperBound, -lowerBound)

  def *(value: Double): Interval = {
    if (value >= 0.0) {
      Interval(value * lowerBound, value * upperBound)
    } else {
      Interval(value * upperBound, value * lowerBound)
    }
  }
}

object Interval {
  def apply(lowerBound: Double, upperBound: Double): Interval = {
    if (lowerBound <= upperBound) {
      new Interval(lowerBound, upperBound)
    } else {
      Interval.Empty
    }
  }

  def apply(value: Double): Interval = new Interval(value, value)

  def unapply(argument: Any): Option[(Double, Double)] = argument match {
    case interval: Interval => Some((interval.lowerBound, interval.upperBound))
    case _ => None
  }

  val Empty: Interval = new Interval(Double.PositiveInfinity, Double.NegativeInfinity)
  val Whole: Interval = new Interval(Double.NegativeInfinity, Double.PositiveInfinity)
  val Unit: Interval = new Interval(0.0, 1.0)
}
