package org.opensolid.core

final case class Interval(val lowerBound: Double, val upperBound: Double) extends Bounded1d {
  def this(value: Double) = this(value, value)

  override def equals(other: Any): Boolean = other match {
    case that: Interval =>
      (this.lowerBound == that.lowerBound && this.upperBound == that.upperBound) ||
      (this.isEmpty && that.isEmpty)
    case _ => false
  }

  override def bounds: Interval = this

  def isEmpty: Boolean = lowerBound.isNaN && upperBound.isNaN;

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
  def apply(value: Double): Interval = new Interval(value)

  val Empty: Interval = new Interval(Double.NaN, Double.NaN)

  val Whole: Interval = new Interval(Double.NegativeInfinity, Double.PositiveInfinity)

  val Unit: Interval = new Interval(0.0, 1.0)
}
