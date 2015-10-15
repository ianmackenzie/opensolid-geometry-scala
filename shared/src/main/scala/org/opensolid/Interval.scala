package org.opensolid

import scala.scalajs.js.annotation.{JSExport, JSExportAll}

@JSExport("Interval")
@JSExportAll
final case class Interval(val lowerBound: Double, val upperBound: Double) extends Bounded1d {
  def this(value: Double) = this(value, value)

  override def bounds: Interval = this

  def isEmpty: Boolean = upperBound < lowerBound;

  def width: Double = upperBound - lowerBound

  def median: Double = lowerBound + 0.5 * width

  @JSExport("negated")
  def unary_- : Interval = Interval(-upperBound, -lowerBound)

  @JSExport("times")
  def *(value: Double): Interval = {
    if (value >= 0.0) {
      Interval(value * lowerBound, value * upperBound)
    } else {
      Interval(value * upperBound, value * lowerBound)
    }
  }
}

@JSExport("Interval$StaticMembers")
object Interval {
  @JSExport("singleton")
  def apply(value: Double): Interval = new Interval(value)

  @JSExport("EMPTY")
  val Empty: Interval = new Interval(Double.PositiveInfinity, Double.NegativeInfinity)

  @JSExport("WHOLE")
  val Whole: Interval = new Interval(Double.NegativeInfinity, Double.PositiveInfinity)

  @JSExport("UNIT")
  val Unit: Interval = new Interval(0.0, 1.0)
}
