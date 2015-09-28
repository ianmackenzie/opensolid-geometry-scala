package org.opensolid

import scala.scalajs.js.annotation.{JSExport, JSExportAll}

@JSExport("Interval")
@JSExportAll
final case class Interval(lowerBound: Double, upperBound: Double) extends Bounded1d {
  def width: Double = upperBound - lowerBound
  def median: Double = lowerBound + 0.5 * width
  def bounds: Interval = this
}

@JSExport("Interval_StaticMembers")
object Interval {
  @JSExport
  def singleton(value: Double): Interval = Interval(value, value)

  @JSExport("EMPTY")
  val Empty = Interval(Double.PositiveInfinity, Double.NegativeInfinity)

  @JSExport("WHOLE")
  val Whole = Interval(Double.NegativeInfinity, Double.PositiveInfinity)

  @JSExport("UNIT")
  val Unit = Interval(0.0, 1.0)
}
