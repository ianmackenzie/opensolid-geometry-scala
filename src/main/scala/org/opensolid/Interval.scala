package org.opensolid

import scala.scalajs.js
import js.annotation.JSExport
import js.annotation.JSExportAll

@JSExport("Interval")
@JSExportAll
case class Interval(lower: Double, upper: Double) extends Bounded1d {
  def width: Double = upper - lower
  def median: Double = lower + 0.5 * width
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
