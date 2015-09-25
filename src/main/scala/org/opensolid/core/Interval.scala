package org.opensolid.core

import scala.scalajs.js
import js.annotation.JSExport
import js.annotation.JSExportAll

@JSExport
@JSExportAll
case class Interval(lower: Double, upper: Double) extends Bounded1d {
  def width: Double = upper - lower
  def median: Double = lower + 0.5 * width
  def bounds: Interval = this
}

@JSExport
@JSExportAll
object Interval {
  def singleton(value: Double): Interval = Interval(value, value)

  val EMPTY = Interval(Double.PositiveInfinity, Double.NegativeInfinity)
  val WHOLE = Interval(Double.NegativeInfinity, Double.PositiveInfinity)
  val UNIT = Interval(0.0, 1.0)
}
