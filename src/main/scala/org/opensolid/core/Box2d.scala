package org.opensolid.core

import scala.scalajs.js
import js.annotation.JSExport
import js.annotation.JSExportAll

@JSExport
@JSExportAll
case class Box2d(x: Interval, y: Interval) extends Bounded2d {
  def bounds: Box2d = this
}

@JSExport
@JSExportAll
object Box2d {
  val EMPTY = Box2d(Interval.EMPTY, Interval.EMPTY)
  val WHOLE = Box2d(Interval.WHOLE, Interval.WHOLE)
  val UNIT = Box2d(Interval.UNIT, Interval.UNIT)
}
