package org.opensolid.core

import scala.scalajs.js
import js.annotation.JSExport
import js.annotation.JSExportAll

@JSExport
@JSExportAll
case class Point2d(x: Double, y: Double) extends Bounded2d {
  def bounds: Box2d = Box2d(Interval.singleton(x), Interval.singleton(y))
}

@JSExport
@JSExportAll
object Point2d {
  val ORIGIN = Point2d(0.0, 0.0)
}
