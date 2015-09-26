package org.opensolid

import scala.scalajs.js
import js.annotation.JSExport
import js.annotation.JSExportAll

@JSExport("Point2d")
@JSExportAll
case class Point2d(x: Double, y: Double) extends Bounded2d {
  def bounds: Box2d = Box2d(Interval.singleton(x), Interval.singleton(y))
}

@JSExport("Point2d_StaticMembers")
object Point2d {
  @JSExport("ORIGIN")
  val Origin = Point2d(0.0, 0.0)
}
