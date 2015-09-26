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

@JSExport("Point3d")
@JSExportAll
case class Point3d(x: Double, y: Double, z: Double) extends Bounded3d {
  def bounds: Box3d = Box3d(Interval.singleton(x), Interval.singleton(y), Interval.singleton(z))
}

@JSExport("Point3d_StaticMembers")
object Point3d {
  @JSExport("ORIGIN")
  val Origin = Point3d(0.0, 0.0, 0.0)
}
