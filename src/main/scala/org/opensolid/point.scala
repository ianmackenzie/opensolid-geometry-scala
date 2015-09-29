package org.opensolid

final case class Point2d(x: Double, y: Double) extends Bounded2d {
  def bounds: Box2d = Box2d(Interval(x), Interval(y))
}

object Point2d {
  val Origin = Point2d(0.0, 0.0)
}

final case class Point3d(x: Double, y: Double, z: Double) extends Bounded3d {
  def bounds: Box3d = Box3d(Interval(x), Interval(y), Interval(z))
}

object Point3d {
  val Origin = Point3d(0.0, 0.0, 0.0)
}
