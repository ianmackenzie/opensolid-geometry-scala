package org.opensolid

final case class Point2d(x: Double, y: Double) extends Bounded2d with Transformable2d[Point2d] {
  override def bounds: Box2d = Box2d(Interval(x), Interval(y))

  override def transformedBy(transformation: Transformation2d): Point2d = {
    transformation.transform(this)
  }
}

object Point2d {
  val Origin: Point2d = Point2d(0.0, 0.0)
}

final case class Point3d(x: Double, y: Double, z: Double) extends Bounded3d with Transformable3d[Point3d] {
  override def bounds: Box3d = Box3d(Interval(x), Interval(y), Interval(z))

  override def transformedBy(transformation: Transformation3d): Point3d = {
    transformation.transform(this)
  }
}

object Point3d {
  val Origin: Point3d = Point3d(0.0, 0.0, 0.0)
}
