package org.opensolid.core

final case class Point2d(x: Double, y: Double)
  extends Bounded2d with Transformable2d[Point2d] with Scalable2d[Point2d] {

  override def bounds: Box2d = Box2d(Interval(x), Interval(y))

  override def transformedBy(transformation: Transformation2d): Point2d = {
    transformation(this)
  }

  override def scaledAbout(point: Point2d, scale: Double): Point2d = point + scale * (this - point)

  def +(vector: Vector2d): Point2d = Point2d(x + vector.x, y + vector.y)

  def -(that: Point2d): Vector2d = Vector2d(x - that.x, y - that.y)
}

object Point2d {
  val Origin: Point2d = Point2d(0.0, 0.0)
}

final case class Point3d(x: Double, y: Double, z: Double)
  extends Bounded3d with Transformable3d[Point3d] with Scalable3d[Point3d] {

  override def bounds: Box3d = Box3d(Interval(x), Interval(y), Interval(z))

  override def transformedBy(transformation: Transformation3d): Point3d = {
    transformation(this)
  }

  override def scaledAbout(point: Point3d, scale: Double): Point3d = point + scale * (this - point)

  def +(vector: Vector3d): Point3d = Point3d(x + vector.x, y + vector.y, z + vector.z)

  def -(that: Point3d): Vector3d = Vector3d(x - that.x, y - that.y, z - that.z)
}

object Point3d {
  val Origin: Point3d = Point3d(0.0, 0.0, 0.0)
}
