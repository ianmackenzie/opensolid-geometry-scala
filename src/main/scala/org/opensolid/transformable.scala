package org.opensolid

trait Transformable2d[T] {
  def transformedBy(transformation: Transformation2d): T
}

trait Scalable2d[T] extends Transformable2d[T] {
  def scaledAbout(point: Point2d, scale: Double): T
}

trait Transformable3d[T] {
  def transformedBy(transformation: Transformation3d): T
}

trait Scalable3d[T] extends Transformable3d[T] {
  def scaledAbout(point: Point2d, scale: Double): T
}

trait TransformableAsVector2d[T] {
  def transformedBy(transformation: Transformation2d): T
}

trait TransformableAsVector3d[T] {
  def transformedBy(transformation: Transformation3d): T
}
