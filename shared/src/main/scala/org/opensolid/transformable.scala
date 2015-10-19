package org.opensolid

trait Transformable2d[T] {
  def transformedBy(transformation: Transformation2d): T
}

trait Transformable3d[T] {
  def transformedBy(transformation: Transformation3d): T
}

trait Scalable2d[T] {
  def scaledAbout(point: Point2d, scale: Double): T
}

trait Scalable3d[T] {
  def scaledAbout(point: Point2d, scale: Double): T
}

trait VectorLike2d[T] {
  def transformedBy(transformation: Transformation2d): T
}

trait VectorLike3d[T] {
  def transformedBy(transformation: Transformation3d): T
}
