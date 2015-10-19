package org.opensolid

trait Transformable2d[T] {
  def transformedBy(transformation: Transformation2d): T

  def translatedBy(vector: Vector2d): T = transformedBy(Translation2d(vector))
}
