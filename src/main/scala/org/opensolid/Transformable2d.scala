package org.opensolid

trait Transformable2d[T] {
  def transformedBy(transformation: Transformation2d): T
}
