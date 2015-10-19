package org.opensolid

trait VectorTransformable2d[T] {
  def transformedBy(transformation: Transformation2d): T
}
