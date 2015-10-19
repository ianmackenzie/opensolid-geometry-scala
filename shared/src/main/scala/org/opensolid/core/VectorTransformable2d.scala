package org.opensolid.core

trait VectorTransformable2d[T] {
  def transformedBy(transformation: Transformation2d): T
}
