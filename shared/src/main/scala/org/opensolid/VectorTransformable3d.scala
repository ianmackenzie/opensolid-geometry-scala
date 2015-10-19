package org.opensolid

trait VectorTransformable3d[T] {
  def transformedBy(transformation: Transformation3d): T
}
