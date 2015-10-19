package org.opensolid.core

trait VectorTransformable3d[T] {
  def transformedBy(transformation: Transformation3d): T
}
