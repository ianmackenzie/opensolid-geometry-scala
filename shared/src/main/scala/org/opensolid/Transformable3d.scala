package org.opensolid

trait Transformable3d[T] {
  def transformedBy(transformation: Transformation3d): T
}
