package org.opensolid.core

trait Transformable3d[T] {
  def transformedBy(transformation: Transformation3d): T

  def translatedBy(vector: Vector3d): T = transformedBy(Translation3d(vector))
}
