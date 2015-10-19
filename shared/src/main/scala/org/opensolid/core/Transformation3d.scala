package org.opensolid.core

abstract class Transformation3d {
  def apply(length: Double): Double

  def apply(handedness: Handedness): Handedness

  def apply(point: Point3d): Point3d

  def apply(vector: Vector3d): Vector3d

  def apply(direction: Direction3d): Direction3d

  def andThen(that: Transformation3d): Transformation3d = CompoundTransformation3d(this, that)
}
