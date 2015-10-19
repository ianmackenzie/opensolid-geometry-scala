package org.opensolid

abstract class Transformation3d {
  def transform(length: Double): Double

  def transform(handedness: Handedness): Handedness

  def transform(point: Point3d): Point3d

  def transform(vector: Vector3d): Vector3d

  def transform(direction: Direction3d): Direction3d
}
