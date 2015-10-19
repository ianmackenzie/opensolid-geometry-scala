package org.opensolid.core

final case class Translation3d(vector: Vector3d) extends Transformation3d {
  override def apply(length: Double): Double = length

  override def apply(handedness: Handedness): Handedness = handedness

  override def apply(point: Point3d): Point3d = point + vector
  
  override def apply(vector: Vector3d): Vector3d = vector

  override def apply(direction: Direction3d): Direction3d = direction
}
