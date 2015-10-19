package org.opensolid.core

final case class CompoundTransformation3d(first: Transformation3d, second: Transformation3d)
  extends Transformation3d {

  def apply(length: Double): Double = second(first(length))

  def apply(handedness: Handedness): Handedness = second(first(handedness))

  def apply(point: Point3d): Point3d = second(first(point))
  
  def apply(vector: Vector3d): Vector3d = second(first(vector))

  def apply(direction: Direction3d): Direction3d = second(first(direction))
}
