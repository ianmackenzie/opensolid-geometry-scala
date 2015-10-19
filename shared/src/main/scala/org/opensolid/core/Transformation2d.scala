package org.opensolid.core

abstract class Transformation2d {
  def apply(length: Double): Double

  def apply(handedness: Handedness): Handedness

  def apply(point: Point2d): Point2d
  
  def apply(vector: Vector2d): Vector2d

  def apply(direction: Direction2d): Direction2d

  def andThen(that: Transformation2d): Transformation2d = CompoundTransformation2d(this, that)
}
