package org.opensolid

abstract class Transformation2d {
  def transform(length: Double): Double

  def transform(handedness: Handedness): Handedness

  def transform(point: Point2d): Point2d
  
  def transform(vector: Vector2d): Vector2d

  def transform(direction: Direction2d): Direction2d
}
