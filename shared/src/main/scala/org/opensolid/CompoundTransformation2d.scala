package org.opensolid

final case class CompoundTransformation2d(first: Transformation2d, second: Transformation2d)
  extends Transformation2d {

  def apply(length: Double): Double = second(first(length))

  def apply(handedness: Handedness): Handedness = second(first(handedness))

  def apply(point: Point2d): Point2d = second(first(point))
  
  def apply(vector: Vector2d): Vector2d = second(first(vector))

  def apply(direction: Direction2d): Direction2d = second(first(direction))
}
