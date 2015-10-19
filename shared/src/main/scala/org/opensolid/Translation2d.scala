package org.opensolid

final case class Translation2d(vector: Vector2d) extends Transformation2d {
  override def apply(length: Double): Double = length

  override def apply(handedness: Handedness): Handedness = handedness

  override def apply(point: Point2d): Point2d = point + vector
  
  override def apply(vector: Vector2d): Vector2d = vector

  override def apply(direction: Direction2d): Direction2d = direction
}
