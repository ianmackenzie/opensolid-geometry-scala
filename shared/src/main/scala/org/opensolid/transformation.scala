package org.opensolid

trait Transformation {
  def transform(length: Double): Double

  def transform(handedness: Handedness): Handedness
}

trait Transformation2d extends Transformation {
  def transform(point: Point2d): Point2d
  
  def transform(vector: Vector2d): Vector2d

  def transform(direction: Direction2d): Direction2d

  // TODO: expression and vector expression overloads
}

trait Transformation3d extends Transformation {
  def transform(point: Point3d): Point3d

  def transform(vector: Vector3d): Vector3d

  def transform(direction: Direction3d): Direction3d

  // TODO: expression and vector expression overloads
}
