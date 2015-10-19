package org.opensolid

trait Scalable2d[T] {
  def scaledAbout(point: Point2d, scale: Double): T
}
