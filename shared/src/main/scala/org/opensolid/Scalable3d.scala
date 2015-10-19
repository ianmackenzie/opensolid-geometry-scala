package org.opensolid

trait Scalable3d[T] {
  def scaledAbout(point: Point3d, scale: Double): T
}
