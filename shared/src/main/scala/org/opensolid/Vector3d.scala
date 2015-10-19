package org.opensolid

import scala.math
import scala.util.Random

final case class Vector3d(x: Double, y: Double, z: Double) extends VectorLike3d[Vector3d] {
  def components: (Double, Double, Double) = (x, y, z)

  def squaredLength: Double = x * x + y * y + z * z

  def length: Double = math.sqrt(squaredLength)

  def isZero(precision: Double = DefaultPrecision): Boolean = {
    squaredLength.isZero(precision * precision)
  }

  override def transformedBy(transformation: Transformation3d): Vector3d = {
    transformation.transform(this)
  }

  def normalized: Vector3d = {
    if (x != 0.0 || y != 0.0 || z != 0.0) {
      val length = this.length
      Vector3d(x / length, y / length, z / length)
    } else {
      Vector3d.Zero
    }
  }

  def direction: Direction3d = {
    if (x != 0.0 || y != 0.0 || z != 0.0) {
      val length = this.length
      Direction3d(x / length, y / length, z / length)
    } else {
      Direction3d.None
    }
  }

  def unary_- : Vector3d = Vector3d(-x, -y, -z)

  def +(that: Vector3d): Vector3d = Vector3d(x + that.x, y + that.y, z + that.z)

  def -(that: Vector3d): Vector3d = Vector3d(x - that.x, y - that.y, z - that.z)

  def *(value: Double): Vector3d = Vector3d(x * value, y * value, z * value)

  def /(value: Double): Vector3d = Vector3d(x / value, y / value, z / value)
}

object Vector3d {
  def apply(components: (Double, Double, Double)): Vector3d =
    Vector3d(components._1, components._2, components._3)

  def spherical(radius: Double, azimuth: Double, elevation: Double): Vector3d = {
    val cosElevation = math.cos(elevation)
    Vector3d(
      radius * cosElevation * math.cos(azimuth),
      radius * cosElevation * math.sin(azimuth),
      radius * math.sin(elevation))
  }

  val Zero: Vector3d = Vector3d(0.0, 0.0, 0.0)
}
