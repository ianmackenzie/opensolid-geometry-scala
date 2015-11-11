package org.opensolid.core

import scala.math
import scala.util.Random

final case class Vector3d(x: Double, y: Double, z: Double) extends VectorTransformable3d[Vector3d] {
  def components: Array[Double] = Array(x, y , z)

  def component(index: Int): Double = index match {
    case 0 => x
    case 1 => y
    case 2 => z
    case _ => throw new IndexOutOfBoundsException(s"Index $index is out of bounds for Vector3d")
  }

  def squaredLength: Double = x * x + y * y + z * z

  def length: Double = math.sqrt(squaredLength)

  def isZero(precision: Double): Boolean = squaredLength.isZero(precision * precision)

  def isNotZero(precision: Double): Boolean = squaredLength.isNotZero(precision * precision)

  override def transformedBy(transformation: Transformation3d): Vector3d = transformation(this)

  def normalized: Vector3d = {
    if (x != 0.0 || y != 0.0 || z != 0.0) {
      val length = this.length
      Vector3d(x / length, y / length, z / length)
    } else {
      Vector3d.Zero
    }
  }

  def direction: Direction3d = {
    if (this == Vector3d.Zero) {
      Direction3d.None
    } else {
      val length = this.length
      Direction3d(x / length, y / length, z / length)
    }
  }

  def unary_- : Vector3d = Vector3d(-x, -y, -z)

  def +(that: Vector3d): Vector3d = Vector3d(this.x + that.x, this.y + that.y, this.z + that.z)

  def -(that: Vector3d): Vector3d = Vector3d(this.x - that.x, this.y - that.y, this.z - that.z)

  def *(sign: Sign): Vector3d = Vector3d(x * sign, y * sign, z * sign)

  def *(value: Double): Vector3d = Vector3d(x * value, y * value, z * value)

  def /(value: Double): Vector3d = Vector3d(x / value, y / value, z / value)

  def dot(that: Vector3d): Double = this.x * that.x + this.y * that.y + this.z * that.z

  def dot(direction: Direction3d): Double = x * direction.x + y * direction.y + z * direction.z
}

object Vector3d {
  def fromComponents[T <% Double](components: Seq[T]): Vector3d = components match {
    case Seq(x, y, z) => Vector3d(x, y, z)
    case _ => throw new IllegalArgumentException("Vector3d requires 3 components")
  }

  def spherical(radius: Double, azimuth: Double, elevation: Double): Vector3d = {
    val cosElevation = math.cos(elevation)
    Vector3d(
      radius * cosElevation * math.cos(azimuth),
      radius * cosElevation * math.sin(azimuth),
      radius * math.sin(elevation))
  }

  def random: Vector3d = random(Random)

  def random(generator: Random): Vector3d =
    Vector3d(generator.nextDouble(), generator.nextDouble(), generator.nextDouble())

  val Zero: Vector3d = Vector3d(0.0, 0.0, 0.0)
}
