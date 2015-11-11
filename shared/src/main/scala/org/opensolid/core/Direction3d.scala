package org.opensolid.core

import scala.annotation.tailrec
import scala.math
import scala.util.Random

final case class Direction3d(x: Double, y: Double, z: Double)
  extends VectorTransformable3d[Direction3d] {
  
  def components: Array[Double] = Array(x, y, z)

  def component(index: Int): Double = index match {
    case 0 => x
    case 1 => y
    case _ => throw new IndexOutOfBoundsException(s"Index $index is out of bounds for Direction3d")
  }

  def vector: Vector3d = Vector3d(x, y, z)

  def unary_- : Direction3d = Direction3d(-x, -y, -z)

  def *(sign: Sign): Direction3d = Direction3d(x * sign, y * sign, z * sign)

  def *(value: Double): Vector3d = Vector3d(x * value, y * value, z * value)

  def /(value: Double): Vector3d = Vector3d(x / value, y / value, z / value)

  def transformedBy(transformation: Transformation3d): Direction3d = transformation(this)

  def dot(vector: Vector3d): Double = x * vector.x + y * vector.y + z * vector.z

  def dot(that: Direction3d): Double = this.x * that.x + this.y * that.y + this.z * that.z

  def cross(vector: Vector3d): Vector3d = this.vector.cross(vector)

  def cross(that: Direction3d): Vector3d = this.vector.cross(that.vector)

  def orthogonalDirection: Direction3d = vector.orthogonalDirection
}

object Direction3d {
  def fromComponents[T <% Double](components: Seq[T]): Direction3d = components match {
    case Seq(x, y, z) => Direction3d(x, y, z)
    case _ => throw new IllegalArgumentException("Direction3d requires 3 components")
  }

  def spherical(azimuth: Double, elevation: Double): Direction3d = {
    val cosElevation = math.cos(elevation)
    val sinElevation = math.sin(elevation)
    val cosAzimuth = math.cos(azimuth)
    val sinAzimuth = math.sin(azimuth)
    Direction3d(cosElevation * cosAzimuth, cosElevation * sinAzimuth, sinElevation)
  }

  def random: Direction3d = random(Random)

  def random(generator: Random): Direction3d = {
    @tailrec
    def generate: Direction3d = {
      val x = -1.0 + 2.0 * generator.nextDouble()
      val y = -1.0 + 2.0 * generator.nextDouble()
      val z = -1.0 + 2.0 * generator.nextDouble()
      val squaredNorm = x * x + y * y + z * z
      if (squaredNorm >= 0.25 && squaredNorm <= 1.0) {
        val norm = math.sqrt(squaredNorm)
        Direction3d(x / norm, y / norm, z / norm)
      } else {
        generate
      }
    }
    generate
  }

  val None: Direction3d = Direction3d(0.0, 0.0, 0.0)

  val X: Direction3d = Direction3d(1.0, 0.0, 0.0)

  val Y: Direction3d = Direction3d(0.0, 1.0, 0.0)

  val Z: Direction3d = Direction3d(0.0, 0.0, 1.0)
}
