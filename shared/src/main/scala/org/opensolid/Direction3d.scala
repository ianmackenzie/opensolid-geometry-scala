package org.opensolid

import scala.annotation.tailrec
import scala.math
import scala.util.Random

final case class Direction3d(x: Double, y: Double, z: Double)
  extends VectorTransformable3d[Direction3d] {
  
  def components: (Double, Double, Double) = (x, y, z)

  def vector: Vector3d = Vector3d(x, y, z)

  def unary_- : Direction3d = Direction3d(-x, -y, -z)

  def *(value: Double): Vector3d = Vector3d(x * value, y * value, z * value)

  def /(value: Double): Vector3d = Vector3d(x / value, y / value, z / value)

  def transformedBy(transformation: Transformation3d): Direction3d = {
    transformation(this)
  }
}

object Direction3d {
  def apply(components: (Double, Double, Double)): Direction3d =
    Direction3d(components._1, components._2, components._3)

  def spherical(azimuth: Double, elevation: Double): Direction3d = {
    val cosElevation = math.cos(elevation)
    val sinElevation = math.sin(elevation)
    val cosAzimuth = math.cos(azimuth)
    val sinAzimuth = math.sin(azimuth)
    Direction3d(cosElevation * cosAzimuth, cosElevation * sinAzimuth, sinElevation)
  }

  def random(generator: Random = Random): Direction3d = {
    @tailrec def generate: Direction3d = {
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
