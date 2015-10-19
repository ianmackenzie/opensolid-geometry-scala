package org.opensolid.core

import scala.annotation.tailrec
import scala.math
import scala.util.Random

final case class Direction2d(x: Double, y: Double) extends VectorTransformable2d[Direction2d] {
  def components: (Double, Double) = (x, y)

  def vector: Vector2d = Vector2d(x, y)

  def unary_- : Direction2d = Direction2d(-x, -y)

  def *(value: Double): Vector2d = Vector2d(x * value, y * value)

  def /(value: Double): Vector2d = Vector2d(x / value, y / value)

  def transformedBy(transformation: Transformation2d): Direction2d = transformation(this)
}

object Direction2d {
  def apply(components: (Double, Double)): Direction2d = Direction2d(components._1, components._2)

  def polar(angle: Double): Direction2d = Direction2d(math.cos(angle), math.sin(angle))

  def random(generator: Random = Random): Direction2d = {
    @tailrec def generate: Direction2d = {
      val x = -1.0 + 2.0 * generator.nextDouble()
      val y = -1.0 + 2.0 * generator.nextDouble()
      val squaredNorm = x * x + y * y
      if (squaredNorm >= 0.25 && squaredNorm <= 1.0) {
        val norm = math.sqrt(squaredNorm)
        Direction2d(x / norm, y / norm)
      } else {
        generate
      }
    }
    generate
  }

  val None: Direction2d = Direction2d(0.0, 0.0)

  val X: Direction2d = Direction2d(1.0, 0.0)

  val Y: Direction2d = Direction2d(0.0, 1.0)
}
