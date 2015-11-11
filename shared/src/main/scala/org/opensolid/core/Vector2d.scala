package org.opensolid.core

import scala.math
import scala.util.Random

final case class Vector2d(x: Double, y: Double) extends VectorTransformable2d[Vector2d] {
  def components: Array[Double] = Array(x, y)

  def component(index: Int): Double = index match {
    case 0 => x
    case 1 => y
    case _ => throw new IndexOutOfBoundsException(s"Index $index is out of bounds for Vector2d")
  }

  def squaredLength: Double = x * x + y * y

  def length: Double = math.sqrt(squaredLength)

  def isZero(precision: Double): Boolean = squaredLength.isZero(precision * precision)

  def isNotZero(precision: Double): Boolean = squaredLength.isNotZero(precision * precision)

  override def transformedBy(transformation: Transformation2d): Vector2d = transformation(this)

  def normalized: Vector2d = direction.vector

  def direction: Direction2d = {
    if (this == Vector2d.Zero) {
      Direction2d.None
    } else {
      val length = this.length
      Direction2d(x / length, y / length)
    }
  }

  def orthogonalDirection: Direction2d = direction.orthogonalDirection

  def unary_- : Vector2d = Vector2d(-x, -y)

  def +(that: Vector2d): Vector2d = Vector2d(this.x + that.x, this.y + that.y)

  def -(that: Vector2d): Vector2d = Vector2d(this.x - that.x, this.y - that.y)

  def *(sign: Sign): Vector2d = Vector2d(x * sign, y * sign)

  def *(value: Double): Vector2d = Vector2d(x * value, y * value)

  def /(value: Double): Vector2d = Vector2d(x / value, y / value)

  def dot(that: Vector2d): Double = this.x * that.x + this.y * that.y

  def dot(direction: Direction2d): Double = x * direction.x + y * direction.y
}

object Vector2d {
  def fromComponents[T <% Double](components: Seq[T]): Vector2d = components match {
    case Seq(x, y) => Vector2d(x, y)
    case _ => throw new IllegalArgumentException("Vector2d requires 2 components")
  }

  def polar(radius: Double, angle: Double): Vector2d =
    Vector2d(radius * math.cos(angle), radius * math.sin(angle))

  def random: Vector2d = random(Random)

  def random(generator: Random): Vector2d = Vector2d(generator.nextDouble(), generator.nextDouble())

  val Zero: Vector2d = Vector2d(0.0, 0.0)
}
