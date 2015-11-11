package org.opensolid.core

import scala.math
import scala.util.Random

final case class Vector2d(x: Double, y: Double) extends VectorTransformable2d[Vector2d] {
  def components: Array[Double] = Array(x, y)

  def apply(index: Int): Double = index match {
    case 0 => x
    case 1 => y
    case _ => throw new IndexOutOfBoundsException(s"Index $index is out of bounds for Vector2d")
  }

  def squaredLength: Double = x * x + y * y

  def length: Double = math.sqrt(squaredLength)

  def isZero(precision: Double): Boolean = squaredLength.isZero(precision * precision)

  override def transformedBy(transformation: Transformation2d): Vector2d = transformation(this)

  def normalized: Vector2d = {
    if (x != 0.0 || y != 0.0) {
      val length = this.length
      Vector2d(x / length, y / length)
    } else {
      Vector2d.Zero
    }
  }

  def direction: Direction2d = {
    if (x != 0.0 || y != 0.0) {
      val length = this.length
      Direction2d(x / length, y / length)
    } else {
      Direction2d.None
    }
  }

  def unary_- : Vector2d = Vector2d(-x, -y)

  def +(that: Vector2d): Vector2d = Vector2d(x + that.x, y + that.y)

  def -(that: Vector2d): Vector2d = Vector2d(x - that.x, y - that.y)

  def *(value: Double): Vector2d = Vector2d(x * value, y * value)

  def /(value: Double): Vector2d = Vector2d(x / value, y / value)
}

object Vector2d {
  def fromComponents[T <% Double](components: Seq[T]): Vector2d = components match {
    case Seq(x, y) => Vector2d(x, y)
    case _ => throw new IllegalArgumentException("Vector2d requires 2 components")
  }

  def polar(radius: Double, angle: Double): Vector2d =
    Vector2d(radius * math.cos(angle), radius * math.sin(angle))

  val Zero: Vector2d = Vector2d(0.0, 0.0)
}
