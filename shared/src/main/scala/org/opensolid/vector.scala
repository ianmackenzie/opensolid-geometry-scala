package org.opensolid

import scala.math

trait Vector {
  def squaredLength: Double
  def length: Double = math.sqrt(squaredLength)

  def isZero(precision: Double = DefaultPrecision): Boolean = {
    squaredLength.isZero(precision * precision)
  }
}

case class Vector2d(x: Double, y: Double) extends Vector with TransformableAsVector2d[Vector2d] {
  override def squaredLength: Double = x * x + y * y

  override def transformedBy(transformation: Transformation2d): Vector2d = {
    transformation.transform(this)
  }

  def normalized: UnitVector2d = {
    if (x != 0.0 || y != 0.0) {
      val length = this.length
      UnitVector2d(x / length, y / length)
    } else {
      UnitVector2d.Zero
    }
  }

  def unary_- : Vector2d = Vector2d(-x, -y)

  final def +(that: Vector2d): Vector2d = Vector2d(x + that.x, y + that.y)
  final def -(that: Vector2d): Vector2d = Vector2d(x - that.x, y - that.y)
  final def *(value: Double): Vector2d = Vector2d(x * value, y * value)
  final def /(value: Double): Vector2d = Vector2d(x / value, y / value)
}

object Vector2d {
  val Zero: Vector2d = Vector2d(0.0, 0.0)
}

case class Vector3d(x: Double, y: Double, z: Double) extends Vector with TransformableAsVector3d[Vector3d] {
  override def squaredLength: Double = x * x + y * y + z * z

  override def transformedBy(transformation: Transformation3d): Vector3d = {
    transformation.transform(this)
  }

  def normalized: UnitVector3d = {
    if (x != 0.0 || y != 0.0 || z != 0.0) {
      val length = this.length
      UnitVector3d(x / length, y / length, z / length)
    } else {
      UnitVector3d.Zero
    }
  }

  def unary_- : Vector3d = Vector3d(-x, -y, -z)

  final def +(that: Vector3d): Vector3d = Vector3d(x + that.x, y + that.y, z + that.z)
  final def -(that: Vector3d): Vector3d = Vector3d(x - that.x, y - that.y, z - that.z)
  final def *(value: Double): Vector3d = Vector3d(x * value, y * value, z * value)
  final def /(value: Double): Vector3d = Vector3d(x / value, y / value, z / value)
}

object Vector3d {
  val Zero: Vector3d = Vector3d(0.0, 0.0, 0.0)
}
