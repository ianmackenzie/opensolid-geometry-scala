package org.opensolid

import scala.math

import scala.scalajs.js.annotation.{JSExport, JSExportAll}

@JSExportAll
trait Vector {
  def squaredLength: Double

  def length: Double = math.sqrt(squaredLength)

  def isZero(precision: Double = DefaultPrecision): Boolean = {
    squaredLength.isZero(precision * precision)
  }
}

@JSExport("Vector2d")
@JSExportAll
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

  @JSExport("negated")
  def unary_- : Vector2d = Vector2d(-x, -y)

  @JSExport("plus")
  final def +(that: Vector2d): Vector2d = Vector2d(x + that.x, y + that.y)

  @JSExport("minus")
  final def -(that: Vector2d): Vector2d = Vector2d(x - that.x, y - that.y)

  @JSExport("times")
  final def *(value: Double): Vector2d = Vector2d(x * value, y * value)

  @JSExport("dividedBy")
  final def /(value: Double): Vector2d = Vector2d(x / value, y / value)
}

@JSExport("Vector2d$StaticMembers")
object Vector2d {
  @JSExport("ZERO")
  val Zero: Vector2d = Vector2d(0.0, 0.0)
}

@JSExport("Vector3d")
@JSExportAll
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

  @JSExport("negated")
  def unary_- : Vector3d = Vector3d(-x, -y, -z)

  @JSExport("plus")
  final def +(that: Vector3d): Vector3d = Vector3d(x + that.x, y + that.y, z + that.z)

  @JSExport("minus")
  final def -(that: Vector3d): Vector3d = Vector3d(x - that.x, y - that.y, z - that.z)

  @JSExport("times")
  final def *(value: Double): Vector3d = Vector3d(x * value, y * value, z * value)

  @JSExport("dividedBy")
  final def /(value: Double): Vector3d = Vector3d(x / value, y / value, z / value)
}

@JSExport("Vector3d$StaticMembers")
object Vector3d {
  @JSExport("ZERO")
  val Zero: Vector3d = Vector3d(0.0, 0.0, 0.0)
}
