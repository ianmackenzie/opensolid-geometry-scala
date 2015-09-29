package org.opensolid

final class UnitVector2d(x: Double, y: Double) extends Vector2d(x, y) {
  override def toString: String = s"UnitVector2d($x,$y)"

  override def squaredLength: Double = if (x != 0.0 || y != 0.0) 1.0 else 0.0
  override def length: Double = squaredLength
  override def normalized: UnitVector2d = this
  override def unary_- : UnitVector2d = this

  override def transformedBy(transformation: Transformation2d): UnitVector2d = {
    transformation.transform(this)
  }
}

object UnitVector2d {
  def apply(x: Double, y: Double): UnitVector2d = new UnitVector2d(x, y)

  val Zero: UnitVector2d = UnitVector2d(0.0, 0.0)
  val X: UnitVector2d = UnitVector2d(1.0, 0.0)
  val Y: UnitVector2d = UnitVector2d(0.0, 1.0)
}

final class UnitVector3d(x: Double, y: Double, z: Double) extends Vector3d(x, y, z) {
  override def toString: String = s"UnitVector3d($x,$y,$z)"

  override def squaredLength: Double = if (x != 0.0 || y != 0.0 || z != 0.0) 1.0 else 0.0
  override def length: Double = squaredLength
  override def normalized: UnitVector3d = this
  override def unary_- : UnitVector3d = this

  override def transformedBy(transformation: Transformation3d): UnitVector3d = {
    transformation.transform(this)
  }
}

object UnitVector3d {
  def apply(x: Double, y: Double, z: Double): UnitVector3d = new UnitVector3d(x, y, z)

  val Zero: UnitVector3d = UnitVector3d(0.0, 0.0, 0.0)
  val X: UnitVector3d = UnitVector3d(1.0, 0.0, 0.0)
  val Y: UnitVector3d = UnitVector3d(0.0, 1.0, 0.0)
  val Z: UnitVector3d = UnitVector3d(0.0, 0.0, 1.0)
}
