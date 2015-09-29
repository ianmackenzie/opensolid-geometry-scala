package org.opensolid

final class UnitVector2d(x: Double, y: Double) extends Vector2d(x, y) {
  override def squaredLength = if (x != 0.0 || y != 0.0) 1.0 else 0.0
  override def length = squaredLength
  override def normalized = this
  override def toString = s"UnitVector2d($x,$y)"
}

object UnitVector2d {
  def apply(x: Double, y: Double) = new UnitVector2d(x, y)

  val Zero = UnitVector2d(0.0, 0.0)
  val X = UnitVector2d(1.0, 0.0)
  val Y = UnitVector2d(0.0, 1.0)
}

final class UnitVector3d(x: Double, y: Double, z: Double) extends Vector3d(x, y, z) {
  override def squaredLength = if (x != 0.0 || y != 0.0 || z != 0.0) 1.0 else 0.0
  override def length = squaredLength
  override def normalized = this
  override def toString = s"UnitVector3d($x,$y,$z)"
}

object UnitVector3d {
  def apply(x: Double, y: Double, z: Double) = new UnitVector3d(x, y, z)

  val Zero = UnitVector3d(0.0, 0.0, 0.0)
  val X = UnitVector3d(1.0, 0.0, 0.0)
  val Y = UnitVector3d(0.0, 1.0, 0.0)
  val Z = UnitVector3d(0.0, 0.0, 1.0)
}
