package org.opensolid

import scala.scalajs.js
import js.annotation.JSExport
import js.annotation.JSExportAll

@JSExport("UnitVector2d")
@JSExportAll
class UnitVector2d(x: Double, y: Double) extends Vector2d(x, y) {
  override def squaredLength = if (x != 0.0 || y != 0.0) 1.0 else 0.0
  override def length = squaredLength
  override def normalized = this
}

@JSExport("UnitVector2d_StaticMembers")
object UnitVector2d {
  def apply(x: Double, y: Double) = new UnitVector2d(x, y)

  @JSExport
  val Zero = UnitVector2d(0.0, 0.0)

  @JSExport
  val X = UnitVector2d(1.0, 0.0)

  @JSExport
  val Y = UnitVector2d(0.0, 1.0)
}

@JSExport("UnitVector3d")
@JSExportAll
class UnitVector3d(x: Double, y: Double, z: Double) extends Vector3d(x, y, z) {
  override def squaredLength = if (x != 0.0 || y != 0.0 || z != 0.0) 1.0 else 0.0
  override def length = squaredLength
  override def normalized = this
}

@JSExport("UnitVector3d_StaticMembers")
object UnitVector3d {
  def apply(x: Double, y: Double, z: Double) = new UnitVector3d(x, y, z)

  @JSExport
  val Zero = UnitVector3d(0.0, 0.0, 0.0)

  @JSExport
  val X = UnitVector3d(1.0, 0.0, 0.0)

  @JSExport
  val Y = UnitVector3d(0.0, 1.0, 0.0)

  @JSExport
  val Z = UnitVector3d(0.0, 0.0, 1.0)
}
