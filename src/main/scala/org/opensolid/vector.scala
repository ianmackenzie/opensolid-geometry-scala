package org.opensolid

import scala.math

import scala.scalajs.js
import js.annotation.JSExport
import js.annotation.JSExportAll

import org.opensolid.utils._

@JSExportAll
trait Vector {
  def squaredLength: Double
  def length = math.sqrt(squaredLength)
  def isZero(precision: Double = 1e-12) = squaredLength.isZero(precision * precision)
}

@JSExport("Vector2d")
@JSExportAll
case class Vector2d(x: Double, y: Double) extends Vector {
  def squaredLength = x * x + y * y

  def normalized = {
    if (x != 0.0 || y != 0.0) {
      val length = this.length
      UnitVector2d(x / length, y / length)
    } else {
      UnitVector2d.Zero
    }
  }
}

@JSExport("Vector2d_StaticMembers")
object Vector2d {
  @JSExport("ZERO")
  val Zero = Vector2d(0.0, 0.0)
}

@JSExport("Vector3d")
@JSExportAll
case class Vector3d(x: Double, y: Double, z: Double) extends Vector {
  def squaredLength = x * x + y * y + z * z

  def normalized = {
    if (x != 0.0 || y != 0.0 || z != 0.0) {
      val length = this.length
      UnitVector3d(x / length, y / length, z / length)
    } else {
      UnitVector3d.Zero
    }
  }
}

@JSExport("Vector3d_StaticMembers")
object Vector3d {
  @JSExport("ZERO")
  val Zero = Vector3d(0.0, 0.0, 0.0)
}
