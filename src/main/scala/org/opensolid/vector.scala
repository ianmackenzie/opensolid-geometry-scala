package org.opensolid

import scala.math

trait Vector {
  def squaredLength: Double
  def length = math.sqrt(squaredLength)
  def isZero(precision: Double = DefaultPrecision) = squaredLength.isZero(precision * precision)
}

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

object Vector2d {
  val Zero = Vector2d(0.0, 0.0)
}

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

object Vector3d {
  val Zero = Vector3d(0.0, 0.0, 0.0)
}
