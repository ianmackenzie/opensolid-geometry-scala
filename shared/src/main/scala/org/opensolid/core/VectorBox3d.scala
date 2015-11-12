package org.opensolid.core

final case class VectorBox3d(x: Interval, y: Interval, z: Interval) {
  def components: Array[Interval] = Array(x, y, z)

  def component(index: Int): Interval = index match {
    case 0 => x
    case 1 => y
    case 2 => z
    case _ => throw new IndexOutOfBoundsException(s"Index $index is out of bounds for VectorBox3d")
  }

  def squaredLength: Interval = x * x + y * y + z * z

  def length: Interval = Interval.sqrt(squaredLength)

  def normalized: VectorBox3d = direction.vector

  def direction: DirectionBox3d = {
    if (this == VectorBox3d.Zero) {
      DirectionBox3d.None
    } else {
      val length = this.length
      DirectionBox3d(x / length, y / length, z / length)
    }
  }

  def unary_- : VectorBox3d = VectorBox3d(-x, -y, -z)

  def +(vector: Vector3d): VectorBox3d = VectorBox3d(x + vector.x, y + vector.y, z + vector.z)

  def +(that: VectorBox3d): VectorBox3d =
    VectorBox3d(this.x + that.x, this.y + that.y, this.z + that.z)

  def -(vector: Vector3d): VectorBox3d = VectorBox3d(x - vector.x, y - vector.y, z - vector.z)

  def -(that: VectorBox3d): VectorBox3d =
    VectorBox3d(this.x - that.x, this.y - that.y, this.z - that.z)

  def *(sign: Sign): VectorBox3d = VectorBox3d(x * sign, y * sign, z * sign)

  def *(value: Double): VectorBox3d = VectorBox3d(x * value, y * value, z * value)

  def *(interval: Interval): VectorBox3d = VectorBox3d(x * interval, y * interval, z * interval)

  def /(value: Double): VectorBox3d = VectorBox3d(x / value, y / value, z / value)

  def /(interval: Interval): VectorBox3d = VectorBox3d(x / interval, y / interval, z / interval)

  def dot(vector: Vector3d): Interval = x * vector.x + y * vector.y + z * vector.z

  def dot(direction: Direction3d): Interval = x * direction.x + y * direction.y + z * direction.z

  def dot(that: VectorBox3d): Interval = this.x * that.x + this.y * that.y + this.z * that.z

  def dot(directionBox: DirectionBox3d): Interval =
    x * directionBox.x + y * directionBox.y + z * directionBox.z

  def cross(vector: Vector3d): VectorBox3d =
    VectorBox3d(
      y * vector.z - z * vector.y,
      z * vector.x - x * vector.z,
      x * vector.y - y * vector.x
    )

  def cross(direction: Direction3d): VectorBox3d = cross(direction.vector)

  def cross(that: VectorBox3d): VectorBox3d =
    VectorBox3d(
      this.y * that.z - this.z * that.y,
      this.z * that.x - this.x * that.z,
      this.x * that.y - this.y * that.x
    )

  def cross(directionBox: DirectionBox3d): VectorBox3d = cross(directionBox.vector)
}

object VectorBox3d {
  def fromComponents(components: Seq[Interval]): VectorBox3d = components match {
    case Seq(x, y, z) => VectorBox3d(x, y, z)
    case _ => throw new IllegalArgumentException("VectorBox3d requires 3 components")
  }

  val Zero: VectorBox3d = VectorBox3d(Interval.Zero, Interval.Zero, Interval.Zero)
}
