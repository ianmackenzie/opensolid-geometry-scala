////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  OpenSolid is a generic library for the representation and manipulation    //
//  of geometric objects such as points, curves, surfaces, and volumes.       //
//                                                                            //
//  Copyright 2007-2015 by Ian Mackenzie                                      //
//  ian.e.mackenzie@gmail.com                                                 //
//                                                                            //
//  This Source Code Form is subject to the terms of the Mozilla Public       //
//  License, v. 2.0. If a copy of the MPL was not distributed with this file, //
//  you can obtain one at http://mozilla.org/MPL/2.0/.                        //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

package org.opensolid.core

final case class VectorBox3d(x: Interval, y: Interval, z: Interval) {
  def components: Array[Interval] = Array(x, y, z)

  def component(index: Int): Interval = index match {
    case 0 => x
    case 1 => y
    case 2 => z
    case _ => throw new IndexOutOfBoundsException(s"Index $index is out of bounds for VectorBox3d")
  }

  def isEmpty: Boolean = x.isEmpty || y.isEmpty || z.isEmpty

  def isWhole: Boolean = x.isWhole && y.isWhole && z.isWhole

  def isSingleton: Boolean = x.isSingleton && y.isSingleton && z.isSingleton

  def hull(vector: Vector3d): VectorBox3d =
    VectorBox3d(x.hull(vector.x), y.hull(vector.y), z.hull(vector.z))

  def hull(that: VectorBox3d): VectorBox3d =
    VectorBox3d(this.x.hull(that.x), this.y.hull(that.y), this.z.hull(that.z))

  def intersection(that: VectorBox3d): VectorBox3d = {
    val x = this.x.intersection(that.x)
    val y = this.y.intersection(that.y)
    val z = this.z.intersection(that.z)
    if (x.isEmpty || y.isEmpty || z.isEmpty) VectorBox3d.Empty else VectorBox3d(x, y, z)
  }

  def overlaps(that: VectorBox3d): Boolean =
    this.x.overlaps(that.x) && this.y.overlaps(that.y) && this.z.overlaps(that.z)

  def overlaps(that: VectorBox3d, tolerance: Double): Boolean =
    this.x.overlaps(that.x, tolerance) &&
    this.y.overlaps(that.y, tolerance) &&
    this.z.overlaps(that.z, tolerance)

  def contains(vector: Vector3d): Boolean =
    x.contains(vector.x) && y.contains(vector.y) && z.contains(vector.z)

  def contains(vector: Vector3d, tolerance: Double): Boolean =
    x.contains(vector.x, tolerance) &&
    y.contains(vector.y, tolerance) &&
    z.contains(vector.z, tolerance)

  def contains(that: VectorBox3d): Boolean =
    this.x.contains(that.x) && this.y.contains(that.y) && this.z.contains(that.z)

  def contains(that: VectorBox3d, tolerance: Double): Boolean =
    this.x.contains(that.x, tolerance) &&
    this.y.contains(that.y, tolerance) &&
    this.z.contains(that.z, tolerance)

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

  val Empty: VectorBox3d = VectorBox3d(Interval.Empty, Interval.Empty, Interval.Empty)

  val Whole: VectorBox3d = VectorBox3d(Interval.Whole, Interval.Whole, Interval.Whole)

  val Zero: VectorBox3d = VectorBox3d(Interval.Zero, Interval.Zero, Interval.Zero)
}
