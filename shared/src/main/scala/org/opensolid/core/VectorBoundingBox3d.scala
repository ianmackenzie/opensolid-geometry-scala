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

import scala.util.Random

final case class VectorBoundingBox3d(x: Interval, y: Interval, z: Interval) {
  def components: Array[Interval] = Array(x, y, z)

  def component(index: Int): Interval = index match {
    case 0 => x
    case 1 => y
    case 2 => z
    case _ =>
      throw new IndexOutOfBoundsException(s"Index $index is out of bounds for VectorBoundingBox3d")
  }

  def isEmpty: Boolean = x.isEmpty || y.isEmpty || z.isEmpty

  def isWhole: Boolean = x.isWhole && y.isWhole && z.isWhole

  def isSingleton: Boolean = x.isSingleton && y.isSingleton && z.isSingleton

  def center: Vector3d = Vector3d(x.median, y.median, z.median)

  def interpolated(u: Double, v: Double, w: Double): Vector3d =
    Vector3d(x.interpolated(u), y.interpolated(v), z.interpolated(w))

  def randomVector: Vector3d = randomVector(Random)

  def randomVector(generator: Random): Vector3d =
    interpolated(generator.nextDouble, generator.nextDouble, generator.nextDouble)

  def hull(vector: Vector3d): VectorBoundingBox3d =
    VectorBoundingBox3d(x.hull(vector.x), y.hull(vector.y), z.hull(vector.z))

  def hull(that: VectorBoundingBox3d): VectorBoundingBox3d =
    VectorBoundingBox3d(this.x.hull(that.x), this.y.hull(that.y), this.z.hull(that.z))

  def intersection(that: VectorBoundingBox3d): VectorBoundingBox3d = {
    val x = this.x.intersection(that.x)
    val y = this.y.intersection(that.y)
    val z = this.z.intersection(that.z)
    if (x.isEmpty || y.isEmpty || z.isEmpty) {
      VectorBoundingBox3d.Empty
    } else {
      VectorBoundingBox3d(x, y, z)
    }
  }

  def overlaps(that: VectorBoundingBox3d): Boolean =
    this.x.overlaps(that.x) && this.y.overlaps(that.y) && this.z.overlaps(that.z)

  def overlaps(that: VectorBoundingBox3d, tolerance: Double): Boolean =
    this.x.overlaps(that.x, tolerance) &&
    this.y.overlaps(that.y, tolerance) &&
    this.z.overlaps(that.z, tolerance)

  def contains(vector: Vector3d): Boolean =
    x.contains(vector.x) && y.contains(vector.y) && z.contains(vector.z)

  def contains(vector: Vector3d, tolerance: Double): Boolean =
    x.contains(vector.x, tolerance) &&
    y.contains(vector.y, tolerance) &&
    z.contains(vector.z, tolerance)

  def contains(that: VectorBoundingBox3d): Boolean =
    this.x.contains(that.x) && this.y.contains(that.y) && this.z.contains(that.z)

  def contains(that: VectorBoundingBox3d, tolerance: Double): Boolean =
    this.x.contains(that.x, tolerance) &&
    this.y.contains(that.y, tolerance) &&
    this.z.contains(that.z, tolerance)

  def squaredLength: Interval = x * x + y * y + z * z

  def length: Interval = Interval.sqrt(squaredLength)

  def normalized: VectorBoundingBox3d = directionBoundingBox.vectorBoundingBox

  def directionBoundingBox: DirectionBoundingBox3d = {
    if (this == VectorBoundingBox3d.Zero) {
      DirectionBoundingBox3d.Empty
    } else {
      val length = this.length
      DirectionBoundingBox3d(x / length, y / length, z / length)
    }
  }

  def unary_- : VectorBoundingBox3d = VectorBoundingBox3d(-x, -y, -z)

  def +(vector: Vector3d): VectorBoundingBox3d =
    VectorBoundingBox3d(x + vector.x, y + vector.y, z + vector.z)

  def +(that: VectorBoundingBox3d): VectorBoundingBox3d =
    VectorBoundingBox3d(this.x + that.x, this.y + that.y, this.z + that.z)

  def -(vector: Vector3d): VectorBoundingBox3d =
    VectorBoundingBox3d(x - vector.x, y - vector.y, z - vector.z)

  def -(that: VectorBoundingBox3d): VectorBoundingBox3d =
    VectorBoundingBox3d(this.x - that.x, this.y - that.y, this.z - that.z)

  def *(sign: Sign): VectorBoundingBox3d = sign match {
    case Sign.Positive => this
    case Sign.Negative => -this
    case _ => VectorBoundingBox3d.Empty
  }

  def *(value: Double): VectorBoundingBox3d = VectorBoundingBox3d(x * value, y * value, z * value)

  def *(interval: Interval): VectorBoundingBox3d =
    VectorBoundingBox3d(x * interval, y * interval, z * interval)

  def /(value: Double): VectorBoundingBox3d = VectorBoundingBox3d(x / value, y / value, z / value)

  def /(interval: Interval): VectorBoundingBox3d =
    VectorBoundingBox3d(x / interval, y / interval, z / interval)

  def dot(vector: Vector3d): Interval = x * vector.x + y * vector.y + z * vector.z

  def dot(direction: Direction3d): Interval = dot(direction.vector)

  def dot(that: VectorBoundingBox3d): Interval = this.x * that.x + this.y * that.y + this.z * that.z

  def dot(directionBoundingBox: DirectionBoundingBox3d): Interval =
    dot(directionBoundingBox.vectorBoundingBox)

  def cross(vector: Vector3d): VectorBoundingBox3d =
    VectorBoundingBox3d(
      y * vector.z - z * vector.y,
      z * vector.x - x * vector.z,
      x * vector.y - y * vector.x
    )

  def cross(direction: Direction3d): VectorBoundingBox3d = cross(direction.vector)

  def cross(that: VectorBoundingBox3d): VectorBoundingBox3d =
    VectorBoundingBox3d(
      this.y * that.z - this.z * that.y,
      this.z * that.x - this.x * that.z,
      this.x * that.y - this.y * that.x
    )

  def cross(directionBoundingBox: DirectionBoundingBox3d): VectorBoundingBox3d =
    cross(directionBoundingBox.vectorBoundingBox)
}

object VectorBoundingBox3d {
  def fromComponents(components: Seq[Interval]): VectorBoundingBox3d = components match {
    case Seq(x, y, z) => VectorBoundingBox3d(x, y, z)
    case _ => throw new IllegalArgumentException("VectorBoundingBox3d requires 3 components")
  }

  val Empty: VectorBoundingBox3d =
    VectorBoundingBox3d(Interval.Empty, Interval.Empty, Interval.Empty)

  val Whole: VectorBoundingBox3d =
    VectorBoundingBox3d(Interval.Whole, Interval.Whole, Interval.Whole)

  val Zero: VectorBoundingBox3d =
    VectorBoundingBox3d(Interval.Zero, Interval.Zero, Interval.Zero)
}
