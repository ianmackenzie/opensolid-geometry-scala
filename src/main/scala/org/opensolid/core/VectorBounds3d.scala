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

final case class VectorBounds3d(x: Interval, y: Interval, z: Interval) {
  def components: (Interval, Interval, Interval) =
    (x, y, z)

  def component(index: Int): Interval = index match {
    case 0 => x
    case 1 => y
    case 2 => z
    case _ =>
      throw new IndexOutOfBoundsException(s"Index $index is out of bounds for VectorBounds3d")
  }

  def isEmpty: Boolean =
    x.isEmpty || y.isEmpty || z.isEmpty

  def isWhole: Boolean =
    x.isWhole && y.isWhole && z.isWhole

  def isSingleton: Boolean =
    x.isSingleton && y.isSingleton && z.isSingleton

  def hull(vector: Vector3d): VectorBounds3d =
    VectorBounds3d(x.hull(vector.x), y.hull(vector.y), z.hull(vector.z))

  def hull(that: VectorBounds3d): VectorBounds3d =
    VectorBounds3d(this.x.hull(that.x), this.y.hull(that.y), this.z.hull(that.z))

  def intersection(that: VectorBounds3d): VectorBounds3d = {
    val x = this.x.intersection(that.x)
    val y = this.y.intersection(that.y)
    val z = this.z.intersection(that.z)
    if (x.isEmpty || y.isEmpty || z.isEmpty) {
      VectorBounds3d.Empty
    } else {
      VectorBounds3d(x, y, z)
    }
  }

  def overlaps(that: VectorBounds3d): Boolean =
    this.x.overlaps(that.x) && this.y.overlaps(that.y) && this.z.overlaps(that.z)

  def overlaps(that: VectorBounds3d, tolerance: Double): Boolean =
    this.x.overlaps(that.x, tolerance) &&
    this.y.overlaps(that.y, tolerance) &&
    this.z.overlaps(that.z, tolerance)

  def contains(vector: Vector3d): Boolean =
    x.contains(vector.x) && y.contains(vector.y) && z.contains(vector.z)

  def contains(vector: Vector3d, tolerance: Double): Boolean =
    x.contains(vector.x, tolerance) &&
    y.contains(vector.y, tolerance) &&
    z.contains(vector.z, tolerance)

  def contains(that: VectorBounds3d): Boolean =
    this.x.contains(that.x) && this.y.contains(that.y) && this.z.contains(that.z)

  def contains(that: VectorBounds3d, tolerance: Double): Boolean =
    this.x.contains(that.x, tolerance) &&
    this.y.contains(that.y, tolerance) &&
    this.z.contains(that.z, tolerance)
}

object VectorBounds3d {
  def fromComponents(components: (Interval, Interval, Interval)): VectorBounds3d =
    components match {
      case (x, y, z) => VectorBounds3d(x, y, z)
    }

  def singleton(vector: Vector3d): VectorBounds3d =
    VectorBounds3d(
      Interval.singleton(vector.x),
      Interval.singleton(vector.y),
      Interval.singleton(vector.z)
    )

  val Empty: VectorBounds3d = VectorBounds3d(Interval.Empty, Interval.Empty, Interval.Empty)

  val Whole: VectorBounds3d = VectorBounds3d(Interval.Whole, Interval.Whole, Interval.Whole)

  val Zero: VectorBounds3d = VectorBounds3d(Interval.Zero, Interval.Zero, Interval.Zero)
}
