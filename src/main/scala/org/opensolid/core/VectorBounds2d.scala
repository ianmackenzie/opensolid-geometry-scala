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

final case class VectorBounds2d(x: Interval, y: Interval) {
  def components: (Interval, Interval) =
    (x, y)

  def component(index: Int): Interval = index match {
    case 0 => x
    case 1 => y
    case _ =>
      throw new IndexOutOfBoundsException(s"Index $index is out of bounds for VectorBounds2d")
  }

  def isEmpty: Boolean =
    x.isEmpty || y.isEmpty

  def isWhole: Boolean =
    x.isWhole && y.isWhole

  def isSingleton: Boolean =
    x.isSingleton && y.isSingleton

  def expandedBy(value: Double): VectorBounds2d =
    VectorBounds2d(x.expandedBy(value), y.expandedBy(value))

  def hull(vector: Vector2d): VectorBounds2d =
    VectorBounds2d(x.hull(vector.x), y.hull(vector.y))

  def hull(that: VectorBounds2d): VectorBounds2d =
    VectorBounds2d(this.x.hull(that.x), this.y.hull(that.y))

  def intersection(that: VectorBounds2d): VectorBounds2d = {
    val x = this.x.intersection(that.x)
    val y = this.y.intersection(that.y)
    if (x.isEmpty || y.isEmpty) VectorBounds2d.Empty else VectorBounds2d(x, y)
  }

  def overlaps(that: VectorBounds2d): Boolean =
    this.x.overlaps(that.x) && this.y.overlaps(that.y)

  def contains(vector: Vector2d): Boolean =
    x.contains(vector.x) && y.contains(vector.y)

  def contains(that: VectorBounds2d): Boolean =
    this.x.contains(that.x) && this.y.contains(that.y)
}

object VectorBounds2d {
  def fromComponents(components: (Interval, Interval)): VectorBounds2d = components match {
    case (x, y) => VectorBounds2d(x, y)
  }

  def singleton(vector: Vector2d): VectorBounds2d =
    VectorBounds2d(Interval.singleton(vector.x), Interval.singleton(vector.y))

  val Empty: VectorBounds2d = VectorBounds2d(Interval.Empty, Interval.Empty)

  val Whole: VectorBounds2d = VectorBounds2d(Interval.Whole, Interval.Whole)

  val Zero: VectorBounds2d = VectorBounds2d(Interval.Zero, Interval.Zero)
}
