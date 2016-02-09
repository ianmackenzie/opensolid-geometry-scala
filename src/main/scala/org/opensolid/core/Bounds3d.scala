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

final case class Bounds3d(x: Interval, y: Interval, z: Interval)
  extends Bounded[Bounds3d] {

  def components: (Interval, Interval, Interval) =
    (x, y, z)

  def component(index: Int): Interval = index match {
    case 0 => x
    case 1 => y
    case 2 => z
    case _ => throw new IndexOutOfBoundsException(s"Index $index is out of bounds for Bounds3d")
  }

  override def bounds: Bounds3d = this

  def isEmpty: Boolean =
    x.isEmpty || y.isEmpty || z.isEmpty

  def isWhole: Boolean =
    x.isWhole && y.isWhole && z.isWhole

  def isSingleton: Boolean =
    x.isSingleton && y.isSingleton && z.isSingleton

  def hull(point: Point3d): Bounds3d =
    Bounds3d(x.hull(point.x), y.hull(point.y), z.hull(point.z))

  def hull(that: Bounds3d): Bounds3d =
    Bounds3d(this.x.hull(that.x), this.y.hull(that.y), this.z.hull(that.z))

  def intersection(that: Bounds3d): Bounds3d = {
    val x = this.x.intersection(that.x)
    val y = this.y.intersection(that.y)
    val z = this.z.intersection(that.z)
    if (x.isEmpty || y.isEmpty || z.isEmpty) Bounds3d.Empty else Bounds3d(x, y, z)
  }

  def overlaps(that: Bounds3d): Boolean =
    this.x.overlaps(that.x) && this.y.overlaps(that.y) && this.z.overlaps(that.z)

  def overlaps(that: Bounds3d, tolerance: Double): Boolean =
    this.x.overlaps(that.x, tolerance) &&
    this.y.overlaps(that.y, tolerance) &&
    this.z.overlaps(that.z, tolerance)

  def contains(point: Point3d): Boolean =
    x.contains(point.x) && y.contains(point.y) && z.contains(point.z)

  def contains(point: Point3d, tolerance: Double): Boolean =
    x.contains(point.x, tolerance) &&
    y.contains(point.y, tolerance) &&
    z.contains(point.z, tolerance)

  def contains(that: Bounds3d): Boolean =
    this.x.contains(that.x) && this.y.contains(that.y) && this.z.contains(that.z)

  def contains(that: Bounds3d, tolerance: Double): Boolean =
    this.x.contains(that.x, tolerance) &&
    this.y.contains(that.y, tolerance) &&
    this.z.contains(that.z, tolerance)

  def bisected(index: Int): (Bounds3d, Bounds3d) =
    (index % 3) match {
      case 0 => {
        val (xLower, xUpper) = x.bisected
        (Bounds3d(xLower, y, z), Bounds3d(xUpper, y, z))
      }
      case 1 => {
        val (yLower, yUpper) = y.bisected
        (Bounds3d(x, yLower, z), Bounds3d(x, yUpper, z))
      }
      case _ => {
        val (zLower, zUpper) = z.bisected
        (Bounds3d(x, y, zLower), Bounds3d(x, y, zUpper))
      }
    }
}

object Bounds3d {
  def fromComponents(components: (Interval, Interval, Interval)): Bounds3d = components match {
    case (x, y, z) => Bounds3d(x, y, z)
  }

  def singleton(point: Point3d): Bounds3d =
    Bounds3d(Interval.singleton(point.x), Interval.singleton(point.y), Interval.singleton(point.z))

  def hullOf(points: (Point3d, Point3d)): Bounds3d = points match {
    case (first, second) => first.hull(second)
  }

  def hullOf(points: (Point3d, Point3d, Point3d)): Bounds3d = points match {
    case (first, second, third) => first.hull(second).hull(third)
  }

  val Empty: Bounds3d = Bounds3d(Interval.Empty, Interval.Empty, Interval.Empty)

  val Whole: Bounds3d = Bounds3d(Interval.Whole, Interval.Whole, Interval.Whole)

  val Unit: Bounds3d = Bounds3d(Interval.Unit, Interval.Unit, Interval.Unit)

  implicit val Traits: Bounds[Bounds3d] = new Bounds[Bounds3d] {
    override def component(bounds: Bounds3d, index: Int): Interval =
      bounds.component(index)

    override def overlaps(first: Bounds3d, second: Bounds3d, tolerance: Double): Boolean =
      first.overlaps(second, tolerance)

    override def contains(first: Bounds3d, second: Bounds3d, tolerance: Double): Boolean =
      first.contains(second, tolerance)

    override def bisected(bounds: Bounds3d, index: Int): (Bounds3d, Bounds3d) =
      bounds.bisected(index)

    override def hull(first: Bounds3d, second: Bounds3d): Bounds3d =
      first.hull(second)

    override val NumDimensions: Int = 3

    override val Empty: Bounds3d = Bounds3d.Empty
  }
}
