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

import scala.beans.BeanProperty
import scala.util.Random

final case class BoundingBox3d(x: Interval, y: Interval, z: Interval)
  extends Bounds[BoundingBox3d] with GeometricallyComparable[BoundingBox3d] {

  def this(components: (Interval, Interval, Interval)) =
    this(components.first, components.second, components.third)

  def components: (Interval, Interval, Interval) = (x, y, z)

  def component(index: Int): Interval = index match {
    case 0 => x
    case 1 => y
    case 2 => z
    case _ =>
      throw new IndexOutOfBoundsException(s"Index $index is out of bounds for BoundingBox3d")
  }

  override def bounds: BoundingBox3d = this

  override def isEqualTo(that: BoundingBox3d, tolerance: Double): Boolean =
    this.minVertex.isEqualTo(that.minVertex, tolerance) &&
    this.maxVertex.isEqualTo(that.maxVertex, tolerance)

  def isEmpty: Boolean = x.isEmpty || y.isEmpty || z.isEmpty

  def isWhole: Boolean = x.isWhole && y.isWhole && z.isWhole

  def isSingleton: Boolean = x.isSingleton && y.isSingleton && z.isSingleton

  def center: Point3d = Point3d(x.median, y.median, z.median)

  def minVertex: Point3d = Point3d(x.lowerBound, y.lowerBound, z.lowerBound)

  def maxVertex: Point3d = Point3d(x.upperBound, y.upperBound, z.upperBound)

  def vertices: (Point3d, Point3d, Point3d, Point3d, Point3d, Point3d, Point3d, Point3d) = (
    Point3d(x.lowerBound, y.lowerBound, z.lowerBound),
    Point3d(x.upperBound, y.lowerBound, z.lowerBound),
    Point3d(x.lowerBound, y.upperBound, z.lowerBound),
    Point3d(x.upperBound, y.upperBound, z.lowerBound),
    Point3d(x.lowerBound, y.lowerBound, z.upperBound),
    Point3d(x.upperBound, y.lowerBound, z.upperBound),
    Point3d(x.lowerBound, y.upperBound, z.upperBound),
    Point3d(x.upperBound, y.upperBound, z.upperBound)
  )

  def interpolated(u: Double, v: Double, w: Double): Point3d =
    Point3d(x.interpolated(u), y.interpolated(v), z.interpolated(w))

  def randomPoint: Point3d = randomPoint(Random)

  def randomPoint(generator: Random): Point3d =
    interpolated(generator.nextDouble, generator.nextDouble, generator.nextDouble)

  def hull(point: Point3d): BoundingBox3d =
    BoundingBox3d(x.hull(point.x), y.hull(point.y), z.hull(point.z))

  override def hull(that: BoundingBox3d): BoundingBox3d =
    BoundingBox3d(this.x.hull(that.x), this.y.hull(that.y), this.z.hull(that.z))

  def intersection(that: BoundingBox3d): BoundingBox3d = {
    val x = this.x.intersection(that.x)
    val y = this.y.intersection(that.y)
    val z = this.z.intersection(that.z)
    if (x.isEmpty || y.isEmpty || z.isEmpty) BoundingBox3d.Empty else BoundingBox3d(x, y, z)
  }

  def overlaps(that: BoundingBox3d): Boolean =
    this.x.overlaps(that.x) && this.y.overlaps(that.y) && this.z.overlaps(that.z)

  override def overlaps(that: BoundingBox3d, tolerance: Double): Boolean =
    this.x.overlaps(that.x, tolerance) &&
    this.y.overlaps(that.y, tolerance) &&
    this.z.overlaps(that.z, tolerance)

  def contains(point: Point3d): Boolean =
    x.contains(point.x) && y.contains(point.y) && z.contains(point.z)

  def contains(point: Point3d, tolerance: Double): Boolean =
    x.contains(point.x, tolerance) &&
    y.contains(point.y, tolerance) &&
    z.contains(point.z, tolerance)

  def contains(that: BoundingBox3d): Boolean =
    this.x.contains(that.x) && this.y.contains(that.y) && this.z.contains(that.z)

  override def contains(that: BoundingBox3d, tolerance: Double): Boolean =
    this.x.contains(that.x, tolerance) &&
    this.y.contains(that.y, tolerance) &&
    this.z.contains(that.z, tolerance)

  override def bisected(dimensionIndex: Int): (BoundingBox3d, BoundingBox3d) =
    (dimensionIndex % 3) match {
      case 0 => {
        val (xLower, xUpper) = x.bisected
        (BoundingBox3d(xLower, y, z), BoundingBox3d(xUpper, y, z))
      }
      case 1 => {
        val (yLower, yUpper) = y.bisected
        (BoundingBox3d(x, yLower, z), BoundingBox3d(x, yUpper, z))
      }
      case _ => {
        val (zLower, zUpper) = z.bisected
        (BoundingBox3d(x, y, zLower), BoundingBox3d(x, y, zUpper))
      }
    }

  def +(vector: Vector3d): BoundingBox3d = BoundingBox3d(x + vector.x, y + vector.y, z + vector.z)

  def +(vectorBoundingBox: VectorBoundingBox3d): BoundingBox3d =
    BoundingBox3d(x + vectorBoundingBox.x, y + vectorBoundingBox.y, z + vectorBoundingBox.z)

  def -(vector: Vector3d): BoundingBox3d =
    BoundingBox3d(x - vector.x, y - vector.y, z - vector.z)

  def -(vectorBoundingBox: VectorBoundingBox3d): BoundingBox3d =
    BoundingBox3d(x - vectorBoundingBox.x, y - vectorBoundingBox.y, z - vectorBoundingBox.z)

  def -(point: Point3d): VectorBoundingBox3d =
    VectorBoundingBox3d(x - point.x, y - point.y, z - point.z)

  def -(that: BoundingBox3d): VectorBoundingBox3d =
    VectorBoundingBox3d(this.x - that.x, this.y - that.y, this.z - that.z)
}

object BoundingBox3d {
  def apply(components: (Interval, Interval, Interval)): BoundingBox3d =
    new BoundingBox3d(components)

  def hullOf(points: (Point3d, Point3d)): BoundingBox3d = points.first.hull(points.second)

  def hullOf(points: (Point3d, Point3d, Point3d)): BoundingBox3d =
    points.first.hull(points.second).hull(points.third)

  def hullOf(points: (Point3d, Point3d, Point3d, Point3d)): BoundingBox3d =
    points.first.hull(points.second).hull(points.third).hull(points.fourth)

  def hullOf(points: (Point3d, Point3d, Point3d, Point3d, Point3d)): BoundingBox3d =
    points.first.hull(points.second).hull(points.third).hull(points.fourth).hull(points.fifth)

  def hullOf(points: (Point3d, Point3d, Point3d, Point3d, Point3d, Point3d)): BoundingBox3d =
    points.first.
      hull(points.second).
      hull(points.third).
      hull(points.fourth).
      hull(points.fifth).
      hull(points.sixth)

  def hullOf(
    points: (Point3d, Point3d, Point3d, Point3d, Point3d, Point3d, Point3d)
  ): BoundingBox3d =
    points.first.
      hull(points.second).
      hull(points.third).
      hull(points.fourth).
      hull(points.fifth).
      hull(points.sixth).
      hull(points.seventh)

  def hullOf(
    points: (Point3d, Point3d, Point3d, Point3d, Point3d, Point3d, Point3d, Point3d)
  ): BoundingBox3d =
    points.first.
      hull(points.second).
      hull(points.third).
      hull(points.fourth).
      hull(points.fifth).
      hull(points.sixth).
      hull(points.seventh).
      hull(points.eigth)

  @BeanProperty
  val Empty: BoundingBox3d = BoundingBox3d(Interval.Empty, Interval.Empty, Interval.Empty)

  @BeanProperty
  val Whole: BoundingBox3d = BoundingBox3d(Interval.Whole, Interval.Whole, Interval.Whole)

  @BeanProperty
  val Unit: BoundingBox3d = BoundingBox3d(Interval.Unit, Interval.Unit, Interval.Unit)
}
