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

final case class Box3d(x: Interval, y: Interval, z: Interval)
  extends Bounded[Box3d] with GeometricallyComparable[Box3d] {

  def this(components: (Interval, Interval, Interval)) =
    this(components.first, components.second, components.third)

  def components: (Interval, Interval, Interval) = (x, y, z)

  def component(index: Int): Interval = index match {
    case 0 => x
    case 1 => y
    case 2 => z
    case _ => throw new IndexOutOfBoundsException(s"Index $index is out of bounds for Box3d")
  }

  override def equals(that: Box3d, tolerance: Double): Boolean =
    this.minVertex.equals(that.minVertex, tolerance) &&
    this.maxVertex.equals(that.maxVertex, tolerance)

  override def bounds: Box3d = this

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

  def hull(point: Point3d): Box3d = Box3d(x.hull(point.x), y.hull(point.y), z.hull(point.z))

  def hull(that: Box3d): Box3d =
    Box3d(this.x.hull(that.x), this.y.hull(that.y), this.z.hull(that.z))

  def intersection(that: Box3d): Box3d = {
    val x = this.x.intersection(that.x)
    val y = this.y.intersection(that.y)
    val z = this.z.intersection(that.z)
    if (x.isEmpty || y.isEmpty || z.isEmpty) Box3d.Empty else Box3d(x, y, z)
  }

  def overlaps(that: Box3d): Boolean =
    this.x.overlaps(that.x) && this.y.overlaps(that.y) && this.z.overlaps(that.z)

  def overlaps(that: Box3d, tolerance: Double): Boolean =
    this.x.overlaps(that.x, tolerance) &&
    this.y.overlaps(that.y, tolerance) &&
    this.z.overlaps(that.z, tolerance)

  def contains(point: Point3d): Boolean =
    x.contains(point.x) && y.contains(point.y) && z.contains(point.z)

  def contains(point: Point3d, tolerance: Double): Boolean =
    x.contains(point.x, tolerance) &&
    y.contains(point.y, tolerance) &&
    z.contains(point.z, tolerance)

  def contains(that: Box3d): Boolean =
    this.x.contains(that.x) && this.y.contains(that.y) && this.z.contains(that.z)

  def contains(that: Box3d, tolerance: Double): Boolean =
    this.x.contains(that.x, tolerance) &&
    this.y.contains(that.y, tolerance) &&
    this.z.contains(that.z, tolerance)

  def bisected(index: Int): (Box3d, Box3d) =
    (index % 3) match {
      case 0 => {
        val (xLower, xUpper) = x.bisected
        (Box3d(xLower, y, z), Box3d(xUpper, y, z))
      }
      case 1 => {
        val (yLower, yUpper) = y.bisected
        (Box3d(x, yLower, z), Box3d(x, yUpper, z))
      }
      case _ => {
        val (zLower, zUpper) = z.bisected
        (Box3d(x, y, zLower), Box3d(x, y, zUpper))
      }
    }

  def +(vector: Vector3d): Box3d = Box3d(x + vector.x, y + vector.y, z + vector.z)

  def +(vectorBox: VectorBox3d): Box3d = Box3d(x + vectorBox.x, y + vectorBox.y, z + vectorBox.z)

  def -(vector: Vector3d): Box3d = Box3d(x - vector.x, y - vector.y, z - vector.z)

  def -(vectorBox: VectorBox3d): Box3d = Box3d(x - vectorBox.x, y - vectorBox.y, z - vectorBox.z)

  def -(point: Point3d): VectorBox3d = VectorBox3d(x - point.x, y - point.y, z - point.z)

  def -(that: Box3d): VectorBox3d = VectorBox3d(this.x - that.x, this.y - that.y, this.z - that.z)
}

object Box3d {
  def apply(components: (Interval, Interval, Interval)): Box3d = new Box3d(components)

  def singleton(point: Point3d): Box3d =
    Box3d(Interval.singleton(point.x), Interval.singleton(point.y), Interval.singleton(point.z))

  def hullOf(points: (Point3d, Point3d)): Box3d = points.first.hull(points.second)

  def hullOf(points: (Point3d, Point3d, Point3d)): Box3d =
    points.first.hull(points.second).hull(points.third)

  def hullOf(points: (Point3d, Point3d, Point3d, Point3d)): Box3d =
    points.first.hull(points.second).hull(points.third).hull(points.fourth)

  def hullOf(points: (Point3d, Point3d, Point3d, Point3d, Point3d)): Box3d =
    points.first.hull(points.second).hull(points.third).hull(points.fourth).hull(points.fifth)

  def hullOf(points: (Point3d, Point3d, Point3d, Point3d, Point3d, Point3d)): Box3d =
    points.first.
      hull(points.second).
      hull(points.third).
      hull(points.fourth).
      hull(points.fifth).
      hull(points.sixth)

  def hullOf(points: (Point3d, Point3d, Point3d, Point3d, Point3d, Point3d, Point3d)): Box3d =
    points.first.
      hull(points.second).
      hull(points.third).
      hull(points.fourth).
      hull(points.fifth).
      hull(points.sixth).
      hull(points.seventh)

  def hullOf(
    points: (Point3d, Point3d, Point3d, Point3d, Point3d, Point3d, Point3d, Point3d)
  ): Box3d =
    points.first.
      hull(points.second).
      hull(points.third).
      hull(points.fourth).
      hull(points.fifth).
      hull(points.sixth).
      hull(points.seventh).
      hull(points.eigth)

  val Empty: Box3d = Box3d(Interval.Empty, Interval.Empty, Interval.Empty)

  val Whole: Box3d = Box3d(Interval.Whole, Interval.Whole, Interval.Whole)

  val Unit: Box3d = Box3d(Interval.Unit, Interval.Unit, Interval.Unit)

  implicit val Traits: Bounds[Box3d] = new Bounds[Box3d] {
    override def component(box: Box3d, index: Int): Interval = box.component(index)

    override def overlaps(firstBox: Box3d, secondBox: Box3d, tolerance: Double): Boolean =
      firstBox.overlaps(secondBox, tolerance)

    override def contains(firstBox: Box3d, secondBox: Box3d, tolerance: Double): Boolean =
      firstBox.contains(secondBox, tolerance)

    override def bisected(box: Box3d, index: Int): (Box3d, Box3d) = box.bisected(index)

    override def hull(firstBox: Box3d, secondBox: Box3d): Box3d = firstBox.hull(secondBox)

    override val NumDimensions: Int = 3

    override val Empty: Box3d = Box3d.Empty
  }
}
