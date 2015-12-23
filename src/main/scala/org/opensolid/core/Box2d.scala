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

final case class Box2d(x: Interval, y: Interval)
  extends Bounds[Box2d] with GeometricallyComparable[Box2d] {

  def this(components: (Interval, Interval)) = this(components.first, components.second)

  def components: (Interval, Interval) = (x, y)

  def component(index: Int): Interval = index match {
    case 0 => x
    case 1 => y
    case _ =>
      throw new IndexOutOfBoundsException(s"Index $index is out of bounds for Box2d")
  }

  override def bounds: Box2d = this

  override def isEqualTo(that: Box2d, tolerance: Double): Boolean =
    this.minVertex.isEqualTo(that.minVertex, tolerance) &&
    this.maxVertex.isEqualTo(that.maxVertex, tolerance)

  def isEmpty: Boolean = x.isEmpty || y.isEmpty

  def isWhole: Boolean = x.isWhole && y.isWhole

  def isSingleton: Boolean = x.isSingleton && y.isSingleton

  def center: Point2d = Point2d(x.median, y.median)

  def minVertex: Point2d = Point2d(x.lowerBound, y.lowerBound)

  def maxVertex: Point2d = Point2d(x.upperBound, y.upperBound)

  def vertices: (Point2d, Point2d, Point2d, Point2d) = (
    Point2d(x.lowerBound, y.lowerBound),
    Point2d(x.upperBound, y.lowerBound),
    Point2d(x.lowerBound, y.upperBound),
    Point2d(x.upperBound, y.upperBound)
  )

  def interpolated(u: Double, v: Double): Point2d = Point2d(x.interpolated(u), y.interpolated(v))

  def randomPoint: Point2d = randomPoint(Random)

  def randomPoint(generator: Random): Point2d =
    interpolated(generator.nextDouble, generator.nextDouble)

  def hull(point: Point2d): Box2d = Box2d(x.hull(point.x), y.hull(point.y))

  override def hull(that: Box2d): Box2d =
    Box2d(this.x.hull(that.x), this.y.hull(that.y))

  def intersection(that: Box2d): Box2d = {
    val x = this.x.intersection(that.x)
    val y = this.y.intersection(that.y)
    if (x.isEmpty || y.isEmpty) Box2d.Empty else Box2d(x, y)
  }

  def overlaps(that: Box2d): Boolean = this.x.overlaps(that.x) && this.y.overlaps(that.y)

  override def overlaps(that: Box2d, tolerance: Double): Boolean =
    this.x.overlaps(that.x, tolerance) && this.y.overlaps(that.y, tolerance)

  def contains(point: Point2d): Boolean = x.contains(point.x) && y.contains(point.y)

  def contains(point: Point2d, tolerance: Double): Boolean =
    x.contains(point.x, tolerance) && y.contains(point.y, tolerance)

  def contains(that: Box2d): Boolean = this.x.contains(that.x) && this.y.contains(that.y)

  def contains(that: Box2d, tolerance: Double): Boolean =
    this.x.contains(that.x, tolerance) && this.y.contains(that.y, tolerance)

  override def bisected(dimensionIndex: Int): (Box2d, Box2d) = {
    if (dimensionIndex % 2 == 0) {
      val (xLower, xUpper) = x.bisected
      (Box2d(xLower, y), Box2d(xUpper, y))
    } else {
      val (yLower, yUpper) = y.bisected
      (Box2d(x, yLower), Box2d(x, yUpper))
    }
  }

  def +(vector: Vector2d): Box2d = Box2d(x + vector.x, y + vector.y)

  def +(vectorBox: VectorBox2d): Box2d =
    Box2d(x + vectorBox.x, y + vectorBox.y)

  def -(vector: Vector2d): Box2d = Box2d(x - vector.x, y - vector.y)

  def -(vectorBox: VectorBox2d): Box2d =
    Box2d(x - vectorBox.x, y - vectorBox.y)

  def -(point: Point2d): VectorBox2d = VectorBox2d(x - point.x, y - point.y)

  def -(that: Box2d): VectorBox2d =
    VectorBox2d(this.x - that.x, this.y - that.y)
}

object Box2d {
  def apply(components: (Interval, Interval)): Box2d = new Box2d(components)

  def hullOf(points: (Point2d, Point2d)): Box2d = points.first.hull(points.second)

  def hullOf(points: (Point2d, Point2d, Point2d)): Box2d =
    points.first.hull(points.second).hull(points.third)

  def hullOf(points: (Point2d, Point2d, Point2d, Point2d)): Box2d =
    points.first.hull(points.second).hull(points.third).hull(points.fourth)

  def hullOf(points: (Point2d, Point2d, Point2d, Point2d, Point2d)): Box2d =
    points.first.hull(points.second).hull(points.third).hull(points.fourth).hull(points.fifth)

  def hullOf(points: (Point2d, Point2d, Point2d, Point2d, Point2d, Point2d)): Box2d =
    points.first.
      hull(points.second).
      hull(points.third).
      hull(points.fourth).
      hull(points.fifth).
      hull(points.sixth)

  def hullOf(
    points: (Point2d, Point2d, Point2d, Point2d, Point2d, Point2d, Point2d)
  ): Box2d =
    points.first.
      hull(points.second).
      hull(points.third).
      hull(points.fourth).
      hull(points.fifth).
      hull(points.sixth).
      hull(points.seventh)

  def hullOf(
    points: (Point2d, Point2d, Point2d, Point2d, Point2d, Point2d, Point2d, Point2d)
  ): Box2d =
    points.first.
      hull(points.second).
      hull(points.third).
      hull(points.fourth).
      hull(points.fifth).
      hull(points.sixth).
      hull(points.seventh).
      hull(points.eigth)

  @BeanProperty
  val Empty: Box2d = Box2d(Interval.Empty, Interval.Empty)

  @BeanProperty
  val Whole: Box2d = Box2d(Interval.Whole, Interval.Whole)

  @BeanProperty
  val Unit: Box2d = Box2d(Interval.Unit, Interval.Unit)
}
