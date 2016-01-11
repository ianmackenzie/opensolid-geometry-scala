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

import scala.math

final case class Point2d(x: Double, y: Double)
  extends Scalable2d[Point2d] with Bounded[Box2d] with GeometricallyComparable[Point2d] {

  def components: (Double, Double) = (x, y)

  def component(index: Int): Double = index match {
    case 0 => x
    case 1 => y
    case _ => throw new IndexOutOfBoundsException(s"Index $index is out of bounds for Point2d")
  }

  override def bounds: Box2d = Box2d(Interval(x), Interval(y))

  override def equals(that: Point2d, tolerance: Double): Boolean =
    this.squaredDistanceTo(that).isZero(tolerance * tolerance)

  def squaredDistanceTo(that: Point2d): Double = (this - that).squaredLength

  def distanceTo(that: Point2d): Double = (this - that).length

  def isOrigin(tolerance: Double): Boolean = x * x + y * y <= tolerance * tolerance

  def distanceAlong(axis: Axis2d): Double = (this - axis.originPoint).dot(axis.direction)

  def distanceTo(axis: Axis2d): Double = (this - axis.originPoint).dot(axis.normalDirection)

  def isOn(axis: Axis2d, tolerance: Double): Boolean = distanceTo(axis).isZero(tolerance)

  override def transformedBy(transformation: Transformation2d): Point2d = transformation(this)

  override def scaledAbout(point: Point2d, scale: Double): Point2d = point + scale * (this - point)

  def projectedOnto(axis: Axis2d): Point2d = axis.originPoint + distanceAlong(axis) * axis.direction

  def placedOnto(plane: Plane3d): Point3d =
    plane.originPoint + x * plane.xDirection + y * plane.yDirection

  def hull(that: Point2d): Box2d = Box2d(this.x.hull(that.x), this.y.hull(that.y))

  def hull(box: Box2d): Box2d = Box2d(x.hull(box.x), y.hull(box.y))

  def +(vector: Vector2d): Point2d = Point2d(x + vector.x, y + vector.y)

  def +(vectorBox: VectorBox2d): Box2d = Box2d(x + vectorBox.x, y + vectorBox.y)

  def -(vector: Vector2d): Point2d = Point2d(x - vector.x, y - vector.y)

  def -(vectorBox: VectorBox2d): Box2d = Box2d(x - vectorBox.x, y - vectorBox.y)

  def -(that: Point2d): Vector2d = Vector2d(x - that.x, y - that.y)

  def -(box: Box2d): VectorBox2d = VectorBox2d(x - box.x, y - box.y)
}

object Point2d {
  def polar(radius: Double, angle: Double): Point2d =
    Point2d(radius * math.cos(angle), radius * math.sin(angle))

  val Origin: Point2d = Point2d(0.0, 0.0)
}
