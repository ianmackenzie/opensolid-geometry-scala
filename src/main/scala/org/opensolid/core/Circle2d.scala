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

final case class Circle2d(centerPoint: Point2d, radius: Double)
  extends Scalable2d[Circle2d] with Bounded[Box2d] with GeometricallyComparable[Circle2d] {

  def this(radius: Double, firstPoint: Point2d, secondPoint: Point2d, direction: Handedness) =
    this(numerics.centerPoint(radius, firstPoint, secondPoint, direction), radius)

  private[this] def this(that: Circle2d) = this(that.centerPoint, that.radius)

  def this(firstPoint: Point2d, secondPoint: Point2d, thirdPoint: Point2d) =
    this(numerics.circleThroughPoints(firstPoint, secondPoint, thirdPoint))

  override def transformedBy(transformation: Transformation2d): Circle2d =
    Circle2d(centerPoint.transformedBy(transformation), radius)

  override def scaledAbout(point: Point2d, scale: Double): Circle2d =
    Circle2d(centerPoint.scaledAbout(point, scale), radius * scale)

  override def bounds: Box2d =
    Box2d(
      Interval(centerPoint.x - radius, centerPoint.x + radius),
      Interval(centerPoint.y - radius, centerPoint.y + radius)
    )

  override def isEqualTo(that: Circle2d, tolerance: Double): Boolean = {
    val centerDistance = this.centerPoint.distanceTo(that.centerPoint)
    val radiusDifference = (this.radius - that.radius).abs
    (centerDistance + radiusDifference).isZero(tolerance)
  }
}
