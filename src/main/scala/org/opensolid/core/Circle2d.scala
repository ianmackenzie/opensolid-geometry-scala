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

final case class Circle2d(centerPoint: Point2d, radius: Double) extends Scalable2d[Circle2d]
  with Bounded[Bounds2d]
  with GeometricallyComparable[Circle2d] {

  require(radius >= 0.0)

  override def transformedBy(transformation: Transformation2d): Circle2d =
    Circle2d(centerPoint.transformedBy(transformation), radius)

  override def scaledAbout(point: Point2d, scale: Double): Circle2d =
    Circle2d(centerPoint.scaledAbout(point, scale), radius * scale)

  def translatedTo(point: Point2d): Circle2d =
    Circle2d(point, radius)

  override def bounds: Bounds2d =
    Bounds2d(
      Interval(centerPoint.x - radius, centerPoint.x + radius),
      Interval(centerPoint.y - radius, centerPoint.y + radius)
    )

  override def isEqualTo(that: Circle2d, tolerance: Double): Boolean = {
    val centerDistance = this.centerPoint.distanceTo(that.centerPoint)
    val radiusDifference = (this.radius - that.radius).abs
    (centerDistance + radiusDifference).isZero(tolerance)
  }

  def contains(point: Point2d): Boolean =
    point.squaredDistanceTo(centerPoint) <= radius * radius

  def projectedOnto(axis: Axis2d): LineSegment2d = {
    val projectedCenter = centerPoint.projectedOnto(axis)
    val offset = radius * axis.direction
    LineSegment2d(projectedCenter - offset, projectedCenter + offset)
  }

  def placedOnto(plane: Plane3d): Circle3d =
    Circle3d(centerPoint.placedOnto(plane), plane.normalDirection, radius)
}

object Circle2d {
  def throughTwoPoints(firstPoint: Point2d, secondPoint: Point2d, radius: Double): Circle2d = {
    val displacementVector = firstPoint.vectorTo(secondPoint)
    val halfDistance = displacementVector.length / 2.0
    val sidewaysDistance = math.sqrt((halfDistance * halfDistance - radius * radius).max(0.0))
    val sidewaysDirection = displacementVector.normalDirection
    val centerPoint = firstPoint + displacementVector / 2.0 + sidewaysDistance * sidewaysDirection
    Circle2d(centerPoint, radius)
  }

  def throughThreePoints(
    firstPoint: Point2d,
    secondPoint: Point2d,
    thirdPoint: Point2d
  ): Circle2d = {
    val u = firstPoint.vectorTo(secondPoint)
    val v = firstPoint.vectorTo(thirdPoint)
    val a = u.length
    val b = secondPoint.distanceTo(thirdPoint)
    val c = v.length
    val a2 = a * a
    val b2 = b * b
    val c2 = c * c
    val t1 = a2 * (b2 + c2 - a2)
    val t2 = b2 * (c2 + a2 - b2)
    val t3 = c2 * (a2 + b2 - c2)
    val sum = t1 + t2 + t3
    if (sum <= 0.0) throw GeometricException("Points are collinear")
    val sumInverse = 1.0 / sum
    val centerPoint = firstPoint + (t1 * sumInverse) * v + (t3 * sumInverse) * u
    val firstRadius = centerPoint.distanceTo(firstPoint)
    val secondRadius = centerPoint.distanceTo(secondPoint)
    val thirdRadius = centerPoint.distanceTo(thirdPoint)
    val radius = (firstRadius + secondRadius + thirdRadius) / 3.0;
    Circle2d(centerPoint, radius)
  }

  def circumcircle(triangle: Triangle2d): Circle2d =
    Circle2d.throughThreePoints(triangle.firstVertex, triangle.secondVertex, triangle.thirdVertex)

  val Unit: Circle2d = Circle2d(Point2d.Origin, 1.0)
}
