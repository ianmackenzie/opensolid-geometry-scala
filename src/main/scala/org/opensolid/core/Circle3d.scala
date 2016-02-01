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

final case class Circle3d(centerPoint: Point3d, normalDirection: Direction3d, radius: Double)
  extends Scalable3d[Circle3d] with Bounded[Bounds3d] {

  require(radius >= 0.0)

  override def transformedBy(transformation: Transformation3d): Circle3d =
    Circle3d(
      centerPoint.transformedBy(transformation),
      normalDirection.transformedBy(transformation),
      radius
    )

  override def scaledAbout(point: Point3d, scale: Double): Circle3d =
    Circle3d(centerPoint.scaledAbout(point, scale), normalDirection, radius * scale)

  override def bounds: Bounds3d = {
    val nx2 = normalDirection.x * normalDirection.x
    val ny2 = normalDirection.y * normalDirection.y
    val nz2 = normalDirection.z * normalDirection.z
    val dx = radius * math.sqrt(ny2 + nz2)
    val dy = radius * math.sqrt(nx2 + nz2)
    val dz = radius * math.sqrt(nx2 + ny2)
    Bounds3d(
      Interval(centerPoint.x - dx, centerPoint.x + dx),
      Interval(centerPoint.y - dy, centerPoint.y + dy),
      Interval(centerPoint.z - dz, centerPoint.z + dz)
    )
  }

  def axis: Axis3d = Axis3d(centerPoint, normalDirection)

  def plane: Plane3d = Plane3d(centerPoint, normalDirection)
}

object Circle3d {
  def throughTwoPoints(
    firstPoint: Point3d,
    secondPoint: Point3d,
    normalDirection: Direction3d,
    radius: Double
  ): Circle3d = {
    val plane = Plane3d(firstPoint + 0.5 * (secondPoint - firstPoint), normalDirection)
    Circle2d.throughTwoPoints(
      firstPoint.projectedInto(plane),
      secondPoint.projectedInto(plane),
      radius
    ).placedOnto(plane)
  }

  def throughTwoPoints(
    points: (Point3d, Point3d),
    normalDirection: Direction3d,
    radius: Double
  ): Circle3d = Circle3d.throughTwoPoints(points.first, points.second, normalDirection, radius)

  def throughThreePoints(
    firstPoint: Point3d,
    secondPoint: Point3d,
    thirdPoint: Point3d
  ): Circle3d = Circle3d.circumcircle(Triangle3d(firstPoint, secondPoint, thirdPoint))

  def throughThreePoints(points: (Point3d, Point3d, Point3d)): Circle3d =
    Circle3d.throughThreePoints(points.first, points.second, points.third)

  def circumcircle(triangle: Triangle3d): Circle3d = {
    val plane = triangle.plane
    Circle2d.circumcircle(triangle.projectedInto(plane)).placedOnto(plane)
  }
}
