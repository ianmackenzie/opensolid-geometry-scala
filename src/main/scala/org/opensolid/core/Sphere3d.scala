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

final case class Sphere3d(centerPoint: Point3d, radius: Double)
  extends Scalable3d[Sphere3d] with Bounded[Box3d] with GeometricallyComparable[Sphere3d] {

  require(radius >= 0.0)

  override def transformedBy(transformation: Transformation3d): Sphere3d =
    Sphere3d(centerPoint.transformedBy(transformation), radius)

  override def scaledAbout(point: Point3d, scale: Double): Sphere3d =
    Sphere3d(centerPoint.scaledAbout(point, scale), radius * scale)

  override def bounds: Box3d =
    Box3d(
      Interval(centerPoint.x - radius, centerPoint.x + radius),
      Interval(centerPoint.y - radius, centerPoint.y + radius),
      Interval(centerPoint.z - radius, centerPoint.z + radius)
    )

  override def equals(that: Sphere3d, tolerance: Double): Boolean = {
    val centerDistance = this.centerPoint.distanceTo(that.centerPoint)
    val radiusDifference = (this.radius - that.radius).abs
    (centerDistance + radiusDifference).isZero(tolerance)
  }

  def contains(point: Point3d): Boolean = point.squaredDistanceTo(centerPoint) <= radius * radius

  def projectedOnto(plane: Plane3d): Circle3d =
    Circle3d(centerPoint.projectedOnto(plane), plane.normalDirection, radius)
}
