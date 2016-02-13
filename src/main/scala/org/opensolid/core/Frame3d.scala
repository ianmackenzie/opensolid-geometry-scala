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

final case class Frame3d(
  originPoint: Point3d,
  xDirection: Direction3d,
  yDirection: Direction3d,
  zDirection: Direction3d
) extends Transformable3d[Frame3d] {

  def transformedBy(transformation: Transformation3d): Frame3d =
    Frame3d(
      originPoint.transformedBy(transformation),
      xDirection.transformedBy(transformation),
      yDirection.transformedBy(transformation),
      zDirection.transformedBy(transformation)
    )

  def translatedTo(point: Point3d): Frame3d =
    Frame3d(point, xDirection, yDirection, zDirection)

  def xAxis: Axis3d =
    Axis3d(originPoint, xDirection)

  def yAxis: Axis3d =
    Axis3d(originPoint, yDirection)

  def zAxis: Axis3d =
    Axis3d(originPoint, zDirection)

  def xyPlane: Plane3d =
    Plane3d(originPoint, xDirection, yDirection, zDirection)

  def xzPlane: Plane3d =
    Plane3d(originPoint, xDirection, zDirection, -yDirection)

  def yxPlane: Plane3d =
    Plane3d(originPoint, yDirection, xDirection, -zDirection)

  def yzPlane: Plane3d =
    Plane3d(originPoint, yDirection, zDirection, xDirection)

  def zxPlane: Plane3d =
    Plane3d(originPoint, zDirection, xDirection, yDirection)

  def zyPlane: Plane3d =
    Plane3d(originPoint, zDirection, yDirection, -xDirection)
}

object Frame3d {
  def local(originPoint: Point3d): Frame3d =
    Frame3d(originPoint, Direction3d.X, Direction3d.Y, Direction3d.Z)

  def fromXYPlane(plane: Plane3d): Frame3d =
    Frame3d(plane.originPoint, plane.xDirection, plane.yDirection, plane.normalDirection)

  def fromXZPlane(plane: Plane3d): Frame3d =
    Frame3d(plane.originPoint, plane.xDirection, -plane.normalDirection, plane.yDirection)

  def fromYXPlane(plane: Plane3d): Frame3d =
    Frame3d(plane.originPoint, plane.yDirection, plane.xDirection, -plane.normalDirection)

  def fromYZPlane(plane: Plane3d): Frame3d =
    Frame3d(plane.originPoint, plane.normalDirection, plane.xDirection, plane.yDirection)

  def fromZXPlane(plane: Plane3d): Frame3d =
    Frame3d(plane.originPoint, plane.yDirection, plane.normalDirection, plane.xDirection)

  def fromZYPlane(plane: Plane3d): Frame3d =
    Frame3d(plane.originPoint, -plane.normalDirection, plane.yDirection, plane.xDirection)

  val Global: Frame3d = Frame3d(Point3d.Origin, Direction3d.X, Direction3d.Y, Direction3d.Z)
}
