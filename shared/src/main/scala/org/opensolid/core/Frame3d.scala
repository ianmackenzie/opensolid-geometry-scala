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

case class Frame3d(
  originPoint: Point3d,
  basis: (Direction3d, Direction3d, Direction3d),
  handedness: Handedness
) extends Transformable3d[Frame3d] {

  require(handedness.sign == Sign.of(basis._1.cross(basis._2).dot(basis._3)))

  def transformedBy(transformation: Transformation3d): Frame3d =
    Frame3d(
      originPoint.transformedBy(transformation),
      (
        xDirection.transformedBy(transformation),
        yDirection.transformedBy(transformation),
        zDirection.transformedBy(transformation)
      ),
      handedness.transformedBy(transformation)
    )

  def xDirection: Direction3d = basis._1

  def yDirection: Direction3d = basis._2

  def zDirection: Direction3d = basis._3

  def xAxis: Axis3d = Axis3d(originPoint, xDirection)

  def yAxis: Axis3d = Axis3d(originPoint, yDirection)

  def zAxis: Axis3d = Axis3d(originPoint, zDirection)

  def xyPlane: Plane3d = Plane3d(originPoint, (xDirection, yDirection), zDirection, handedness)

  def xzPlane: Plane3d = Plane3d(originPoint, (xDirection, zDirection), -yDirection, handedness)

  def yxPlane: Plane3d = Plane3d(originPoint, (yDirection, xDirection), -zDirection, handedness)

  def yzPlane: Plane3d = Plane3d(originPoint, (yDirection, zDirection), xDirection, handedness)

  def zxPlane: Plane3d = Plane3d(originPoint, (zDirection, xDirection), yDirection, handedness)

  def zyPlane: Plane3d = Plane3d(originPoint, (zDirection, yDirection), -xDirection, handedness)
}

object Frame3d {
  def apply(originPoint: Point3d): Frame3d =
    Frame3d(originPoint, (Direction3d.X, Direction3d.Y, Direction3d.Z), Handedness.Right)

  def apply(originPoint: Point3d, basis: (Direction3d, Direction3d, Direction3d)): Frame3d =
    Frame3d(originPoint, basis, Handedness.fromSignOf(basis._1.cross(basis._2).dot(basis._3)))

  @BeanProperty
  val Global: Frame3d =
    Frame3d(Point3d.Origin, (Direction3d.X, Direction3d.Y, Direction3d.Z), Handedness.Right)
}
