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

final case class Rotation3d(axis: Axis3d, angle: Double) extends Transformation3d {
  private[this] val (xDirection, yDirection, zDirection) =
    Numerics.rotationBasis(axis.direction, angle)

  override def apply(point: Point3d): Point3d =
    axis.originPoint + apply(point - axis.originPoint)

  override def apply(vector: Vector3d): Vector3d =
    vector.x * xDirection + vector.y * yDirection + vector.z * zDirection

  override def apply(direction: Direction3d): Direction3d =
    Direction3d(
      direction.x * xDirection.x + direction.y * yDirection.x + direction.z * zDirection.x,
      direction.x * xDirection.y + direction.y * yDirection.y + direction.z * zDirection.y,
      direction.x * xDirection.z + direction.y * yDirection.z + direction.z * zDirection.z
    )
}
