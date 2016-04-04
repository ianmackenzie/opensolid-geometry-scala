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
  private[this] val (
    Direction3d(a00, a10, a20),
    Direction3d(a01, a11, a21),
    Direction3d(a02, a12, a22)
  ) = Numerics.rotationBasis(axis.direction, angle)

  override def apply(point: Point3d): Point3d =
    axis.originPoint + apply(axis.originPoint.vectorTo(point))

  override def apply(vector: Vector3d): Vector3d =
    Vector3d(
      a00 * vector.x + a01 * vector.y + a02 * vector.z,
      a10 * vector.x + a11 * vector.y + a12 * vector.z,
      a20 * vector.x + a21 * vector.y + a22 * vector.z
    )

  override def apply(direction: Direction3d): Direction3d =
    Direction3d(
      a00 * direction.x + a01 * direction.y + a02 * direction.z,
      a10 * direction.x + a11 * direction.y + a12 * direction.z,
      a20 * direction.x + a21 * direction.y + a22 * direction.z
    )
}
