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

final case class Mirror3d(plane: Plane3d) extends Transformation3d {
  private[this] val originPoint = plane.originPoint
  private[this] val normalDirection = plane.normalDirection

  override def apply(point: Point3d): Point3d =
    point - 2 * originPoint.vectorTo(point).projectedOnto(normalDirection)

  override def apply(vector: Vector3d): Vector3d =
    vector - 2 * vector.projectedOnto(normalDirection)

  override def apply(direction: Direction3d): Direction3d = {
    val dotProduct =
      direction.x * normalDirection.x +
      direction.y * normalDirection.y +
      direction.z * normalDirection.z
    Direction3d(
      direction.x - 2 * dotProduct * normalDirection.x,
      direction.y - 2 * dotProduct * normalDirection.y,
      direction.z - 2 * dotProduct * normalDirection.z
    )
  }
}
