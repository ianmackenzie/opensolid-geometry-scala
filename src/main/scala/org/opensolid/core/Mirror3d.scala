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
  override def apply(point: Point3d): Point3d =
    point - 2 * (point - plane.originPoint).dot(plane.normalDirection) * plane.normalDirection

  override def apply(vector: Vector3d): Vector3d =
    vector - 2 * vector.dot(plane.normalDirection) * plane.normalDirection
}
