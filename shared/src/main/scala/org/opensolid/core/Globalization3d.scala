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

case class Globalization3d(frame: Frame3d) extends Transformation3d {
  def apply(length: Double): Double = length

  def apply(handedness: Handedness): Handedness = frame.handedness * handedness

  def apply(point: Point3d): Point3d =
    frame.originPoint +
    point.x * frame.xDirection +
    point.y * frame.yDirection +
    point.z * frame.zDirection

  def apply(vector: Vector3d): Vector3d =
    vector.x * frame.xDirection + vector.y * frame.yDirection + vector.z * frame.zDirection

  def apply(direction: Direction3d): Direction3d = Direction3d(apply(direction.vector))
}
