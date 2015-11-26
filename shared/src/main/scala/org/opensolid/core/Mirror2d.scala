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

case class Mirror2d(point: Point2d, direction: Direction2d) extends Transformation2d {
  def apply(length: Double): Double = length

  def apply(handedness: Handedness): Handedness = -handedness

  def apply(point: Point2d): Point2d = point - 2 * (point - this.point).dot(direction) * direction

  def apply(vector: Vector2d): Vector2d = vector - 2 * vector.dot(direction) * direction

  def apply(direction: Direction2d): Direction2d = Direction2d(apply(direction.vector))
}

object Mirror2d {
  def apply(axis: Axis2d): Mirror2d = Mirror2d(axis.originPoint, axis.normalDirection)
}
