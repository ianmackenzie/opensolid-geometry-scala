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

final case class Rotation2d(point: Point2d, angle: Double) extends Transformation2d {
  private[this] val sinAngle = math.sin(angle)
  private[this] val cosAngle = math.cos(angle)

  def apply(length: Double): Double = length

  def apply(handedness: Handedness): Handedness = handedness

  def apply(point: Point2d): Point2d = this.point + apply(point - this.point)

  def apply(vector: Vector2d): Vector2d =
    Vector2d(cosAngle * vector.x - sinAngle * vector.y, sinAngle * vector.x + cosAngle * vector.y)

  def apply(direction: Direction2d): Direction2d = Direction2d(apply(direction.vector))
}
