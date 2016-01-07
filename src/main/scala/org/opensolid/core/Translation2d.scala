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

final case class Translation2d(vector: Vector2d) extends Transformation2d {
  def this(x: Double, y: Double) = this(Vector2d(x, y))

  def this(axis: Axis2d, distance: Double) = this(distance * axis.direction)

  override def apply(point: Point2d): Point2d = point + vector

  override def apply(vector: Vector2d): Vector2d = vector
}

object Translation2d {
  def apply(x: Double, y: Double): Translation2d = new Translation2d(x, y)

  def apply(axis: Axis2d, distance: Double): Translation2d = new Translation2d(axis, distance)
}
