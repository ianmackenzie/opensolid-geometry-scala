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

final case class Localization2d(frame: Frame2d) extends Transformation2d {
  def apply(point: Point2d): Point2d = {
    val displacement = frame.originPoint.vectorTo(point)
    Point2d(
      displacement.componentIn(frame.xDirection),
      displacement.componentIn(frame.yDirection)
    )
  }

  def apply(vector: Vector2d): Vector2d =
    Vector2d(
      vector.componentIn(frame.xDirection),
      vector.componentIn(frame.yDirection)
    )

  def apply(direction: Direction2d): Direction2d = {
    Direction2d(
      direction.componentIn(frame.xDirection),
      direction.componentIn(frame.yDirection)
    )
  }
}
