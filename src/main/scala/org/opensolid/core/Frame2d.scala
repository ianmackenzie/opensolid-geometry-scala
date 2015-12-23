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

case class Frame2d(originPoint: Point2d, xDirection: Direction2d, yDirection: Direction2d)
  extends Transformable2d[Frame2d] {

  def transformedBy(transformation: Transformation2d): Frame2d =
    Frame2d(
      originPoint.transformedBy(transformation),
      xDirection.transformedBy(transformation),
      yDirection.transformedBy(transformation)
    )

  def xAxis: Axis2d = Axis2d(originPoint, xDirection)

  def yAxis: Axis2d = Axis2d(originPoint, yDirection)

  def handedness: Handedness =
    Handedness.fromSignOf(xDirection.x * yDirection.y - xDirection.y * yDirection.x)

  def placedOnto(plane: Plane3d): Plane3d =
    Plane3d(
      originPoint.placedOnto(plane),
      xDirection.placedOnto(plane),
      yDirection.placedOnto(plane)
    )
}

object Frame2d {
  def apply(originPoint: Point2d): Frame2d = Frame2d(originPoint, Direction2d.X, Direction2d.Y)

  def apply(originPoint: Point2d, xDirection: Direction2d): Frame2d =
    Frame2d(originPoint, xDirection, xDirection.normalDirection)

  @BeanProperty
  val Global: Frame2d = Frame2d(Point2d.Origin, Direction2d.X, Direction2d.Y)
}
