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

case class Frame2d(
  originPoint: Point2d,
  xDirection: Direction2d,
  yDirection: Direction2d,
  handedness: Handedness
) extends Transformable2d[Frame2d] {

  require(handedness == Handedness.fromSignOf(xDirection.normalDirection.dot(yDirection)))

  def transformedBy(transformation: Transformation2d): Frame2d =
    Frame2d(
      originPoint.transformedBy(transformation),
      xDirection.transformedBy(transformation),
      yDirection.transformedBy(transformation),
      handedness.transformedBy(transformation)
    )

  def xAxis: Axis2d = Axis2d(originPoint, xDirection, handedness)

  def yAxis: Axis2d = Axis2d(originPoint, yDirection, handedness)

  def placedOnto(plane: Plane3d): Plane3d =
    Plane3d(
      originPoint.placedOnto(plane),
      xDirection.placedOnto(plane),
      yDirection.placedOnto(plane),
      handedness.sign * plane.normalDirection,
      handedness
    )
}

object Frame2d {
  def apply(originPoint: Point2d): Frame2d =
    Frame2d(originPoint, Direction2d.X, Direction2d.Y, Handedness.Right)

  def apply(originPoint: Point2d, xDirection: Direction2d): Frame2d =
    Frame2d(originPoint, xDirection, xDirection.normalDirection, Handedness.Right)

  def apply(originPoint: Point2d, xDirection: Direction2d, yDirection: Direction2d): Frame2d = {
    val handedness = Handedness.fromSignOf(xDirection.normalDirection.dot(yDirection))
    Frame2d(originPoint, xDirection, yDirection, handedness)
  }

  val Global: Frame2d = Frame2d(Point2d.Origin, Direction2d.X, Direction2d.Y, Handedness.Right)
}
