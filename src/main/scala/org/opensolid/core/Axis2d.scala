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

case class Axis2d(originPoint: Point2d, direction: Direction2d) extends Transformable2d[Axis2d] {
  def pointAt(distance: Double): Point2d = originPoint + distance * direction

  def normalDirection: Direction2d = direction.normalDirection

  def normalDirection(handedness: Handedness): Direction2d = direction.normalDirection(handedness)

  def normalAxis: Axis2d = Axis2d(originPoint, direction.normalDirection)

  def normalAxis(handedness: Handedness): Axis2d =
    Axis2d(originPoint, direction.normalDirection(handedness))

  def reversed: Axis2d = Axis2d(originPoint, -direction)

  def transformedBy(transformation: Transformation2d): Axis2d =
    Axis2d(originPoint.transformedBy(transformation), direction.transformedBy(transformation))

  def projectedOnto(that: Axis2d): Axis2d =
    Axis2d(this.originPoint.projectedOnto(that), this.direction.projectedOnto(that).direction)

  def placedOnto(plane: Plane3d): Axis3d =
    Axis3d(originPoint.placedOnto(plane), direction.placedOnto(plane))
}

object Axis2d {
  @BeanProperty
  val X = Axis2d(Point2d.Origin, Direction2d.X)

  @BeanProperty
  val Y = Axis2d(Point2d.Origin, Direction2d.Y)
}
