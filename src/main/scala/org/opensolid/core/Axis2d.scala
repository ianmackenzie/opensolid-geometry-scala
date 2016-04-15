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

final case class Axis2d(originPoint: Point2d, direction: Direction2d)
  extends Transformable2d[Axis2d] {

  def normalDirection: Direction2d =
    direction.normalDirection

  def normalAxis: Axis2d =
    Axis2d(originPoint, normalDirection)

  def reversed: Axis2d =
    Axis2d(originPoint, -direction)

  def transformedBy(transformation: Transformation2d): Axis2d =
    Axis2d(originPoint.transformedBy(transformation), direction.transformedBy(transformation))

  def translatedTo(point: Point2d): Axis2d =
    Axis2d(point, direction)

  def placedOnto(plane: Plane3d): Axis3d =
    Axis3d(originPoint.placedOnto(plane), direction.placedOnto(plane))

  def signedDistanceTo(point: Point2d): Double =
    originPoint.vectorTo(point).componentIn(normalDirection)

  def signedDistanceTo[P](expression: PointExpression2d[P]): Expression1d[P] =
    PointExpression2d.Constant[P](originPoint).vectorTo(expression).componentIn(normalDirection)
}

object Axis2d {
  val X = Axis2d(Point2d.Origin, Direction2d.X)

  val Y = Axis2d(Point2d.Origin, Direction2d.Y)
}
