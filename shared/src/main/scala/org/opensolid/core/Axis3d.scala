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

case class Axis3d(originPoint: Point3d, direction: Direction3d)
  extends Transformable3d[Axis3d] with Scalable3d[Axis3d] {

  def pointAt(distance: Double): Point3d = originPoint + distance * direction

  def reversed: Axis3d = Axis3d(originPoint, -direction)

  def transformedBy(transformation: Transformation3d): Axis3d =
    Axis3d(originPoint.transformedBy(transformation), direction.transformedBy(transformation))

  def scaledAbout(point: Point3d, scale: Double): Axis3d = {
    require(scale > 0.0)
    Axis3d(originPoint.scaledAbout(point, scale), direction)
  }
}

object Axis3d {
  val X = Axis3d(Point3d.Origin, Direction3d.X)
  val Y = Axis3d(Point3d.Origin, Direction3d.Y)
  val Z = Axis3d(Point3d.Origin, Direction3d.Z)
}
