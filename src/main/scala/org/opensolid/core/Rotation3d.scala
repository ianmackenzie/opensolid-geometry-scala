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

final case class Rotation3d(point: Point3d, basis: (Direction3d, Direction3d, Direction3d))
  extends Transformation3d {

  def this(axis: Axis3d, angle: Double) =
    this(axis.originPoint, numerics.rotationBasis(axis.direction, angle))

  def apply(handedness: Handedness): Handedness = handedness

  def apply(point: Point3d): Point3d = this.point + apply(point - this.point)

  def apply(vector: Vector3d): Vector3d = {
    val (xDirection, yDirection, zDirection) = basis
    vector.x * xDirection + vector.y * yDirection + vector.z * zDirection
  }

  def apply(direction: Direction3d): Direction3d = Direction3d(apply(direction.vector))
}

object Rotation3d {
  def apply(axis: Axis3d, angle: Double): Rotation3d = new Rotation3d(axis, angle)

  def aboutX(point: Point3d, angle: Double): Rotation3d = {
    val sinAngle = math.sin(angle)
    val cosAngle = math.cos(angle)
    val yDirection = Direction3d(0.0, cosAngle, sinAngle)
    val zDirection = Direction3d(0.0, -sinAngle, cosAngle)
    Rotation3d(point, (Direction3d.X, yDirection, zDirection))
  }

  def aboutY(point: Point3d, angle: Double): Rotation3d = {
    val sinAngle = math.sin(angle)
    val cosAngle = math.cos(angle)
    val xDirection = Direction3d(cosAngle, 0.0, -sinAngle)
    val zDirection = Direction3d(sinAngle, 0.0, cosAngle)
    Rotation3d(point, (xDirection, Direction3d.Y, zDirection))
  }

  def aboutZ(point: Point3d, angle: Double): Rotation3d = {
    val sinAngle = math.sin(angle)
    val cosAngle = math.cos(angle)
    val xDirection = Direction3d(cosAngle, sinAngle, 0.0)
    val yDirection = Direction3d(-sinAngle, cosAngle, 0.0)
    Rotation3d(point, (xDirection, yDirection, Direction3d.Z))
  }
}
