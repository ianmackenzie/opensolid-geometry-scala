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

package object numerics {
  def normalDirectionFromThreePoints(
    firstPoint: Point3d,
    secondPoint: Point3d,
    thirdPoint: Point3d
  ): Direction3d = {
    val firstVector = secondPoint - firstPoint
    val secondVector = thirdPoint - firstPoint
    val crossProduct = firstVector.cross(secondVector)
    val firstSquaredLength = firstVector.squaredLength
    val secondSquaredLength = secondVector.squaredLength
    val crossProductSquaredLength = crossProduct.squaredLength
    if (crossProductSquaredLength > math.ulp(firstSquaredLength * secondSquaredLength)) {
      // Cross product is (reasonably) well conditioned - use it to compute the
      // normal direction
      Direction3d(crossProduct / math.sqrt(crossProductSquaredLength))
    } else {
      // Cross product is poorly conditioned (i.e., triangle is degenerate or
      // nearly so) - instead of using the cross product, compute a unit vector
      // perpendicular to the longest of the two edges
      if (firstSquaredLength >= secondSquaredLength) {
        firstVector.normalDirection
      } else {
        secondVector.normalDirection
      }
    }
  }

  def rotationBasis(
    direction: Direction3d,
    angle: Double
  ): (Direction3d, Direction3d, Direction3d) = {
    val halfAngle = 0.5 * angle
    val sinHalfAngle = math.sin(halfAngle)
    val x = direction.x * sinHalfAngle
    val y = direction.y * sinHalfAngle
    val z = direction.z * sinHalfAngle
    val w = math.cos(halfAngle)
    val wx = w * x
    val wy = w * y
    val wz = w * z
    val xx = x * x
    val xy = x * y
    val xz = x * z
    val yy = y * y
    val yz = y * z
    val zz = z * z
    val xDirection = Direction3d(1 - 2 * (yy + zz), 2 * (xy + wz), 2 * (xz - wy))
    val yDirection = Direction3d(2 * (xy - wz), 1 - 2 * (xx + zz), 2 * (yz + wx))
    val zDirection = Direction3d(2 * (xz + wy), 2 * (yz - wx), 1 - 2 * (xx + yy))
    (xDirection, yDirection, zDirection)
  }

  def normalBasis(direction: Direction3d): (Direction3d, Direction3d) = {
    val xDirection = direction.normalDirection
    val yDirection = Direction3d(direction.cross(xDirection))
    (xDirection, yDirection)
  }
}
