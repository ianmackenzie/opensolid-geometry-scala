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
import java.lang.Math

object Numerics {
  def normalDirection(
    firstPoint: Point3d,
    secondPoint: Point3d,
    thirdPoint: Point3d
  ): Direction3d = {
    val firstVector = firstPoint.vectorTo(secondPoint)
    val secondVector = firstPoint.vectorTo(thirdPoint)
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

  def binormalToBasis(xDirection: Direction3d, yDirection: Direction3d): Direction3d =
    Direction3d(
      xDirection.y * yDirection.z - xDirection.z * yDirection.y,
      xDirection.z * yDirection.x - xDirection.x * yDirection.z,
      xDirection.x * yDirection.y - xDirection.y * yDirection.x
    )

  def normalBasis(direction: Direction3d): (Direction3d, Direction3d) = {
    val xDirection = direction.normalDirection
    val yDirection = binormalToBasis(direction, xDirection)
    (xDirection, yDirection)
  }

  def newtonRaphson(
    expression: (Double) => Double,
    derivative: (Double) => Double,
    interval: Interval
  ): Option[Double] = {
    val xMin = interval.lowerBound
    val xMax = interval.upperBound
    var x = interval.midpoint
    var y = expression(x)
    var error = y.abs
    var nonImprovementCount = 0
    var xWithinInterval = true
    var root = x
    var minError = error
    while (xWithinInterval && nonImprovementCount < 2) {
      val yPrime = derivative(x)
      val xNew = x - y / yPrime
      if (xMin < xNew && xNew < xMax) {
        val yNew = expression(xNew)
        val errorNew = yNew.abs
        if (errorNew >= error) nonImprovementCount += 1
        x = xNew
        y = yNew
        error = errorNew
        if (error < minError) {
          root = x
          minError = error
        }
      } else {
        xWithinInterval = false
      }
    }
    if (xWithinInterval) Some(root) else None
  }
}
