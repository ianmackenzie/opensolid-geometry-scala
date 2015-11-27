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
}
