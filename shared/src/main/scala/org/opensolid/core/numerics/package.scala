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
    point0: Point3d,
    point1: Point3d,
    point2: Point3d,
    handedness: Handedness
  ): Direction3d = {
    val vector1 = point1 - point0
    val vector2 = point2 - point0
    val crossProduct = handedness.sign * vector1.cross(vector2)
    val squaredLength1 = vector1.squaredLength
    val squaredLength2 = vector2.squaredLength
    val crossProductSquaredLength = crossProduct.squaredLength
    if (crossProductSquaredLength > math.ulp(squaredLength1 * squaredLength2)) {
      // Cross product is (reasonably) well conditioned - use it to compute the
      // normal direction
      Direction3d(crossProduct / math.sqrt(crossProductSquaredLength))
    } else {
      // Cross product is poorly conditioned (i.e., triangle is degenerate or
      // nearly so) - instead of using the cross product, compute a unit vector
      // perpendicular to the longest of the two edges
      if (squaredLength1 >= squaredLength2) vector1.normalDirection else vector2.normalDirection
    }
  }
}
