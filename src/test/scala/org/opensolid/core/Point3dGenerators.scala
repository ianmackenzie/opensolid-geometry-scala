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

import org.opensolid.core.DoubleGenerators._
import org.scalacheck._

trait Point3dGenerators {
  val anyPoint3d: Gen[Point3d] =
    for {
      x <- finiteDouble
      y <- finiteDouble
      z <- finiteDouble
    } yield Point3d(x, y, z)

  implicit val arbitraryPoint3d: Arbitrary[Point3d] = Arbitrary(anyPoint3d)

  def pointWithin(bounds: Bounds3d): Gen[Point3d] =
    for {
      x <- valueWithin(bounds.x)
      y <- valueWithin(bounds.y)
      z <- valueWithin(bounds.z)
    } yield Point3d(x, y, z)
}

object Point3dGenerators extends Point3dGenerators
