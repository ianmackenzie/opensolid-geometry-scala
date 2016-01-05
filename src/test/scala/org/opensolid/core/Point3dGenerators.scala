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
  val randomPoint3d: Gen[Point3d] =
    for {
      x <- randomDouble
      y <- randomDouble
      z <- randomDouble
    } yield Point3d(x, y, z)

  implicit val arbitraryPoint3d: Arbitrary[Point3d] = Arbitrary(randomPoint3d)

  def pointWithin(box: Box3d): Gen[Point3d] =
    for {
      x <- valueWithin(box.x)
      y <- valueWithin(box.y)
      z <- valueWithin(box.z)
    } yield Point3d(x, y, z)
}

object Point3dGenerators extends Point3dGenerators
