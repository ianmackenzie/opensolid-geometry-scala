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

import org.opensolid.core.IntervalGenerators._
import org.scalacheck._

trait VectorBox3dGenerators {
  val randomVectorBox3d: Gen[VectorBox3d] =
    for {
      x <- randomInterval
      y <- randomInterval
      z <- randomInterval
    } yield VectorBox3d(x, y, z)

  implicit val arbitraryVectorBox3d: Arbitrary[VectorBox3d] = Arbitrary(randomVectorBox3d)

  val closedVectorBox3d: Gen[VectorBox3d] =
    for {
      x <- closedInterval
      y <- closedInterval
      z <- closedInterval
    } yield VectorBox3d(x, y, z)
}

object VectorBox3dGenerators extends VectorBox3dGenerators
