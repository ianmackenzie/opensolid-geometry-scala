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

trait VectorBounds3dGenerators {
  val randomVectorBounds3d: Gen[VectorBounds3d] =
    for {
      x <- randomInterval
      y <- randomInterval
      z <- randomInterval
    } yield VectorBounds3d(x, y, z)

  implicit val arbitraryVectorBounds3d: Arbitrary[VectorBounds3d] = Arbitrary(randomVectorBounds3d)

  val closedVectorBounds3d: Gen[VectorBounds3d] =
    for {
      x <- closedInterval
      y <- closedInterval
      z <- closedInterval
    } yield VectorBounds3d(x, y, z)
}

object VectorBounds3dGenerators extends VectorBounds3dGenerators
