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

trait VectorBounds2dGenerators {
  val randomVectorBounds2d: Gen[VectorBounds2d] =
    for {
      x <- randomInterval
      y <- randomInterval
    } yield VectorBounds2d(x, y)

  implicit val arbitraryVectorBounds2d: Arbitrary[VectorBounds2d] = Arbitrary(randomVectorBounds2d)

  val closedVectorBounds2d: Gen[VectorBounds2d] =
    for {
      x <- closedInterval
      y <- closedInterval
    } yield VectorBounds2d(x, y)
}

object VectorBounds2dGenerators extends VectorBounds2dGenerators
