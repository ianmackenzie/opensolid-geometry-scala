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

trait VectorBox2dGenerators {
  val randomVectorBox2d: Gen[VectorBox2d] =
    for {
      x <- randomInterval
      y <- randomInterval
    } yield VectorBox2d(x, y)

  implicit val arbitraryVectorBox2d: Arbitrary[VectorBox2d] = Arbitrary(randomVectorBox2d)

  val closedVectorBox2d: Gen[VectorBox2d] =
    for {
      x <- closedInterval
      y <- closedInterval
    } yield VectorBox2d(x, y)
}

object VectorBox2dGenerators extends VectorBox2dGenerators
