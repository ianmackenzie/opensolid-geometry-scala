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

import scala.util.Random

import org.opensolid.core.DoubleGenerators._
import org.scalacheck._

trait Direction2dGenerators {
  val randomDirection2d: Gen[Direction2d] =
    for {
      seed <- Gen.choose(Int.MinValue, Int.MaxValue)
    } yield Direction2d.random(new Random(seed))

  implicit val arbitraryVector2d: Arbitrary[Direction2d] =
    Arbitrary(
      Gen.frequency(
        1 -> Direction2d.X,
        1 -> Direction2d.Y,
        1 -> -Direction2d.X,
        1 -> -Direction2d.Y,
        4 -> randomDirection2d
      )
    )
}

object Direction2dGenerators extends Direction2dGenerators
