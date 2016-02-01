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

trait Point2dGenerators {
  val randomPoint2d: Gen[Point2d] =
    for {
      x <- randomDouble
      y <- randomDouble
    } yield Point2d(x, y)

  implicit val arbitraryPoint2d: Arbitrary[Point2d] = Arbitrary(randomPoint2d)

  def pointWithin(bounds: Bounds2d): Gen[Point2d] =
    for {
      x <- valueWithin(bounds.x)
      y <- valueWithin(bounds.y)
    } yield Point2d(x, y)
}

object Point2dGenerators extends Point2dGenerators
