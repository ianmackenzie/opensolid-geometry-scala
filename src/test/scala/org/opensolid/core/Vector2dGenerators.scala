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

trait Vector2dGenerators {
  val randomVector2d: Gen[Vector2d] =
    for {
      x <- randomDouble
      y <- randomDouble
    } yield Vector2d(x, y)

  implicit val arbitraryVector2d: Arbitrary[Vector2d] = Arbitrary(randomVector2d)

  def vectorWithin(vectorBox: VectorBox2d): Gen[Vector2d] =
    for {
      x <- valueWithin(vectorBox.x)
      y <- valueWithin(vectorBox.y)
    } yield Vector2d(x, y)
}

object Vector2dGenerators extends Vector2dGenerators
