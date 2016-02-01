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

trait Vector3dGenerators {
  val randomVector3d: Gen[Vector3d] =
    for {
      x <- randomDouble
      y <- randomDouble
      z <- randomDouble
    } yield Vector3d(x, y, z)

  implicit val arbitraryVector3d: Arbitrary[Vector3d] = Arbitrary(randomVector3d)

  def vectorWithin(vectorBounds: VectorBounds3d): Gen[Vector3d] =
    for {
      x <- valueWithin(vectorBounds.x)
      y <- valueWithin(vectorBounds.y)
      z <- valueWithin(vectorBounds.z)
    } yield Vector3d(x, y, z)
}

object Vector3dGenerators extends Vector3dGenerators
