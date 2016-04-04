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
import org.opensolid.core.Vector3dGenerators._
import org.scalacheck._

trait Direction3dGenerators {
  private[this] val randomDirection3d: Gen[Direction3d] = {
    val vectorGenerator =
      vectorWithin(VectorBounds3d(Interval(-1.0, 1.0), Interval(-1.0, 1.0), Interval(-1.0, 1.0)))
    val radiusPredicate = (vector: Vector3d) => Interval(0.25, 1.0).contains(vector.squaredLength)
    vectorGenerator.retryUntil(radiusPredicate).map(_.direction)
  }

  val anyDirection3d: Gen[Direction3d] =
    Gen.frequency(
      1 -> Direction3d.X,
      1 -> Direction3d.Y,
      1 -> Direction3d.Z,
      1 -> -Direction3d.X,
      1 -> -Direction3d.Y,
      1 -> -Direction3d.Z,
      8 -> randomDirection3d
    )

  implicit val arbitraryDirection3d: Arbitrary[Direction3d] = Arbitrary(anyDirection3d)
}

object Direction3dGenerators extends Direction3dGenerators
