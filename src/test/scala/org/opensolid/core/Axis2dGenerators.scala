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

import org.opensolid.core.Direction2dGenerators._
import org.opensolid.core.Point2dGenerators._
import org.scalacheck._

trait Axis2dGenerators {
  val randomAxis2d: Gen[Axis2d] =
    for {
      originPoint <- randomPoint2d
      direction <- randomDirection2d
    } yield Axis2d(originPoint, direction)

  implicit val arbitraryAxis2d: Arbitrary[Axis2d] = Arbitrary(randomAxis2d)
}

object Axis2dGenerators extends Axis2dGenerators
