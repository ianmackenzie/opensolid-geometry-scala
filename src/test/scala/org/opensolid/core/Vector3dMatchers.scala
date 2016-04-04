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

import scala.math

import org.opensolid.core.DoubleMatchers._
import org.scalatest._
import org.scalatest.matchers._

trait Vector3dMatchers {
  def beEqualTo(expected: Vector3d, tolerance: Double): Matcher[Vector3d] =
    new ApproximatelyEqualMatcher[Vector3d](
      expected,
      tolerance,
      (firstVector: Vector3d, secondVector: Vector3d) =>
        (firstVector - secondVector).length match {
          case 0.0 =>
            None
          case difference: Double =>
            Some(difference)
        }
    )
}

object Vector3dMatchers extends Vector3dMatchers
