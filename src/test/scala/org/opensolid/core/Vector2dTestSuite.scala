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

import org.scalacheck.Gen
import org.scalatest._
import org.scalatest.matchers._
import org.scalatest.prop._

class Vector2dTestSuite
  extends TestSuite with Vector2dGenerators with Axis2dGenerators with Vector2dMatchers {

  test("length") {
    forAll {
      (vector: Vector2d, scale: Double) => {
        val scaledLength = vector.length * scale.abs
        val tolerance = 2.ulps(scaledLength)
        (vector * scale).length should approximatelyEqual(scaledLength, tolerance)
      }
    }
  }

  test("squaredLength") {
    forAll {
      (vector: Vector2d) => {
        val tolerance = 2.ulps(vector.squaredLength)
        vector.squaredLength should approximatelyEqual(vector.length * vector.length, tolerance)
      }
    }
  }
}
