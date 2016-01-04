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
import org.opensolid.core.IntervalGenerators._
import org.opensolid.core.Vector2dGenerators._
import org.scalacheck.Gen
import org.scalatest._
import org.scalatest.matchers._
import org.scalatest.prop._

class Vector2dTestSuite extends TestSuite {
  test("length") {
    forAll {
      (vector: Vector2d, scale: Double) => {
        (vector * scale).length should approximatelyEqual(vector.length * scale.abs, 2.ulps)
      }
    }
  }

  test("squaredLength") {
    forAll {
      (vector: Vector2d) => {
        vector.squaredLength should approximatelyEqual(vector.length * vector.length, 2.ulps)
      }
    }
  }
}
