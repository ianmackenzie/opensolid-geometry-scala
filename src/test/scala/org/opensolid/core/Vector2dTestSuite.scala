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

  test("projectedOnto(axis)") {
    forAll {
      (vector: Vector2d, axis: Axis2d) => {
        val projected = vector.projectedOnto(axis)
        val tolerance = 3.ulps(vector.length)
        projected.length should approximatelyEqual(vector.dot(axis.direction).abs, tolerance)
        projected.cross(axis.direction) should approximatelyEqual(0.0, tolerance)
        projected.projectedOnto(axis) should approximatelyEqual(projected, tolerance)
      }
    }
  }

  test("normalized") {
    forAll {
      (vector: Vector2d) => {
        val normalized = vector.normalized
        if (vector == Vector2d.Zero) {
          normalized shouldBe Vector2d.Zero
        } else {
          normalized.length should approximatelyEqual(1.0, 2.ulps(1.0))
          val tolerance = 2.ulps(vector.length)
          vector.dot(normalized) should approximatelyEqual(vector.length, tolerance)
          (normalized * vector.length) should approximatelyEqual(vector, tolerance)
        }
      }
    }
  }

  test("direction") {
    forAll { (vector: Vector2d) => vector.direction.vector shouldBe vector.normalized }
  }
}
