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

import org.scalatest._
import org.scalatest.matchers._

trait DoubleMatchers {
  implicit class DoubleExtensions(value: Double) {
    def ulps(nominal: Double): Double = value.abs * math.ulp(nominal).max(math.ulp(1.0))
  }

  def approximatelyEqual(expected: Double, tolerance: Double): ApproximatelyEqualMatcher[Double] =
    new ApproximatelyEqualMatcher[Double](
      expected,
      tolerance,
      (firstValue: Double, secondValue: Double) => (firstValue - secondValue).abs
    )
}

object DoubleMatchers extends DoubleMatchers
