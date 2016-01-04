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
  case class Ulps(count: Int)

  implicit class IntExtensions(value: Int) {
    def ulps: Ulps = Ulps(value)
  }

  class ApproximatelyEqualMatcher(expected: Double, tolerance: Ulps) extends Matcher[Double] {
    def apply(value: Double): MatchResult =
      MatchResult(
        (value - expected).abs <= tolerance.count * math.ulp(expected).max(math.ulp(1.0)),
        s"$value did not equal $expected to within ${tolerance.count} ulps",
        s"$value equalled $expected to within ${tolerance.count} ulps"
      )
  }

  def approximatelyEqual(expected: Double, tolerance: Ulps) =
    new ApproximatelyEqualMatcher(expected, tolerance)
}

object DoubleMatchers extends DoubleMatchers
