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
  import DoubleMatchers._

  def beEqualTo(expected: Double, tolerance: Double): Matcher[Double] =
    new ApproximatelyEqualMatcher[Double](
      expected,
      tolerance,
      (firstValue: Double, secondValue: Double) => difference(firstValue, secondValue).map(_.abs)
    )

  def beLessThanOrEqualTo(expected: Double, tolerance: Double): Matcher[Double] =
    new LessThanOrEqualToMatcher(expected, tolerance)

  def beGreaterThanOrEqualTo(expected: Double, tolerance: Double): Matcher[Double] =
    new GreaterThanOrEqualToMatcher(expected, tolerance)
}

object DoubleMatchers extends DoubleMatchers {
  private def difference(firstValue: Double, secondValue: Double): Option[Double] =
    if (firstValue == secondValue || (firstValue.isNaN && secondValue.isNaN)) {
      None
    } else {
      Some(firstValue - secondValue)
    }

  class LessThanOrEqualToMatcher(expected: Double, tolerance: Double) extends Matcher[Double] {
    def apply(actual: Double): MatchResult =
      difference(actual, expected) match {
        case None =>
          MatchResult(true, "", s"$actual is equal to $expected")
        case Some(delta) =>
          MatchResult(
            delta <= tolerance,
            s"$actual is greater than $expected by more than $tolerance (difference: $delta)",
            s"$actual is less than or equal to $expected to within $tolerance (difference: $delta)"
          )
      }
  }

  class GreaterThanOrEqualToMatcher(expected: Double, tolerance: Double) extends Matcher[Double] {
    def apply(actual: Double): MatchResult =
      difference(actual, expected) match {
        case None =>
          MatchResult(true, "", s"$actual is equal to $expected")
        case Some(delta) =>
          MatchResult(
            delta >= tolerance,
            s"$actual is greater than $expected by more than $tolerance (difference: $delta)",
            s"$actual is less than or equal to $expected to within $tolerance (difference: $delta)"
          )
      }
  }
}
