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

import org.scalacheck._

trait DoubleGenerators {
  val finiteDouble: Gen[Double] =
    for {
      x <- Gen.chooseNum(-1.0, 1.0)
      y <- Gen.frequency(64 -> 1e0, 32 -> 1e1, 16 -> 1e2, 8 -> 1e3, 4 -> 1e4, 2 -> 1e5, 1 -> 1e6)
    } yield x * y

  val validDouble: Gen[Double] =
    Gen.frequency(
      8 -> finiteDouble,
      1 -> Double.PositiveInfinity,
      1 -> Double.NegativeInfinity
    )

  val anyDouble: Gen[Double] =
    Gen.frequency(
      8 -> validDouble,
      1 -> Double.NaN
    )

  implicit val arbitraryDouble: Arbitrary[Double] = Arbitrary(anyDouble)

  def sortedValues(count: Integer): Gen[List[Double]] =
    Gen.listOfN[Double](count, validDouble).map(list => list.sorted).suchThat(_.length == count)

  def valueWithin(interval: Interval): Gen[Double] = interval match {
    case Interval.Empty =>
      Double.NaN
    case Interval.Whole =>
      validDouble
    case Interval.NegativeInfinity =>
      Double.NegativeInfinity
    case Interval.PositiveInfinity =>
      Double.PositiveInfinity
    case Interval(Double.NegativeInfinity, upper) =>
      if (upper <= 0.0) {
        validDouble.map(upper - _.abs).map(-_.abs)
      } else {
        validDouble.map(upper - _.abs)
      }
    case Interval(lower, Double.PositiveInfinity) =>
      if (lower >= 0.0) {
        validDouble.map(lower + _.abs).map(_.abs)
      } else {
        validDouble.map(lower + _.abs)
      }
    case Interval(lower, upper) => {
      val containedValue =
        Gen.chooseNum(0.0, 1.0).map(interval.interpolated(_)).suchThat(interval.contains(_))
      if (upper <= 0.0) {
        containedValue.map(-_.abs)
      } else if (lower >= 0.0) {
        containedValue.map(_.abs)
      } else {
        containedValue
      }
    }
  }
}

object DoubleGenerators extends DoubleGenerators
