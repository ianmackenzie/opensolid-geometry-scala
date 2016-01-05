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
  val randomDouble: Gen[Double] =
    for {
      x <- Gen.chooseNum(-1.0, 1.0)
      y <- Gen.frequency(64 -> 1e0, 32 -> 1e1, 16 -> 1e2, 8 -> 1e3, 4 -> 1e4, 2 -> 1e5, 1 -> 1e6)
    } yield x * y

  implicit val arbitraryDouble: Arbitrary[Double] = Arbitrary(randomDouble)

  def sortedValues(count: Integer): Gen[List[Double]] =
    Gen.listOfN[Double](count, randomDouble).map(list => list.sorted).suchThat(_.length == count)

  def valueWithin(interval: Interval): Gen[Double] = interval match {
    case Interval.Whole => randomDouble
    case Interval(Double.NegativeInfinity, upper) => randomDouble.map(upper - _.abs)
    case Interval(lower, Double.PositiveInfinity) => randomDouble.map(lower + _.abs)
    case _ => Gen.chooseNum(0.0, 1.0).map(interval.interpolated(_)).suchThat(interval.contains(_))
  }
}

object DoubleGenerators extends DoubleGenerators
