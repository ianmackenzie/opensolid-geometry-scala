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

import org.opensolid.core.DoubleGenerators._

trait IntervalGenerators {
  val singletonInterval: Gen[Interval] = randomDouble.map(Interval.singleton(_))

  val randomWidthInterval: Gen[Interval] =
    for {
      median <- randomDouble
      halfWidth <- randomDouble.map(math.abs(_))
      interval = Interval(median - halfWidth, median + halfWidth)
      if (!interval.width.isInfinity)
    } yield interval

  val negativeHalfOpenInterval: Gen[Interval] =
    randomDouble.map(Interval(Double.NegativeInfinity, _))

  val positiveHalfOpenInterval: Gen[Interval] =
    randomDouble.map(Interval(_, Double.PositiveInfinity))

  val oneUlpInterval: Gen[Interval] =
    randomDouble.map(value => Interval(value, value + math.ulp(value)))

  val twoUlpInterval: Gen[Interval] =
    randomDouble.map(value => Interval(value - math.ulp(value), value + math.ulp(value)))

  val closedInterval: Gen[Interval] =
    Gen.frequency(1 -> oneUlpInterval, 1 -> twoUlpInterval, 10 -> randomWidthInterval)

  val randomInterval: Gen[Interval] =
    Gen.frequency(
      1 -> Interval.Empty,
      1 -> Interval.Whole,
      1 -> Interval.Zero,
      1 -> Interval.singleton(1.0),
      1 -> Interval.singleton(-1.0),
      2 -> negativeHalfOpenInterval,
      2 -> positiveHalfOpenInterval,
      2 -> singletonInterval,
      2 -> oneUlpInterval,
      2 -> twoUlpInterval,
      10 -> randomWidthInterval
    )

  implicit val arbitraryInterval: Arbitrary[Interval] = Arbitrary(randomInterval)
}

object IntervalGenerators extends IntervalGenerators
