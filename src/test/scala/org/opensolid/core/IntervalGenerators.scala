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
  private[this] val finiteSingletonInterval: Gen[Interval] =
    finiteDouble.map(Interval.singleton(_))

  private[this] val validSingletonInterval: Gen[Interval] =
    validDouble.map(Interval.singleton(_))

  private[this] val oneUlpWidthInterval: Gen[Interval] =
    finiteDouble.map(value => Interval(value, value + math.ulp(value)))

  private[this] val twoUlpWidthInterval: Gen[Interval] =
    finiteDouble.map(value => Interval(value - math.ulp(value), value + math.ulp(value)))

  private[this] val finiteWidthInterval: Gen[Interval] =
    for {
      midpoint <- finiteDouble
      halfWidth <- finiteDouble.map(math.abs(_))
      interval = Interval(midpoint - halfWidth, midpoint + halfWidth)
      if (!interval.width.isInfinity)
    } yield interval

  private[this] val upperBoundedInterval: Gen[Interval] =
    finiteDouble.map(Interval(Double.NegativeInfinity, _))

  private[this] val lowerBoundedInterval: Gen[Interval] =
    finiteDouble.map(Interval(_, Double.PositiveInfinity))

  val finiteInterval: Gen[Interval] =
    Gen.frequency(
      8 -> finiteWidthInterval,
      1 -> twoUlpWidthInterval,
      1 -> oneUlpWidthInterval,
      1 -> finiteSingletonInterval
    )

  val validInterval: Gen[Interval] =
    Gen.frequency(
      8 -> finiteInterval,
      1 -> Interval.singleton(Double.NegativeInfinity),
      1 -> Interval.singleton(Double.PositiveInfinity),
      1 -> upperBoundedInterval,
      1 -> lowerBoundedInterval,
      1 -> Interval.Whole
    )

  val anyInterval: Gen[Interval] =
    Gen.frequency(
      8 -> validInterval,
      1 -> Interval.Empty
    )

  implicit val arbitraryInterval: Arbitrary[Interval] = Arbitrary(anyInterval)
}

object IntervalGenerators extends IntervalGenerators
