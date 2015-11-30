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

import org.opensolid.core.IntervalGenerators._
import org.scalacheck._
import org.scalacheck.Prop.BooleanOperators

object IntervalProperties extends Properties("Interval") {
  def valueWithin(interval: Interval): Gen[Double] = interval match {
    case Interval.Whole => randomDouble
    case Interval(Double.NegativeInfinity, upper) => Gen.chooseNum(upper - 1e8, upper)
    case Interval(lower, Double.PositiveInfinity) => Gen.chooseNum(lower, lower + 1e8)
    case _ => Gen.chooseNum(0.0, 1.0).map(interval.interpolated(_)).suchThat(interval.contains(_))
  }

  def expandedInterval(interval: Interval): Interval =
    Interval(interval.lowerBound - interval.width / 2, interval.upperBound + interval.width / 2)

  def intervalOverlapping(domain: Interval): Gen[Interval] = {
    val expandedDomain = expandedInterval(domain)
    for {
      firstValue <- valueWithin(expandedDomain)
      secondValue <- valueWithin(expandedDomain)
      interval = firstValue.hull(secondValue)
      if interval.overlaps(domain)
    } yield interval
  }

  def testIntervalForDomain(domain: Interval): Gen[Interval] = {
    Gen.frequency(
      1 -> Interval.Empty,
      1 -> Interval.Whole,
      10 -> intervalOverlapping(domain)
    )
  }

  def evaluateWithin(domain: Interval, unaryFunction: (Double) => Double): Gen[Double] = {
    for {
      x <- valueWithin(domain)
      y = unaryFunction(x)
      if !y.isInfinity && !y.isNaN
    } yield y
  }

  def evaluateWithin(
    xDomain: Interval,
    yDomain: Interval,
    binaryFunction: (Double, Double) => Double
  ): Gen[Double] = {
    for {
      x <- valueWithin(xDomain)
      y <- valueWithin(yDomain)
      z = binaryFunction(x, y)
      if !z.isInfinity && !z.isNaN
    } yield z
  }

  def tolerantContains(interval: Interval, value: Double): Prop = {
    interval.contains(value) ||
      interval.contains(value, 2 * Interval.ulp(interval).max(math.ulp(1.0))) ||
      (interval.isEmpty && value.isNaN)
  }

  def classifyInterval(interval: Interval): String = interval match {
    case Interval.Empty => "empty"
    case Interval.Whole => "whole"
    case _ => "other"
  }

  def unaryProperty(
    scalarFunction: (Double) => Double,
    intervalFunction: (Interval) => Interval,
    domain: Interval = Interval.Whole
  ): Prop = {
    Prop.forAll(testIntervalForDomain(domain)) {
      (xInterval: Interval) => {
        val yInterval = intervalFunction(xInterval)
        if (xInterval.isEmpty) {
          yInterval.isEmpty: Prop
        } else if (xInterval.isSingleton) {
          tolerantContains(yInterval, scalarFunction(xInterval.lowerBound))
        } else {
          Prop.forAll(evaluateWithin(xInterval.intersection(domain), scalarFunction)) {
            (yValue: Double) => {
              val tag = s"xInterval: $xInterval, yInterval: $yInterval, yValue: $yValue"
              tag |: tolerantContains(yInterval, yValue)
            }
          }
        }
      }
    }
  }

  def mixedProperty(
    scalarFunction: (Double, Double) => Double,
    intervalFunction: (Interval, Double) => Interval,
    xDomain: Interval = Interval.Whole,
    yDomain: Interval = Interval.Whole
  ): Prop = {
    Prop.forAll(testIntervalForDomain(xDomain), valueWithin(yDomain)) {
      (xInterval: Interval, yValue: Double) => {
        val zInterval = intervalFunction(xInterval, yValue)
        if (xInterval.isEmpty) {
          zInterval.isEmpty: Prop
        } else if (xInterval.isSingleton) {
          tolerantContains(zInterval, scalarFunction(xInterval.lowerBound, yValue))
        } else {
          Prop.forAll(
            evaluateWithin(
              xInterval.intersection(xDomain),
              xValue => scalarFunction(xValue, yValue)
            )
          ) {
            (zValue: Double) => {
              val tag =
                s"xInterval: $xInterval, yValue: $yValue, zInterval: $zInterval, zValue: $zValue"
              tag |: tolerantContains(zInterval, zValue)
            }
          }
        }
      }
    }
  }

  def binaryProperty(
    scalarFunction: (Double, Double) => Double,
    intervalFunction: (Interval, Interval) => Interval,
    xDomain: Interval = Interval.Whole,
    yDomain: Interval = Interval.Whole): Prop = {

    Prop.forAll(testIntervalForDomain(xDomain), testIntervalForDomain(yDomain)) {
      (xInterval: Interval, yInterval: Interval) => {
        val zInterval = intervalFunction(xInterval, yInterval)
        if (xInterval.isEmpty || yInterval.isEmpty) {
          zInterval.isEmpty:  Prop
        } else if (xInterval.isSingleton && yInterval.isSingleton) {
          tolerantContains(zInterval, scalarFunction(xInterval.lowerBound, yInterval.lowerBound))
        } else {
          val zValues = evaluateWithin(
            xInterval.intersection(xDomain),
            yInterval.intersection(yDomain),
            scalarFunction
          )
          Prop.forAll(zValues) {
            (zValue: Double) => {
              val tag =
                s"xInterval: $xInterval, " +
                s"yInterval: $yInterval, " +
                s"zInterval: $zInterval, " +
                s"zValue: $zValue"
              tag |: tolerantContains(zInterval, zValue)
            }
          }
        }
      }
    }
  }

  property("unary_-") = unaryProperty(value => -value, interval => -interval)

  property("+(value)") = mixedProperty((x, y) => x + y, (x, y) => x + y)

  property("+(that)") = binaryProperty((x, y) => x + y, (x, y) => x + y)

  property("-(value)") = mixedProperty((x, y) => x - y, (x, y) => x - y)

  property("-(that)") = binaryProperty((x, y) => x - y, (x, y) => x - y)

  property("*(value)") = mixedProperty((x, y) => x * y, (x, y) => x * y)

  property("*(that)") = binaryProperty((x, y) => x * y, (x, y) => x * y)

  property("/(value)") = mixedProperty((x, y) => x / y, (x, y) => x / y)

  property("/(that)") = binaryProperty((x, y) => x / y, (x, y) => x / y)

  property("abs") = unaryProperty(value => value.abs, interval => interval.abs)

  property("squared") = unaryProperty(value => value * value, interval => interval.squared)

  property("sqrt") =
    unaryProperty(
      value => math.sqrt(value),
      interval => Interval.sqrt(interval),
      Interval(0.0, Double.PositiveInfinity))

  property("sin") = unaryProperty(value => math.sin(value), interval => Interval.sin(interval))

  property("cos") = unaryProperty(value => math.cos(value), interval => Interval.cos(interval))

  property("tan") = unaryProperty(value => math.tan(value), interval => Interval.tan(interval))

  property("asin") =
    unaryProperty(
      value => math.asin(value),
      interval => Interval.asin(interval),
      Interval(-1, 1))

  property("acos") =
    unaryProperty(
      value => math.acos(value),
      interval => Interval.acos(interval),
      Interval(-1, 1))

  property("atan") = unaryProperty(value => math.atan(value), interval => Interval.atan(interval))

  property("atan2") = binaryProperty((y, x) => math.atan2(y, x), (y, x) => Interval.atan2(y, x))

  property("exp") =
    unaryProperty(
      value => math.exp(value),
      interval => Interval.exp(interval),
      Interval(Double.NegativeInfinity, 1e2)
    )

  property("log") =
    unaryProperty(
      value => math.log(value),
      interval => Interval.log(interval),
      Interval(0.0, Double.PositiveInfinity)
    )
}
