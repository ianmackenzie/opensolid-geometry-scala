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

class IntervalTestSuite extends TestSuite with IntervalGenerators {
  implicit override val generatorDrivenConfig =
    PropertyCheckConfig(minSuccessful = 500, maxDiscarded = 2500)

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
    } yield z
  }

  def approximatelyContain(value: Double): Matcher[Interval] = new Matcher[Interval] {
    def apply(interval: Interval): MatchResult = {
      val result =
        interval.contains(value) ||
        interval.contains(value, 2 * Interval.ulp(interval).max(math.ulp(1.0))) ||
        value.isNaN
      MatchResult(result, s"$interval does not contain $value", s"$interval contains $value")
    }
  }

  def testUnaryFunction(
    scalarFunction: (Double) => Double,
    intervalFunction: (Interval) => Interval,
    domain: Interval = Interval.Whole
  ): Unit = {
    forAll(testIntervalForDomain(domain), minSuccessful(50)) {
      (xInterval: Interval) => {
        val yInterval = intervalFunction(xInterval)
        if (xInterval.isEmpty) {
          yInterval shouldBe empty
        } else if (xInterval.isSingleton) {
          yInterval should approximatelyContain(scalarFunction(xInterval.lowerBound))
        } else if (yInterval.isEmpty) {
          forAll(valueWithin(xInterval.intersection(domain)), minSuccessful(10)) {
            (xValue: Double) => {
              val yValue = scalarFunction(xValue)
              assert(yValue.isInfinity || yValue.isNaN)
            }
          }
        } else {
          val yValues = evaluateWithin(xInterval.intersection(domain), scalarFunction)
          forAll(yValues, minSuccessful(10)) {
            (yValue: Double) => {
              yInterval should approximatelyContain(yValue)
            }
          }
        }
      }
    }
  }

  def testMixedFunction(
    scalarFunction: (Double, Double) => Double,
    intervalFunction: (Interval, Double) => Interval,
    xDomain: Interval = Interval.Whole,
    yDomain: Interval = Interval.Whole
  ): Unit = {
    forAll(testIntervalForDomain(xDomain), valueWithin(yDomain), minSuccessful(50)) {
      (xInterval: Interval, yValue: Double) => {
        val zInterval = intervalFunction(xInterval, yValue)
        if (xInterval.isEmpty) {
          zInterval shouldBe empty
        } else if (xInterval.isSingleton) {
          zInterval should approximatelyContain(scalarFunction(xInterval.lowerBound, yValue))
        } else if (zInterval.isEmpty) {
          forAll(valueWithin(xInterval.intersection(xDomain)), minSuccessful(10)) {
            (xValue: Double) => {
              val zValue = scalarFunction(xValue, yValue)
              assert(zValue.isInfinity || zValue.isNaN)
            }
          }
        } else {
          val zValues =
            evaluateWithin(
              xInterval.intersection(xDomain),
              xValue => scalarFunction(xValue, yValue)
            )
          forAll(zValues, minSuccessful(10)) {
            (zValue: Double) => {
              zInterval should approximatelyContain(zValue)
            }
          }
        }
      }
    }
  }

  def testBinaryFunction(
    scalarFunction: (Double, Double) => Double,
    intervalFunction: (Interval, Interval) => Interval,
    xDomain: Interval = Interval.Whole,
    yDomain: Interval = Interval.Whole
  ): Unit = {
    forAll(testIntervalForDomain(xDomain), testIntervalForDomain(yDomain), minSuccessful(50)) {
      (xInterval: Interval, yInterval: Interval) => {
        val zInterval = intervalFunction(xInterval, yInterval)
        if (xInterval.isEmpty || yInterval.isEmpty) {
          zInterval shouldBe empty
        } else if (xInterval.isSingleton && yInterval.isSingleton) {
          val zValue = scalarFunction(xInterval.lowerBound, yInterval.lowerBound)
          zInterval should approximatelyContain(zValue)
        } else if (zInterval.isEmpty) {
          forAll(
            valueWithin(xInterval.intersection(xDomain)),
            valueWithin(yInterval.intersection(yDomain)),
            minSuccessful(10)
          ) {
            (xValue: Double, yValue: Double) => {
              val zValue = scalarFunction(xValue, yValue)
              assert(zValue.isInfinity || zValue.isNaN)
            }
          }
        } else {
          val zValues =
            evaluateWithin(
              xInterval.intersection(xDomain),
              yInterval.intersection(yDomain),
              scalarFunction
            )
          forAll(zValues, minSuccessful(10)) {
            (zValue: Double) => {
              zInterval should approximatelyContain(zValue)
            }
          }
        }
      }
    }
  }

  test("equals(other)") {
    forAll { (interval: Interval) => interval shouldBe (interval) }
    forAll { (interval: Interval, value: Double) => interval should not be (value) }
  }

  test("toString") {
    Interval.Empty.toString shouldBe ("Interval.Empty")
    Interval.Whole.toString shouldBe ("Interval.Whole")
    Interval(2, 3).toString shouldBe ("Interval(2.0, 3.0)")
  }

  test("interpolated(value)") {
    forAll(closedInterval, Gen.chooseNum(0.0, 1.0), minSuccessful(500)) {
      (interval: Interval, value: Double) => {
        val interpolated = interval.interpolated(value)
        assert(interval.contains(interpolated, 2.0 * Interval.ulp(interval)))
      }
    }
  }

  test("randomValue") {
    forAll(closedInterval) {
      (interval: Interval) => {
        val randomValue = interval.randomValue
        assert(interval.contains(randomValue, 2.0 * Interval.ulp(interval)))
      }
    }
  }

  test("bisected") {
    forAll {
      (interval: Interval) => {
        val (lower, upper) = interval.bisected
        if (interval.isEmpty) {
          lower shouldBe empty
          upper shouldBe empty
        } else {
          lower.upperBound shouldBe (upper.lowerBound)
          lower.lowerBound shouldBe (interval.lowerBound)
          upper.upperBound shouldBe (interval.upperBound)

          val mid = lower.upperBound
          if (interval.lowerBound + math.ulp(interval.lowerBound) < interval.upperBound) {
            // There is enough room for a bisection point distinct from both lower and upper bounds
            mid shouldBe > (interval.lowerBound)
            mid shouldBe < (interval.upperBound)
          } else {
            // Very small interval - bisection point may be equal to lower and/or upper bounds
            mid shouldBe >= (interval.lowerBound)
            mid shouldBe <= (interval.upperBound)
          }
        }
      }
    }
  }

  test("hull(value)") {
    forAll {
      (interval: Interval, value: Double) => {
        val hull = interval.hull(value)
        if (interval.isEmpty) {
          assert(hull.isSingleton)
          hull.lowerBound shouldBe (value)
        } else {
          assert(hull.contains(interval))
          assert(hull.contains(value))
        }
      }
    }
  }

  test("hull(that)") {
    forAll {
      (firstInterval: Interval, secondInterval: Interval) => {
        val hull = firstInterval.hull(secondInterval)
        if (firstInterval.isEmpty) {
          hull shouldBe (secondInterval)
        } else if (secondInterval.isEmpty) {
          hull shouldBe (firstInterval)
        } else {
          assert(hull.contains(firstInterval))
          assert(hull.contains(secondInterval))
        }
      }
    }
  }

  test("intersection(that)") {
    forAll(randomWidthInterval, randomWidthInterval, minSuccessful(50)) {
      (firstInterval: Interval, secondInterval: Interval) => {
        val intersection = firstInterval.intersection(secondInterval)
        if (firstInterval.isEmpty || secondInterval.isEmpty) {
          intersection shouldBe empty
        } else {
          val firstValues = Gen.chooseNum(firstInterval.lowerBound, firstInterval.upperBound)
          forAll(firstValues, minSuccessful(10)) {
            firstValue =>
              intersection.contains(firstValue) shouldBe (secondInterval.contains(firstValue))
          }
          val secondValues = Gen.chooseNum(secondInterval.lowerBound, secondInterval.upperBound)
          forAll(secondValues, minSuccessful(10)) {
            secondValue =>
              intersection.contains(secondValue) shouldBe (firstInterval.contains(secondValue))
          }
        }
      }
    }
  }

  test("contains(value)") {
    forAll(sortedValues(3)) {
      case lower :: value :: upper :: Nil => assert(Interval(lower, upper).contains(value))
      case _ => fail
    }
  }

  test("contains(value, tolerance)") {
    forAll(minSuccessful(50)) {
      (interval: Interval) => {
        whenever (interval.width > 1e-3) {
          val randomTolerance: Gen[Double] = for {
            candidateTolerance <- Gen.chooseNum(-1e-3, 1e-3)
            if interval.lowerBound - candidateTolerance <= interval.upperBound + candidateTolerance
          } yield candidateTolerance

          forAll(randomTolerance, minSuccessful(2)) {
            (tolerance: Double) => {
              val expandedInterval =
                Interval(interval.lowerBound - tolerance, interval.upperBound + tolerance)
              forAll(valueWithin(expandedInterval), minSuccessful(5)) {
                (value: Double) => {
                  assert(interval.contains(value, tolerance))
                }
              }
            }
          }
        }
      }
    }
  }

  test("contains(that)") {
    forAll(sortedValues(4)) {
      case firstLower :: secondLower :: secondUpper :: firstUpper :: Nil =>
        assert(Interval(firstLower, firstUpper).contains(Interval(secondLower, secondUpper)))
      case _ => fail
    }
  }

  test("contains(that, tolerance)") {
    forAll(sortedValues(4)) {
      case first :: second :: third :: fourth :: Nil => {
        val minTolerance = (second - first).min(fourth - third)
        val maxTolerance = (second - first).max(fourth - third)
        assert(Interval(first, fourth).contains(Interval(second, third), -minTolerance + 1e-3))
        assert(Interval(second, third).contains(Interval(first, fourth), maxTolerance + 1e-3))
        assert(!Interval(second, third).contains(Interval(first, fourth), maxTolerance - 1e-3))
        assert(!Interval(first, fourth).contains(Interval(second, third), -minTolerance - 1e-3))
      }
      case _ => fail
    }
  }

  test("overlaps(that)") {
    forAll(sortedValues(4)) {
      case firstLower :: secondLower :: firstUpper :: secondUpper :: Nil => {
        val first = Interval(firstLower, firstUpper)
        val second = Interval(secondLower, secondUpper)
        assert(first.overlaps(second))
        assert(second.overlaps(first))
      }
      case _ => fail
    }
  }

  test("overlaps(that, tolerance)") {
    forAll(sortedValues(4)) {
      case first :: second :: third :: fourth :: Nil => {
        val tolerance = third - second
        assert(Interval(first, third).overlaps(Interval(second, fourth), -tolerance + 1e-3))
        assert(Interval(second, fourth).overlaps(Interval(first, third), -tolerance + 1e-3))
        assert(!Interval(first, third).overlaps(Interval(second, fourth), -tolerance - 1e-3))
        assert(!Interval(second, fourth).overlaps(Interval(first, third), -tolerance - 1e-3))
        assert(Interval(first, second).overlaps(Interval(third, fourth), tolerance + 1e-3))
        assert(Interval(third, fourth).overlaps(Interval(first, second), tolerance + 1e-3))
        assert(!Interval(first, second).overlaps(Interval(third, fourth), tolerance - 1e-3))
        assert(!Interval(third, fourth).overlaps(Interval(first, second), tolerance - 1e-3))
      }
      case _ => fail
    }
  }

  test("unary_-") {
    testUnaryFunction(value => -value, interval => -interval)
  }

  test("+(value)") {
    testMixedFunction((x, y) => x + y, (x, y) => x + y)
  }

  test("+(that)") {
    testBinaryFunction((x, y) => x + y, (x, y) => x + y)
  }

  test("-(value)") {
    testMixedFunction((x, y) => x - y, (x, y) => x - y)
  }

  test("-(that)") {
    testBinaryFunction((x, y) => x - y, (x, y) => x - y)
  }

  test("*(value)") {
    testMixedFunction((x, y) => x * y, (x, y) => x * y)
  }

  test("*(that)") {
    testBinaryFunction((x, y) => x * y, (x, y) => x * y)
  }

  test("/(value)") {
    testMixedFunction((x, y) => x / y, (x, y) => x / y)
  }

  test("/(that)") {
    testBinaryFunction((x, y) => x / y, (x, y) => x / y)
  }

  test("abs") {
    testUnaryFunction(value => value.abs, interval => interval.abs)
  }

  test("squared") {
    testUnaryFunction(value => value * value, interval => interval.squared)
  }

  test("sqrt") {
    testUnaryFunction(
      value => math.sqrt(value),
      interval => Interval.sqrt(interval),
      Interval(0.0, Double.PositiveInfinity)
    )
  }

  test("sin") {
    testUnaryFunction(value => math.sin(value), interval => Interval.sin(interval))
  }

  test("cos") {
    testUnaryFunction(value => math.cos(value), interval => Interval.cos(interval))
  }

  test("tan") {
    testUnaryFunction(value => math.tan(value), interval => Interval.tan(interval))
  }

  test("asin") {
    testUnaryFunction(
      value => math.asin(value),
      interval => Interval.asin(interval),
      Interval(-1, 1)
    )
  }

  test("acos") {
    testUnaryFunction(
      value => math.acos(value),
      interval => Interval.acos(interval),
      Interval(-1, 1)
    )
  }

  test("atan") {
    testUnaryFunction(value => math.atan(value), interval => Interval.atan(interval))
  }

  test("atan2") {
    testBinaryFunction((y, x) => math.atan2(y, x), (y, x) => Interval.atan2(y, x))
  }

  test("exp") {
    testUnaryFunction(
      value => math.exp(value),
      interval => Interval.exp(interval),
      Interval(Double.NegativeInfinity, 1e2)
    )
  }

  test("log") {
    testUnaryFunction(
      value => math.log(value),
      interval => Interval.log(interval),
      Interval(0.0, Double.PositiveInfinity)
    )
  }
}
