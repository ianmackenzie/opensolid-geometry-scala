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

class IntervalTestSuite extends TestSuite
  with IntervalGenerators
  with DoubleGenerators {

  implicit override val generatorDrivenConfig =
    PropertyCheckConfig(minSuccessful = 10000, maxDiscarded = 2500)

  def testIntervalForDomain(domain: Interval): Gen[Interval] = {
    val candidateInterval =
      domain match {
        case Interval.Whole =>
          validInterval
        case Interval(Double.NegativeInfinity, upperBound) =>
          validInterval.map(_ + upperBound)
        case Interval(lowerBound, Double.PositiveInfinity) =>
          validInterval.map(_ + lowerBound)
        case _ =>
          validInterval.map(_ * (domain.width / 2.0) + domain.midpoint)
      }
    candidateInterval.suchThat(_.overlaps(domain))
  }

  def testValueForDomain(domain: Interval): Gen[Double] =
    Gen.frequency(16 -> valueWithin(domain), 1 -> Double.NaN)

  def evaluateWithin(domain: Interval, unaryFunction: (Double) => Double): Gen[Double] =
    for {
      x <- valueWithin(domain)
      y = unaryFunction(x)
    } yield y

  def evaluateWithin(
    xDomain: Interval,
    yDomain: Interval,
    binaryFunction: (Double, Double) => Double
  ): Gen[Double] =
    for {
      x <- valueWithin(xDomain)
      y <- valueWithin(yDomain)
      z = binaryFunction(x, y)
    } yield z

  def approximatelyContain(value: Double): Matcher[Interval] =
    new Matcher[Interval] {
      def apply(interval: Interval): MatchResult = {
        val result = interval.expandedBy(2 * eps(interval)).contains(value) || value.isNaN
        MatchResult(result, s"$interval does not contain $value", s"$interval contains $value")
      }
    }

  def testUnaryFunction(
    scalarFunction: (Double) => Double,
    intervalFunction: (Interval) => Interval,
    domain: Interval = Interval.Whole
  ): Unit =
    forAll(testIntervalForDomain(domain), minSuccessful(50)) {
      (xInterval: Interval) => {
        val yInterval = intervalFunction(xInterval)
        if (xInterval.isEmpty) {
          yInterval.shouldBe(empty)
        } else if (xInterval.isSingleton) {
          yInterval.should(approximatelyContain(scalarFunction(xInterval.lowerBound)))
        } else if (yInterval.isEmpty) {
          forAll(valueWithin(xInterval.intersection(domain)), minSuccessful(10)) {
            (xValue: Double) => {
              val yValue = scalarFunction(xValue)
              assert(yValue.isNaN)
            }
          }
        } else {
          val yValues = evaluateWithin(xInterval.intersection(domain), scalarFunction)
          forAll(yValues, minSuccessful(10)) {
            (yValue: Double) => yInterval.should(approximatelyContain(yValue))
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
          zInterval.shouldBe(empty)
        } else if (xInterval.isSingleton) {
          zInterval.should(approximatelyContain(scalarFunction(xInterval.lowerBound, yValue)))
        } else if (zInterval.isEmpty) {
          forAll(valueWithin(xInterval.intersection(xDomain)), minSuccessful(10)) {
            (xValue: Double) => {
              val zValue = scalarFunction(xValue, yValue)
              assert(zValue.isNaN)
            }
          }
        } else {
          val zValues =
            evaluateWithin(
              xInterval.intersection(xDomain),
              xValue => scalarFunction(xValue, yValue)
            )
          forAll(zValues, minSuccessful(10)) {
            (zValue: Double) => zInterval.should(approximatelyContain(zValue))
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
          zInterval.shouldBe(empty)
        } else if (xInterval.isSingleton && yInterval.isSingleton) {
          val zValue = scalarFunction(xInterval.lowerBound, yInterval.lowerBound)
          zInterval.should(approximatelyContain(zValue))
        } else if (zInterval.isEmpty) {
          forAll(
            valueWithin(xInterval.intersection(xDomain)),
            valueWithin(yInterval.intersection(yDomain)),
            minSuccessful(10)
          ) {
            (xValue: Double, yValue: Double) => {
              val zValue = scalarFunction(xValue, yValue)
              assert(zValue.isNaN)
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
            (zValue: Double) => zInterval.should(approximatelyContain(zValue))
          }
        }
      }
    }
  }

  test("equals(other)") {
    forAll { (interval: Interval) => interval.shouldBe(interval) }
    forAll { (interval: Interval, value: Double) => interval.should(not(be(value))) }
  }

  test("toString") {
    Interval.Empty.toString.shouldBe("Interval.Empty")
    Interval.Whole.toString.shouldBe("Interval.Whole")
    Interval(2, 3).toString.shouldBe("Interval(2.0, 3.0)")
  }

  test("interpolated(value)") {
    forAll(finiteInterval, Gen.chooseNum(0.0, 1.0), minSuccessful(500)) {
      (interval: Interval, value: Double) => {
        val interpolated = interval.interpolated(value)
        assert(interval.expandedBy(2 * eps(interval)).contains(interpolated))
      }
    }
  }

  test("randomValue") {
    forAll(finiteInterval) {
      (interval: Interval) => {
        val randomValue = interval.randomValue
        assert(interval.isEmpty || interval.expandedBy(2 * eps(interval)).contains(randomValue))
      }
    }
  }

  test("bisected") {
    forAll {
      (interval: Interval) => {
        val (lower, upper) = interval.bisected
        if (interval.isEmpty) {
          lower.shouldBe(empty)
          upper.shouldBe(empty)
        } else {
          lower.upperBound.shouldBe(upper.lowerBound)
          lower.lowerBound.shouldBe(interval.lowerBound)
          upper.upperBound.shouldBe(interval.upperBound)

          val mid = lower.upperBound
          if (interval.lowerBound + math.ulp(interval.lowerBound) < interval.upperBound) {
            // There is enough room for a bisection point distinct from both lower and upper bounds
            assert(mid > interval.lowerBound)
            assert(mid < interval.upperBound)
          } else {
            // Very small interval - bisection point may be equal to lower and/or upper bounds
            assert(mid >= interval.lowerBound)
            assert(mid <= interval.upperBound)
          }
        }
      }
    }
  }

  test("hull(value)") {
    forAll {
      (interval: Interval, value: Double) => {
        val hull = interval.hull(value)
        if (value.isNaN) {
          hull.shouldBe(interval)
        } else if (interval.isEmpty) {
          assert(hull.isSingleton)
          hull.lowerBound.shouldBe(value)
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
          hull.shouldBe(secondInterval)
        } else if (secondInterval.isEmpty) {
          hull.shouldBe(firstInterval)
        } else {
          assert(hull.contains(firstInterval))
          assert(hull.contains(secondInterval))
        }
      }
    }
  }

  test("intersection(that)") {
    forAll(anyInterval, anyInterval, minSuccessful(50)) {
      (firstInterval: Interval, secondInterval: Interval) => {
        val intersection = firstInterval.intersection(secondInterval)
        if (firstInterval.isEmpty || secondInterval.isEmpty) {
          intersection.shouldBe(empty)
        } else {
          val firstValues = valueWithin(firstInterval)
          forAll(firstValues, minSuccessful(10)) {
            firstValue =>
              intersection.contains(firstValue).shouldBe(secondInterval.contains(firstValue))
          }
          val secondValues = valueWithin(secondInterval)
          forAll(secondValues, minSuccessful(10)) {
            secondValue =>
              intersection.contains(secondValue).shouldBe(firstInterval.contains(secondValue))
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

  test("contains(that)") {
    forAll(sortedValues(4)) {
      case firstLower :: secondLower :: secondUpper :: firstUpper :: Nil =>
        assert(Interval(firstLower, firstUpper).contains(Interval(secondLower, secondUpper)))
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
    (Interval.Whole * Interval.Zero).shouldBe(Interval.Zero)
    (Interval.Zero * Interval.Whole).shouldBe(Interval.Zero)
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
