package org.opensolid.core

import org.scalacheck._
import org.scalacheck.Prop.BooleanOperators

object DoubleGenerators {
  val finiteDouble: Gen[Double] = Gen.chooseNum(-1e-6, 1e6)

  val zeroToOne: Gen[Double] = Gen.chooseNum(0.0, 1.0)
}

object IntervalGenerators {
  import DoubleGenerators._

  val singletonInterval: Gen[Interval] = finiteDouble.map(Interval(_))

  val closedInterval: Gen[Interval] = for {
    firstValue <- finiteDouble
    secondValue <- finiteDouble
  } yield Interval(firstValue.min(secondValue), firstValue.max(secondValue))

  val negativeHalfOpenInterval: Gen[Interval] =
    finiteDouble.map(Interval(Double.NegativeInfinity, _))

  val positiveHalfOpenInterval: Gen[Interval] =
    finiteDouble.map(Interval(_, Double.PositiveInfinity))

  val anyInterval: Gen[Interval] = Gen.frequency(
    1 -> Interval.Empty,
    1 -> Interval.Whole,
    1 -> Interval(0.0),
    1 -> Interval(1.0),
    1 -> Interval(-1.0),
    2 -> negativeHalfOpenInterval,
    2 -> positiveHalfOpenInterval,
    2 -> singletonInterval,
    10 -> closedInterval)

  implicit val arbitraryInterval: Arbitrary[Interval] = Arbitrary(anyInterval)
}

object IntervalSpec extends Properties("Interval") {
  import DoubleGenerators._
  import IntervalGenerators._

  property("equals") = Prop.forAll {(interval: Interval) => interval == interval}

  property("interpolated") = Prop.forAll(closedInterval, zeroToOne) {
    (interval: Interval, value: Double) => {
      val interpolated = interval.interpolated(value)
      interval.contains(interpolated, 2.0 * Interval.ulp(interval))
    }
  }

  property("randomValue") = Prop.forAll(closedInterval) {
    (interval: Interval) => {
      val randomValue = interval.randomValue()
      interval.contains(randomValue, 2.0 * Interval.ulp(interval))
    }
  }

  property("bisected") = Prop.forAll {
    (interval: Interval) => {
      val (lower, upper) = interval.bisected
      if (interval.isEmpty) {
        lower.isEmpty && upper.isEmpty
      } else {
        val basicChecks = (lower.upperBound == upper.lowerBound) &&
          (lower.lowerBound == interval.lowerBound) &&
          (upper.upperBound == interval.upperBound)
        val mid = lower.upperBound
        
        if (interval.width.isZero(DefaultPrecision)) {
          basicChecks &&
            mid >= interval.lowerBound &&
            mid <= interval.upperBound
        } else {
          basicChecks &&
            mid > interval.lowerBound &&
            mid < interval.upperBound
        }
      }
    }
  }
}
