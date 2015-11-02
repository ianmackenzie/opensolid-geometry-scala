package org.opensolid.core

import org.scalacheck._
import org.scalacheck.Prop.BooleanOperators

object IntervalGenerators {
  val randomDouble: Gen[Double] =
    for {
      s <- Gen.choose(0L, 1L)
      e <- Gen.choose(0L, 40L)
      m <- Gen.choose(0L, 0xfffffffffffffL)
    } yield java.lang.Double.longBitsToDouble((s << 63) | (e << 52) | m)

  implicit val arbitraryDouble: Arbitrary[Double] = Arbitrary(randomDouble)

  val singletonInterval: Gen[Interval] = randomDouble.map(Interval(_))

  val randomInterval: Gen[Interval] = for {
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
    randomDouble.map(
      value => Interval(value - math.ulp(value), value + math.ulp(value)))

  val closedInterval: Gen[Interval] = Gen.frequency(
    1 -> oneUlpInterval,
    1 -> twoUlpInterval,
    10 -> randomInterval)

  implicit val arbitraryInterval: Arbitrary[Interval] = Arbitrary(Gen.frequency(
    1 -> Interval.Empty,
    1 -> Interval.Whole,
    1 -> Interval(0.0),
    1 -> Interval(1.0),
    1 -> Interval(-1.0),
    2 -> negativeHalfOpenInterval,
    2 -> positiveHalfOpenInterval,
    2 -> singletonInterval,
    2 -> oneUlpInterval,
    2 -> twoUlpInterval,
    10 -> randomInterval))

  def sortedValues(count: Integer): Gen[List[Double]] =
    Gen.listOfN[Double](count, randomDouble).map(list => list.sorted)
}

object IntervalProperties extends Properties("Interval") {
  import IntervalGenerators._

  property("equals(other)") = Prop.forAll {(interval: Interval) => interval == interval} &&
    Prop.forAll {(interval: Interval, value: Double) => interval != value}

  property("interpolated(value)") = Prop.forAll(closedInterval, Gen.chooseNum(0.0, 1.0)) {
    (interval: Interval, value: Double) => {
      val interpolated = interval.interpolated(value)
      interval.contains(interpolated, 2.0 * Interval.ulp(interval))
    }
  }

  property("randomValue()") = Prop.forAll(closedInterval) {
    (interval: Interval) => {
      val randomValue = interval.randomValue
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
        if (interval.lowerBound + math.ulp(interval.lowerBound) < interval.upperBound) {
          // There is enough room for a bisection point distinct from both lower and upper bounds
          basicChecks &&
            mid > interval.lowerBound &&
            mid < interval.upperBound
        } else {
          // Very small interval - bisection point may be equal to lower and/or upper bounds
          basicChecks &&
            mid >= interval.lowerBound &&
            mid <= interval.upperBound
        }
      }
    }
  }

  property("hull(value)") = Prop.forAll {
    (interval: Interval, value: Double) => {
      val hull = interval.hull(value)

      if (interval.isEmpty) {
        hull.isSingleton && hull.lowerBound == value
      } else {
        hull.contains(interval) && hull.contains(value)
      }
    }
  }

  property("hull(that)") = Prop.forAll {
    (firstInterval: Interval, secondInterval: Interval) => {
      val hull = firstInterval.hull(secondInterval)
      
      if (firstInterval.isEmpty) {
        hull == secondInterval
      } else if (secondInterval.isEmpty) {
        hull == firstInterval
      } else {
        hull.contains(firstInterval) && hull.contains(secondInterval)
      }
    }
  }

  property("intersection(that)") = Prop.forAll {
    (firstInterval: Interval, secondInterval: Interval) => {
      val intersection = firstInterval.intersection(secondInterval)

      if (firstInterval.isEmpty || secondInterval.isEmpty) {
        intersection.isEmpty: Prop
      } else {
        Prop.forAll(Gen.chooseNum(firstInterval.lowerBound, firstInterval.upperBound)) {
          firstValue => intersection.contains(firstValue) == secondInterval.contains(firstValue)
        } &&
        Prop.forAll(Gen.chooseNum(secondInterval.lowerBound, secondInterval.upperBound)) {
          secondValue => intersection.contains(secondValue) == firstInterval.contains(secondValue)
        }
      }
    }
  }

  property("contains(value)") = Prop.forAll(sortedValues(3)) {
    case lower :: value :: upper :: Nil => Interval(lower, upper).contains(value)
    case _ => false
  }

  property("contains(that)") = Prop.forAll(sortedValues(4)) {
    case firstLower :: secondLower :: secondUpper :: firstUpper :: Nil =>
      Interval(firstLower, firstUpper).contains(Interval(secondLower, secondUpper))
    case _ => false
  }

  property("overlaps(that)") = Prop.forAll(sortedValues(4)) {
    case firstLower :: secondLower :: firstUpper :: secondUpper :: Nil => {
      val first = Interval(firstLower, firstUpper)
      val second = Interval(secondLower, secondUpper)
      first.overlaps(second) && second.overlaps(first)
    }
    case _ => false
  }

  def valueWithin(interval: Interval): Gen[Double] = {
    if (interval.width.isInfinity) {
      randomDouble.retryUntil(x => interval.contains(x))
    } else {
      Gen.chooseNum(0.0, 1.0).map(interval.interpolated(_))
    }
  }

  def intervalWithin(domain: Interval): Gen[Interval] = {
    for {
      firstValue <- valueWithin(domain)
      secondValue <- valueWithin(domain)
    } yield Interval.hull(firstValue, secondValue)
  }

  def evaluateWithin(interval: Interval, scalarFunction: (Double) => Double): Gen[Double] = {
    valueWithin(interval).map(x => scalarFunction(x)).suchThat(y => !y.isInfinity && !y.isNaN)
  }

  def unaryProperty(
    scalarFunction: (Double) => Double,
    intervalFunction: (Interval) => Interval,
    domain: Interval = Interval.Whole): Prop = {

    Prop.forAll(intervalWithin(domain)) {
      (xInterval: Interval) => {
        val yInterval = intervalFunction(xInterval)
        if (xInterval.isEmpty) {
          yInterval.isEmpty: Prop
        } else if (xInterval.isSingleton) {
          val yValue = scalarFunction(xInterval.lowerBound)
          (yInterval.lowerBound == yValue && yInterval.upperBound == yValue): Prop
        } else {
          Prop.forAll(evaluateWithin(xInterval, scalarFunction)) {
            (yValue: Double) => {
              s"xInterval: $xInterval, yInterval: $yInterval, yValue: $yValue" |:
                yInterval.contains(yValue, 2 * Interval.ulp(yInterval).max(math.ulp(1.0)))
            }
          }
        }
      }
    }
  }

  property("unary_-") = unaryProperty(value => -value, interval => -interval)

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

  property("exp") = unaryProperty(value => math.exp(value), interval => Interval.exp(interval))

  property("log") =
    unaryProperty(
      value => math.log(value),
      interval => Interval.log(interval),
      Interval(0.0, Double.PositiveInfinity))
}
