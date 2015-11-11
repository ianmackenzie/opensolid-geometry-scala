package org.opensolid.core

import org.scalacheck._
import org.scalacheck.Prop.BooleanOperators

object IntervalGenerators {
  val randomDouble: Gen[Double] = for {
    x <- Gen.chooseNum(-1.0, 1.0)
    y <- Gen.chooseNum(0.0, math.log(1e8))
  } yield x * math.exp(y)

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

  property("toString") =
    Interval.Empty.toString == "Interval.Empty" &&
    Interval.Whole.toString == "Interval.Whole" &&
    Interval(2, 3).toString == "Interval(2.0, 3.0)"

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

  property("contains(value, tolerance)") = Prop.forAll {
    (interval: Interval) => {
      val randomTolerance: Gen[Double] = for {
        candidateTolerance <- Gen.chooseNum(-1e-3, 1e-3)
        if interval.lowerBound - candidateTolerance <= interval.upperBound + candidateTolerance
      } yield candidateTolerance
      Prop.forAll(randomTolerance) {
        (tolerance: Double) => {
          val expanded = Interval(interval.lowerBound - tolerance, interval.upperBound + tolerance)
          Prop.forAll(valueWithin(expanded)) {
            (value: Double) => {
              interval.contains(value, tolerance)
            }
          }
        }
      }
    }
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
      interval = Interval.hull(firstValue, secondValue)
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

  def equalSingletons(interval: Interval, value: Double): Boolean = {
    (interval.isEmpty && value.isNaN) ||
    (interval.lowerBound == value && interval.upperBound == value)
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
          val yValue = scalarFunction(xInterval.lowerBound)
          equalSingletons(yInterval, yValue): Prop
        } else {
          Prop.forAll(evaluateWithin(xInterval.intersection(domain), scalarFunction)) {
            (yValue: Double) => {
              s"xInterval: $xInterval, yInterval: $yInterval, yValue: $yValue" |:
                yInterval.contains(yValue, 2 * Interval.ulp(yInterval).max(math.ulp(1.0)))
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
          val zValue = scalarFunction(xInterval.lowerBound, yValue)
          equalSingletons(zInterval, zValue): Prop
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
              tag |: zInterval.contains(zValue, 2 * Interval.ulp(zInterval).max(math.ulp(1.0)))
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
    yDomain: Interval = Interval.Whole) = {

    Prop.forAll(testIntervalForDomain(xDomain), testIntervalForDomain(yDomain)) {
      (xInterval: Interval, yInterval: Interval) => {
        val zInterval = intervalFunction(xInterval, yInterval)
        if (xInterval.isEmpty || yInterval.isEmpty) {
          zInterval.isEmpty:  Prop
        } else if (xInterval.isSingleton && yInterval.isSingleton) {
          val zValue = scalarFunction(xInterval.lowerBound, yInterval.lowerBound)
          equalSingletons(zInterval, zValue): Prop
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

              tag |: zInterval.contains(zValue, 2 * Interval.ulp(zInterval).max(math.ulp(1.0)))
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

  property("hull(firstValue, secondValue)") = Prop.forAll(randomDouble, randomDouble) {
    (firstValue: Double, secondValue: Double) => {
      val interval = Interval.hull(firstValue, secondValue)
      if (firstValue <= secondValue) {
        interval.lowerBound == firstValue && interval.upperBound == secondValue
      } else {
        interval.lowerBound == secondValue && interval.upperBound == firstValue
      }
    }
  }

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
