package org.opensolid.core

import org.opensolid.core.IntervalGenerators._
import org.scalacheck._
import org.scalacheck.Prop.BooleanOperators

object IntervalProperties extends Properties("Interval") {
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

  property("contains(that, tolerance)") = Prop.forAll(sortedValues(4)) {
    case first :: second :: third :: fourth :: Nil => {
      val minTolerance = (second - first).min(fourth - third)
      val maxTolerance = (second - first).max(fourth - third)
      Interval(first, fourth).contains(Interval(second, third), -minTolerance + 1e-3) &&
        Interval(second, third).contains(Interval(first, fourth), maxTolerance + 1e-3) &&
        !Interval(second, third).contains(Interval(first, fourth), maxTolerance - 1e-3) &&
        !Interval(first, fourth).contains(Interval(second, third), -minTolerance - 1e-3)
    }
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

  property("overlaps(that, tolerance)") = Prop.forAll(sortedValues(4)) {
    case first :: second :: third :: fourth :: Nil => {
      val tolerance = third - second
      Interval(first, third).overlaps(Interval(second, fourth), -tolerance + 1e-3) &&
        Interval(second, fourth).overlaps(Interval(first, third), -tolerance + 1e-3) &&
        !Interval(first, third).overlaps(Interval(second, fourth), -tolerance - 1e-3) &&
        !Interval(second, fourth).overlaps(Interval(first, third), -tolerance - 1e-3) &&
        Interval(first, second).overlaps(Interval(third, fourth), tolerance + 1e-3) &&
        Interval(third, fourth).overlaps(Interval(first, second), tolerance + 1e-3) &&
        !Interval(first, second).overlaps(Interval(third, fourth), tolerance - 1e-3) &&
        !Interval(third, fourth).overlaps(Interval(first, second), tolerance - 1e-3)
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
