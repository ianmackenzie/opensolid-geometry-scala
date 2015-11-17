package org.opensolid.core

import org.scalacheck._

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
    Gen.listOfN[Double](count, randomDouble).map(list => list.sorted).suchThat(_.length == count)
}
