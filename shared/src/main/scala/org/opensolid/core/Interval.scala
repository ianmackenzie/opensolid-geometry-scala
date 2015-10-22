package org.opensolid.core

import scala.util.Random

final case class Interval(val lowerBound: Double, val upperBound: Double) extends Bounded1d {
  def this(value: Double) = this(value, value)

  override def equals(other: Any): Boolean = other match {
    case that: Interval =>
      (this.lowerBound == that.lowerBound && this.upperBound == that.upperBound) ||
      (this.isEmpty && that.isEmpty)
    case _ => false
  }

  override def bounds: Interval = this

  def isEmpty: Boolean = lowerBound.isNaN && upperBound.isNaN;

  def width: Double = upperBound - lowerBound

  def interpolated(value: Double): Double = lowerBound + value * width

  def median: Double = interpolated(0.5)

  def randomValue(generator: Random = Random): Double = interpolated(generator.nextDouble())

  def unary_- : Interval = Interval(-upperBound, -lowerBound)

  def *(value: Double): Interval = {
    if (value >= 0.0) {
      Interval(value * lowerBound, value * upperBound)
    } else {
      Interval(value * upperBound, value * lowerBound)
    }
  }

  def squared = {
    if (isEmpty) {
      Interval.Empty
    } else if (lowerBound > 0.0) {
      Interval(lowerBound * lowerBound, upperBound * upperBound)
    } else if (upperBound < 0.0) {
      Interval(upperBound * upperBound, lowerBound * lowerBound)
    } else if (-lowerBound < upperBound) {
      Interval(0.0, upperBound * upperBound)
    } else {
      Interval(0.0, lowerBound * lowerBound)
    }
  }

  def isSingleton = lowerBound == upperBound

  def bisected: (Interval, Interval) = {
    val median = this.median
    (Interval(lowerBound, median), Interval(median, upperBound))
  }

  def hull(value: Double): Interval = {
    if (isEmpty) {
      Interval(value)
    } else {
      Interval(lowerBound.min(value), upperBound.max(value))
    }
  }

  def hull(that: Interval): Interval = {
    if (isEmpty) {
      that
    } else if (that.isEmpty) {
      this
    } else {
      Interval(lowerBound.min(that.lowerBound), upperBound.max(that.upperBound))
    }
  }

  def intersection(that: Interval): Interval = {
    if (isEmpty || that.isEmpty) {
      Interval.Empty
    } else {
      val lowerBound = this.lowerBound.max(that.lowerBound)
      val upperBound = this.upperBound.min(that.upperBound)
      if (lowerBound <= upperBound) Interval(lowerBound, upperBound) else Interval.Empty
    }
  }

  def contains(value: Double): Boolean = value >= lowerBound && value <= upperBound

  def contains(that: Interval): Boolean =
    that.lowerBound >= lowerBound && that.upperBound <= upperBound

  def overlaps(that: Interval): Boolean =
    that.lowerBound <= upperBound && that.upperBound >= lowerBound
}

object Interval {
  def apply(value: Double): Interval = new Interval(value)

  val Empty: Interval = new Interval(Double.NaN, Double.NaN)

  val Whole: Interval = new Interval(Double.NegativeInfinity, Double.PositiveInfinity)

  val Unit: Interval = new Interval(0.0, 1.0)
}
