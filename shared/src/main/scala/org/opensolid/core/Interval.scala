package org.opensolid.core

import scala.math
import scala.util.Random

final case class Interval(val lowerBound: Double, val upperBound: Double) extends Bounded1d {
  def this(value: Double) = this(value, value)

  override def equals(other: Any): Boolean = other match {
    case that: Interval =>
      (this.lowerBound == that.lowerBound && this.upperBound == that.upperBound) ||
      (this.isEmpty && that.isEmpty)
    case value: Double => this.lowerBound == value && this.upperBound == value
    case value: Float => this.lowerBound == value && this.upperBound == value
    case value: Long => this.lowerBound == value && this.upperBound == value
    case value: Int => this.lowerBound == value && this.upperBound == value
    case _ => false
  }

  override def bounds: Interval = this

  def isEmpty: Boolean = lowerBound.isNaN && upperBound.isNaN;

  def width: Double = upperBound - lowerBound

  def interpolated(value: Double): Double = lowerBound + value * width

  def median: Double = interpolated(0.5)

  def randomValue(generator: Random = Random): Double = interpolated(generator.nextDouble())

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

  def unary_- : Interval = Interval(-upperBound, -lowerBound)

  def +(value: Double): Interval = Interval(lowerBound + value, upperBound + value)

  def +(that: Interval): Interval =
    Interval(lowerBound + that.lowerBound, upperBound + that.upperBound)

  def -(value: Double): Interval = Interval(lowerBound - value, upperBound - value)

  def -(that: Interval): Interval =
    Interval(lowerBound - that.upperBound, upperBound - that.lowerBound)

  def *(value: Double): Interval = {
    if (value >= 0.0) {
      Interval(value * lowerBound, value * upperBound)
    } else {
      Interval(value * upperBound, value * lowerBound)
    }
  }

  def *(that: Interval): Interval = {
    val ll = lowerBound * that.lowerBound
    val lu = lowerBound * that.upperBound
    val ul = upperBound * that.lowerBound
    val uu = upperBound * that.upperBound
    Interval(ll.min(lu).min(ul).min(uu), ll.max(lu).max(ul).max(uu))
  }

  def /(value: Double): Interval = {
    if (isEmpty) {
      Interval.Empty
    } else if (value > 0.0) {
      val reciprocal = 1.0 / value
      Interval(lowerBound * reciprocal, upperBound * reciprocal)
    } else if (value < 0.0) {
      val reciprocal = 1.0 / value
      Interval(upperBound * reciprocal, lowerBound * reciprocal)
    } else {
      Interval.Whole
    }
  }

  def /(that: Interval): Interval = {
    if (isEmpty || that.isEmpty) {
      Interval.Empty
    } else if (that.lowerBound > 0.0 || that.upperBound < 0.0) {
      val reciprocal = Interval(1.0 / that.upperBound, 1.0 / that.lowerBound)
      this * reciprocal
    } else if (this == 0.0) {
      Interval(0.0)
    } else {
      Interval.Whole
    }
  }

  def abs: Interval = {
    if (isEmpty) {
      Interval.Empty
    } else if (lowerBound >= 0.0) {
      this
    } else if (upperBound <= 0.0) {
      -this
    } else if (-lowerBound < upperBound) {
      Interval(0.0, upperBound)
    } else {
      Interval(0.0, -lowerBound)
    }
  }

  def squared: Interval = {
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

  def sqrt: Interval = {
    if (isEmpty || upperBound < 0.0) {
      Interval.Empty
    } else {
      Interval(math.sqrt(lowerBound), math.sqrt(upperBound))
    }
  }
}

object Interval {
  def apply(value: Double): Interval = new Interval(value)

  val Empty: Interval = new Interval(Double.NaN, Double.NaN)

  val Whole: Interval = new Interval(Double.NegativeInfinity, Double.PositiveInfinity)

  val Unit: Interval = new Interval(0.0, 1.0)
}
