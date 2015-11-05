package org.opensolid.core

import scala.math
import scala.util.Random

/** Represents a range of real numbers and allows mathematical operations on those ranges.
  *
  * Examples:
  * {{{
  * scala> val a = Interval(2.0, 3.0)
  * a: org.opensolid.core.Interval = Interval(2.0,3.0)
  *
  * scala> val b = 2.0 * a
  * b: org.opensolid.core.Interval = Interval(4.0,6.0)
  *
  * scala> val c = a - b
  * c: org.opensolid.core.Interval = Interval(-4.0,-1.0)
  *
  * scala> c.contains(-2.0)
  * res0: Boolean = true
  *
  * scala> a.overlaps(b)
  * res1: Boolean = false
  *
  * scala> c.lowerBound
  * res2: Double = -4.0
  *
  * scala> c.upperBound
  * res3: Double = -1.0
  *
  * scala> c.median
  * res4: Double = -2.5
  *
  * scala> c.width
  * res5: Double = 3.0
  * }}}
  */
final case class Interval(val lowerBound: Double, val upperBound: Double) extends Bounded1d {
  def this(value: Double) = this(value, value)

  override def equals(other: Any): Boolean = other match {
    case that: Interval =>
      (this.lowerBound == that.lowerBound && this.upperBound == that.upperBound) ||
      (this.isEmpty && that.isEmpty)
    case _ => false
  }

  override def toString: String = {
    if (isEmpty) {
      "Interval.Empty"
    } else if (isWhole) {
      "Interval.Whole"
    } else {
      s"Interval($lowerBound, $upperBound)"
    }
  }

  /** Returns this interval (an interval is its own bounds). */
  override def bounds: Interval = this

  /** Returns true if this is the empty interval (contains no values). Note that a singleton
    * interval (one with zero width) is not considered empty since it contains a single value.
    *
    * Equivalent to `this == Interval.Empty`.
    */
  def isEmpty: Boolean = lowerBound.isNaN && upperBound.isNaN

  /** Returns true if this interval represents the entire range of possible real values (from
    * negative infinity to positive infinity).
    *
    * Equivalent to `this == Interval.Whole`.
    */
  def isWhole: Boolean = lowerBound.isNegInfinity && upperBound.isPosInfinity

  /** Returns the width of this interval (the difference between the upper and lower bounds). */
  def width: Double = upperBound - lowerBound

  /** Returns a value interpolated between the lower and upper bounds of this interval.
    *
    * Examples:
    * {{{
    * scala> val interval = Interval(2.0, 3.0)
    * interval: org.opensolid.core.Interval = Interval(2.0,3.0)
    * 
    * scala> interval.interpolated(0.0)
    * res0: Double = 2.0
    * 
    * scala> interval.interpolated(1.0)
    * res1: Double = 3.0
    * 
    * scala> interval.interpolated(0.5)
    * res2: Double = 2.5
    * 
    * scala> interval.interpolated(2.0)
    * res3: Double = 4.0
    * }}}
    *
    * Behaviour is undefined if the interval has infinite width (either the lower or upper bound is
    * infinite).
    */
  def interpolated(value: Double): Double = lowerBound + value * width

  /** Returns a value halfway between the lower and upper bounds of this interval. */
  def median: Double = interpolated(0.5)

  /** Returns a random value within this interval. */
  def randomValue: Double = randomValue(Random)

  /** Returns a random value within this interval, using the provided generator. */
  def randomValue(generator: Random): Double = interpolated(generator.nextDouble())

  /** Returns true if this interval consists of a single value (the upper and lower bounds are
    * equal).
    */
  def isSingleton = lowerBound == upperBound

  /** Returns a pair of intervals equal to this interval split into two halves.
    *
    * If this interval has finite width, then the split point is this interval's median and the two
    * returned intervals are `Interval(lowerBound, median)` and `Interval(median, upperBound)`.
    * Otherwise, a set of heuristics is used to find a reasonable split point.
    *
    * Examples:
    * {{{
    * scala> Interval(2.0, 3.0).bisected
    * res0: (org.opensolid.core.Interval, org.opensolid.core.Interval) = (Interval(2.0, 2.5),Interval(2.5, 3.0))
    * 
    * scala> Interval.Whole.bisected
    * res1: (org.opensolid.core.Interval, org.opensolid.core.Interval) = (Interval(-Infinity, 0.0),Interval(0.0, Infinity))
    * 
    * scala> Interval(0.0, Double.PositiveInfinity).bisected
    * res2: (org.opensolid.core.Interval, org.opensolid.core.Interval) = (Interval(0.0, 1.0),Interval(1.0, Infinity))
    * 
    * scala> Interval(Double.NegativeInfinity, -10.0).bisected
    * res3: (org.opensolid.core.Interval, org.opensolid.core.Interval) = (Interval(-Infinity, -20.0),Interval(-20.0, -10.0))
    * 
    * scala> Interval.Empty.bisected
    * res4: (org.opensolid.core.Interval, org.opensolid.core.Interval) = (Interval.Empty,Interval.Empty)
    * }}}
    */ 
  def bisected: (Interval, Interval) = {
    if (isEmpty) {
      (Interval.Empty, Interval.Empty)
    } else {
      val mid =
        if (isWhole) {
          0.0
        } else if (lowerBound.isNegInfinity) {
          if (upperBound > 0.0) {
            0.0
          } else if (upperBound < 0.0) {
            2.0 * upperBound
          } else { // upperBound == 0.0
            -1.0
          }
        } else if (upperBound.isPosInfinity) {
          if (lowerBound < 0.0) {
            0.0
          } else if (lowerBound > 0.0) {
            2.0 * lowerBound
          } else { // lowerBound == 0.0
            1.0
          }
        } else {
          this.median
        }
      (Interval(lowerBound, mid), Interval(mid, upperBound))
    }
  }

  /** Returns a new interval that contains both this interval and the given value. */
  def hull(value: Double): Interval = {
    if (isEmpty) {
      Interval(value)
    } else {
      Interval(lowerBound.min(value), upperBound.max(value))
    }
  }

  /** Returns a new interval that contains both this interval and the given interval. */
  def hull(that: Interval): Interval = {
    if (isEmpty) {
      that
    } else if (that.isEmpty) {
      this
    } else {
      Interval(lowerBound.min(that.lowerBound), upperBound.max(that.upperBound))
    }
  }

  /** Returns a new interval that contains all values common to both this interval and the given
    * interval. If the two intervals do not overlap at all then the empty interval is returned.
    */
  def intersection(that: Interval): Interval = {
    val lowerBound = this.lowerBound.max(that.lowerBound)
    val upperBound = this.upperBound.min(that.upperBound)
    if (lowerBound <= upperBound) Interval(lowerBound, upperBound) else Interval.Empty
  }

  /** Returns true if the given value is between the upper and lower bounds of this interval. */
  def contains(value: Double): Boolean =
    value >= lowerBound && value <= upperBound

  /** Returns true if this interval, expanded by the given tolerance, contains the given value.
    * `interval.contains(value, tolerance)` is equivalent to
    * `Interval(interval.lowerBound - tolerance, interval.upperBound + tolerance).contains(value)`.
    * Note that this means it is also possible to use a negative tolerance to make the containment
    * check more strict by not accepting values that are too close to this interval's endpoints.
    *
    * Examples:
    * {{{
    * scala> Interval(2, 3).contains(3.000001)
    * res0: Boolean = false
    * 
    * scala> Interval(2, 3).contains(3.000001, 0.001)
    * res1: Boolean = true
    * 
    * scala> Interval(2, 3).contains(2.000001)
    * res2: Boolean = true
    * 
    * scala> Interval(2, 3).contains(2.000001, -0.001)
    * res3: Boolean = false
    * }}}
    */
  def contains(value: Double, tolerance: Double): Boolean =
    value >= lowerBound - tolerance && value <= upperBound + tolerance

  /** Returns true if this interval fully contains the given interval (that is, this interval
    * contains both the upper and lower bounds of the given interval).
    *
    * Examples:
    * {{{
    * scala> Interval(5, 10).contains(Interval(7, 8))
    * res0: Boolean = true

    * scala> Interval(5, 10).contains(Interval(9, 10))
    * res1: Boolean = true

    * scala> Interval(5, 10).contains(Interval(8, 12))
    * res2: Boolean = false
    * }}}
    */
  def contains(that: Interval): Boolean =
    that.lowerBound >= this.lowerBound && that.upperBound <= this.upperBound

  /** Returns true if this interval, expanded by the given tolerance, fully contains the given
    * interval. `interval.contains(that, tolerance)` is equivalent to
    * `Interval(interval.lowerBound - tolerance, interval.upperBound + tolerance).contains(that)`.
    * Note that this means it is also possible to use a negative tolerance to make the containment
    * check more strict by not accepting intervals that come too close to to this interval's
    * endpoints.
    *
    * Examples:
    * {{{
    * scala> Interval(5, 10).contains(Interval(8, 10.000001))
    * res0: Boolean = false
    * 
    * scala> Interval(5, 10).contains(Interval(8, 10.000001), 0.001)
    * res1: Boolean = true
    * 
    * scala> Interval(5, 10).contains(Interval(8, 9.99999))
    * res2: Boolean = true
    * 
    * scala> Interval(5, 10).contains(Interval(8, 9.99999), -0.001)
    * res3: Boolean = false
    * }}}
    */
  def contains(that: Interval, tolerance: Double): Boolean =
    that.lowerBound >= this.lowerBound - tolerance && that.upperBound <= this.upperBound + tolerance

  /** Returns true if this interval overlaps the given interval.
    *
    * Examples:
    * {{{
    * scala> Interval(2, 4).overlaps(Interval(3, 5))
    * res0: Boolean = true
    * 
    * scala> Interval(5, 10).overlaps(Interval(6, 7))
    * res1: Boolean = true
    * 
    * scala> Interval(2, 4).overlaps(Interval(6, 8))
    * res2: Boolean = false
    * 
    * scala> Interval(0, 1).overlaps(Interval.Whole)
    * res3: Boolean = true
    * 
    * scala> Interval(0, 1).overlaps(Interval.Empty)
    * res4: Boolean = false
    * }}}
    */
  def overlaps(that: Interval): Boolean =
    that.lowerBound <= this.upperBound && that.upperBound >= this.lowerBound


  /** Returns true if this interval, expanded by the given tolerance, overlaps the given interval.
    * `interval.overlaps(that, tolerance)` is equivalent to
    * `Interval(interval.lowerBound - tolerance, interval.upperBound + tolerance).overlaps(that)`.
    * Note that this means it is also possible to use a negative tolerance to make the overlap
    * check more strict by not accepting intervals that only barely touch.
    *
    * Examples:
    * {{{
    * scala> Interval(2, 3).overlaps(Interval(3.000001, 4))
    * res0: Boolean = false
    * 
    * scala> Interval(2, 3).overlaps(Interval(3.000001, 4), 0.001)
    * res1: Boolean = true
    * 
    * scala> Interval(2, 3).overlaps(Interval(2.99999, 4))
    * res2: Boolean = true
    * 
    * scala> Interval(2, 3).overlaps(Interval(2.99999, 4), -0.001)
    * res3: Boolean = false
    * }}}
    */
  def overlaps(that: Interval, tolerance: Double): Boolean =
    that.lowerBound <= this.upperBound + tolerance && that.upperBound >= this.lowerBound - tolerance

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
    val reciprocal = 1.0 / value
    Interval.hull(lowerBound * reciprocal, upperBound * reciprocal)
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
}

object Interval {
  def apply(value: Double): Interval = new Interval(value)

  def hull(firstValue: Double, secondValue: Double): Interval =
    Interval(firstValue.min(secondValue), firstValue.max(secondValue))

  def sqrt(interval: Interval): Interval = {
    if (interval.isEmpty || interval.upperBound < 0.0) {
      Interval.Empty
    } else {
      Interval(math.sqrt(interval.lowerBound.max(0.0)), math.sqrt(interval.upperBound))
    }
  }

  def sin(interval: Interval): Interval = {
    if (interval.isEmpty) {
      Interval.Empty
    } else if (interval.isSingleton) {
      Interval(math.sin(interval.lowerBound))
    } else {
      _cos(interval - math.Pi / 2.0)
    }
  }

  def cos(interval: Interval): Interval = {
    if (interval.isEmpty) {
      Interval.Empty
    } else if (interval.isSingleton) {
      Interval(math.cos(interval.lowerBound))
    } else {
      _cos(interval)
    }
  }

  private[this] def _cos(interval: Interval): Interval = {
    val abs = interval.abs
    val width = abs.width
    val hasMin = (abs.upperBound + math.Pi) % (2 * math.Pi) <= width
    val hasMax = abs.upperBound % (2 * math.Pi) <= width
    if (hasMin && hasMax) {
      cosFullRange
    } else {
      val cosLower = math.cos(abs.lowerBound)
      val cosUpper = math.cos(abs.upperBound)
      val lowerBound = if (hasMin) -1.0 else cosLower.min(cosUpper)
      val upperBound = if (hasMax) 1.0 else cosLower.max(cosUpper)
      Interval(lowerBound, upperBound)
    }
  }

  private[this] val cosFullRange = Interval(-1.0, 1.0)

  def tan(interval: Interval): Interval = {
    val abs = interval.abs
    val hasSingularity = (abs.upperBound + math.Pi / 2.0) % math.Pi <= abs.width
    if (hasSingularity) {
      Interval.Whole
    } else {
      Interval(math.tan(interval.lowerBound), math.tan(interval.upperBound))
    }
  }

  def asin(interval: Interval): Interval = {
    if (interval.isEmpty || interval.lowerBound > 1.0 || interval.upperBound < -1.0) {
      Interval.Empty
    } else {
      Interval(math.asin(interval.lowerBound.max(-1.0)), math.asin(interval.upperBound.min(1.0)))
    }
  }

  def acos(interval: Interval): Interval = {
    if (interval.isEmpty || interval.lowerBound > 1.0 || interval.upperBound < -1.0) {
      Interval.Empty
    } else {
      Interval(math.acos(interval.upperBound.min(1.0)), math.acos(interval.lowerBound.max(-1.0)))
    }
  }

  def atan(interval: Interval): Interval =
    Interval(math.atan(interval.lowerBound), math.atan(interval.upperBound))

  def atan2(x: Interval, y:Interval): Interval = {
    if (y.isEmpty || x.isEmpty) {
      Interval.Empty
    } else if (x.lowerBound > 0.0) {
      Interval.atan(y / x)
    } else if (y.lowerBound > 0.0) {
      Interval.atan(-x / y) + math.Pi / 2.0
    } else if (y.upperBound < 0.0) {
      Interval.atan(-x / y) - math.Pi / 2.0
    } else {
      atan2FullRange
    }
  }

  private[this] val atan2FullRange = Interval(-math.Pi, math.Pi)

  def exp(interval: Interval): Interval = {
    if (interval.isEmpty) {
      Interval.Empty
    } else {
      Interval(math.exp(interval.lowerBound), math.exp(interval.upperBound))
    }
  }

  def log(interval: Interval): Interval = {
    if (interval.isEmpty || interval.upperBound < 0.0) {
      Interval.Empty
    } else {
      Interval(math.log(interval.lowerBound.max(0.0)), math.log(interval.upperBound))
    }
  }

  def ulp(interval: Interval): Double =
    math.ulp(interval.lowerBound).max(math.ulp(interval.upperBound))

  val Empty: Interval = new Interval(Double.NaN, Double.NaN)

  val Whole: Interval = new Interval(Double.NegativeInfinity, Double.PositiveInfinity)

  val Unit: Interval = new Interval(0.0, 1.0)

  val Zero: Interval = new Interval(0.0, 0.0)
}
