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

import scala.math
import scala.util.Random

/** Represents a range of real numbers and allows mathematical operations on those ranges.
  *
  * Intervals support most of the same operations as floating-point numbers (sum, square root,
  * sine, logarithm etc.) as well as some specific to intervals (hull, intersection etc.). Mixed
  * operations such as the sum of an interval and a floating-point value are also supported and
  * result in an interval.
  *
  * In general, mathematical operations on intervals result in an interval that contains all
  * possible floating-point values that could result from applying the corresponding floating-point
  * mathematical operation to on any combination of values from the inputs. For instance, the
  * expression `Interval(1.0, 2.0) * Interval(-5.0, -3.0)` results in `Interval(-10.0, -3.0)` since
  * the lowest possible product of values taken from those two intervals is `2.0 * (-5.0) == -10.0`
  * and the highest possible product is `1.0 * (-3.0) == -3.0`.
  *
  * Note that not only the endpoints are considered - `Interval(0.0, math.Pi).sin` results in
  * `Interval(0.0, 1.0)` even though `math.sin(0.0)` and `math.sin(math.Pi)` are both zero, since
  * `math.sin(math.Pi / 2.0)` is 1.0 and `math.Pi / 2.0` is a possible value within
  * `Interval(0.0, math.Pi)`.
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
  * scala> c.midpoint
  * res4: Double = -2.5
  *
  * scala> c.width
  * res5: Double = 3.0
  * }}}
  */
final case class Interval(lowerBound: Double, upperBound: Double) extends Bounds[Interval]
  with Bounded[Interval] {

  /** Returns a tuple containing the lower and upper bounds of this interval. */
  def endpoints: (Double, Double) =
    (lowerBound, upperBound)

  override def equals(other: Any): Boolean = other match {
    case that: Interval =>
      (this.lowerBound == that.lowerBound && this.upperBound == that.upperBound) ||
      (this.isEmpty && that.isEmpty)
    case _ => false
  }

  override def bounds: Interval =
    this

  override def hashCode: Int =
    (lowerBound, upperBound).hashCode

  override def toString: String = {
    if (isEmpty) {
      "Interval.Empty"
    } else if (isWhole) {
      "Interval.Whole"
    } else {
      s"Interval($lowerBound, $upperBound)"
    }
  }

  /** Returns true if this is the empty interval (contains no values). Note that a singleton
    * interval (one with zero width) is not considered empty since it contains a single value.
    *
    * Equivalent to `this == Interval.Empty`.
    */
  def isEmpty: Boolean =
    lowerBound.isNaN && upperBound.isNaN

  /** Returns true if this interval represents the entire range of possible real values (from
    * negative infinity to positive infinity).
    *
    * Equivalent to `this == Interval.Whole`.
    */
  def isWhole: Boolean =
    lowerBound.isNegInfinity && upperBound.isPosInfinity

  def isZero(tolerance: Double): Boolean =
    lowerBound >= -tolerance && upperBound <= tolerance

  /** Returns the width of this interval (the difference between the upper and lower bounds). */
  def width: Double =
    upperBound - lowerBound

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
    */
  def interpolated(value: Double): Double = {
    val width = this.width
    if (width < Double.PositiveInfinity) {
      lowerBound + value * width
    } else if (width.isNaN || value.isNaN) {
      Double.NaN
    } else if (isWhole) {
      if (value <= 0.0) {
        Double.NegativeInfinity
      } else if (value >= 1.0) {
        Double.PositiveInfinity
      } else {
        Double.NaN
      }
    } else if (lowerBound.isInfinity) {
      if (value < 1.0) {
        Double.NegativeInfinity
      } else if (value > 1.0) {
        Double.PositiveInfinity
      } else {
        upperBound
      }
    } else {
      if (value < 0.0) {
        Double.NegativeInfinity
      } else if (value > 0.0) {
        Double.PositiveInfinity
      } else {
        lowerBound
      }
    }
  }

  /** Returns a value halfway between the lower and upper bounds of this interval. */
  def midpoint: Double =
    interpolated(0.5)

  /** Returns a random value within this interval. */
  def randomValue: Double =
    randomValue(Random)

  /** Returns a random value within this interval, using the provided generator. */
  def randomValue(generator: Random): Double =
    interpolated(generator.nextDouble)

  /** Returns true if this interval consists of a single value (the upper and lower bounds are
    * equal).
    */
  def isSingleton: Boolean =
    lowerBound == upperBound

  /** Returns a pair of intervals equal to this interval split into two halves.
    *
    * If this interval has finite width, then the split point is this interval's midpoint and the
    * two returned intervals are `Interval(lowerBound, midpoint)` and
    * `Interval(midpoint, upperBound)`. Otherwise, a set of heuristics is used to find a reasonable
    * split point.
    *
    * Examples:
    * {{{
    * scala> Interval(2.0, 3.0).bisected
    * res0: (org.opensolid.core.Interval, org.opensolid.core.Interval) =
    *   (Interval(2.0, 2.5),Interval(2.5, 3.0))
    *
    * scala> Interval.Whole.bisected
    * res1: (org.opensolid.core.Interval, org.opensolid.core.Interval) =
    *   (Interval(-Infinity, 0.0),Interval(0.0, Infinity))
    *
    * scala> Interval(0.0, Double.PositiveInfinity).bisected
    * res2: (org.opensolid.core.Interval, org.opensolid.core.Interval) =
    *   (Interval(0.0, 1.0),Interval(1.0, Infinity))
    *
    * scala> Interval(Double.NegativeInfinity, -10.0).bisected
    * res3: (org.opensolid.core.Interval, org.opensolid.core.Interval) =
    *   (Interval(-Infinity, -20.0),Interval(-20.0, -10.0))
    *
    * scala> Interval.Empty.bisected
    * res4: (org.opensolid.core.Interval, org.opensolid.core.Interval) =
    *   (Interval.Empty,Interval.Empty)
    * }}}
    */
  def bisected: (Interval, Interval) =
    if (isEmpty) {
      (Interval.Empty, Interval.Empty)
    } else {
      val mid =
        if (isWhole) {
          0.0
        } else if (lowerBound.isNegInfinity) {
          if (upperBound > 0.0) {
            0.0
          } else if (upperBound <= -0.5) {
            2.0 * upperBound
          } else {
            -1.0
          }
        } else if (upperBound.isPosInfinity) {
          if (lowerBound < 0.0) {
            0.0
          } else if (lowerBound >= 0.5) {
            2.0 * lowerBound
          } else {
            1.0
          }
        } else {
          this.midpoint
        }
      (Interval(lowerBound, mid), Interval(mid, upperBound))
    }

  def bisectedAt(value: Double): (Interval, Interval) =
    if (isEmpty || value.isNaN) {
      (Interval.Empty, Interval.Empty)
    } else if (value < lowerBound) {
      (Interval.Empty, this)
    } else if (value > upperBound) {
      (this, Interval.Empty)
    } else {
      (Interval(lowerBound, value), Interval(value, upperBound))
    }

  override def bisected(index: Int): (Interval, Interval) =
    bisected

  override def expandedBy(value: Double): Interval =
    if (isEmpty || value.isNaN || value.isNegInfinity) {
      Interval.Empty
    } else if (value.isPosInfinity) {
      if (lowerBound.isPosInfinity) this else Interval.Whole
    } else {
      val lowerBound = this.lowerBound - value
      val upperBound = this.upperBound + value
      if (lowerBound <= upperBound) Interval(lowerBound, upperBound) else Interval.Empty
    }

  /** Returns a new interval that contains both this interval and the given value. */
  def hull(value: Double): Interval =
    if (value < lowerBound) {
      Interval(value, upperBound)
    } else if (value > upperBound) {
      Interval(lowerBound, value)
    } else if (isEmpty) {
      Interval.singleton(value)
    } else {
      // value is NaN or inside interval
      this
    }

  /** Returns a new interval that contains both this interval and the given interval. */
  override def hull(that: Interval): Interval =
    if (isEmpty) {
      that
    } else if (that.isEmpty) {
      this
    } else {
      Interval(lowerBound.min(that.lowerBound), upperBound.max(that.upperBound))
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
  override def contains(that: Interval): Boolean =
    that.lowerBound >= this.lowerBound && that.upperBound <= this.upperBound

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
  override def overlaps(that: Interval): Boolean =
    that.lowerBound <= this.upperBound && that.upperBound >= this.lowerBound

  override def component(index: Int): Interval =
    this

  def unary_- : Interval =
    Interval(-upperBound, -lowerBound)

  def negated: Interval =
    -this

  def reciprocal: Interval =
    if (lowerBound > 0.0 || upperBound < 0.0) {
      Interval(1.0 / upperBound, 1.0 / lowerBound)
    } else if (lowerBound < 0.0 && upperBound == 0.0) {
      Interval(Double.NegativeInfinity, 1.0 / lowerBound)
    } else if (lowerBound == 0.0 && upperBound > 0.0) {
      Interval(1.0 / upperBound, Double.PositiveInfinity)
    } else if (isEmpty) {
      Interval.Empty
    } else {
      Interval.Whole
    }

  def +(value: Double): Interval =
    Interval.nondecreasing(lowerBound + value, upperBound + value)

  def plus(value: Double): Interval =
    this + value

  def +(that: Interval): Interval =
    Interval.nondecreasing(this.lowerBound + that.lowerBound, this.upperBound + that.upperBound)

  def plus(that: Interval): Interval =
    this + that

  def -(value: Double): Interval =
    Interval.nondecreasing(lowerBound - value, upperBound - value)

  def minus(value: Double): Interval =
    this - value

  def -(that: Interval): Interval =
    Interval.nondecreasing(this.lowerBound - that.upperBound, this.upperBound - that.lowerBound)

  def minus(that: Interval): Interval =
    this - that

  def *(value: Double): Interval =
    if (value > 0.0) {
      Interval.nondecreasing(lowerBound * value, upperBound * value)
    } else if (value < 0.0) {
      Interval.nondecreasing(upperBound * value, lowerBound * value)
    } else if (value == 0.0) {
      Interval.Zero
    } else {
      Interval.Empty
    }

  def times(value: Double): Interval =
    this * value

  def *(that: Interval): Interval =
    if (this == Interval.Zero && that == Interval.Whole) {
      Interval.Zero
    } else {
      (this * that.lowerBound).hull(this * that.upperBound)
    }

  def times(that: Interval): Interval =
    this * that

  def /(value: Double): Interval =
    if (isEmpty || value.isNaN) {
      Interval.Empty
    } else if (value.isInfinity) {
      if (lowerBound.isPosInfinity || upperBound.isNegInfinity) Interval.Empty else Interval.Zero
    } else if (value > 0.0) {
      Interval(lowerBound / value, upperBound / value)
    } else if (value < 0.0) {
      Interval(upperBound / value, lowerBound / value)
    } else if (value == 0.0) {
      if (lowerBound == 0.0 && upperBound == 0.0) {
        Interval.Empty
      } else if (lowerBound >= 0.0) {
        Interval.PositiveInfinity
      } else if (upperBound <= 0.0) {
        Interval.NegativeInfinity
      } else {
        Interval.Whole
      }
    } else {
      Interval.Empty
    }

  def dividedBy(value: Double): Interval =
    this / value

  def /(that: Interval): Interval =
    this * that.reciprocal

  def dividedBy(that: Interval): Interval =
    this / that

  def abs: Interval =
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

  def squared: Interval =
    if (isEmpty) {
      Interval.Empty
    } else if (lowerBound >= 0.0) {
      Interval(lowerBound * lowerBound, upperBound * upperBound)
    } else if (upperBound <= 0.0) {
      Interval(upperBound * upperBound, lowerBound * lowerBound)
    } else if (-lowerBound < upperBound) {
      Interval(0.0, upperBound * upperBound)
    } else {
      Interval(0.0, lowerBound * lowerBound)
    }

  def sqrt: Interval =
    if (isEmpty || upperBound < 0.0) {
      Interval.Empty
    } else {
      Interval(math.sqrt(lowerBound.max(0.0)), math.sqrt(upperBound))
    }

  def sin: Interval =
    if (isEmpty) {
      Interval.Empty
    } else if (isSingleton) {
      Interval.singleton(math.sin(lowerBound))
    } else {
      val (hasMin, hasMax) = (this - math.Pi / 2.0).cosHasMinMax
      if (hasMin && hasMax) {
        Interval.SinCosFullRange
      } else {
        val sinLower = math.sin(this.lowerBound)
        val sinUpper = math.sin(this.upperBound)
        val lowerBound = if (hasMin) -1.0 else sinLower.min(sinUpper)
        val upperBound = if (hasMax) 1.0 else sinLower.max(sinUpper)
        Interval(lowerBound, upperBound)
      }
    }

  def cos: Interval =
    if (isEmpty) {
      Interval.Empty
    } else if (isSingleton) {
      Interval.singleton(math.cos(lowerBound))
    } else {
      val (hasMin, hasMax) = cosHasMinMax
      if (hasMin && hasMax) {
        Interval.SinCosFullRange
      } else {
        val cosLower = math.cos(this.lowerBound)
        val cosUpper = math.cos(this.upperBound)
        val lowerBound = if (hasMin) -1.0 else cosLower.min(cosUpper)
        val upperBound = if (hasMax) 1.0 else cosLower.max(cosUpper)
        Interval(lowerBound, upperBound)
      }
    }

  private def cosHasMinMax: (Boolean, Boolean) = {
    val abs = this.abs
    if (abs.upperBound.isInfinity) {
      (true, true)
    } else {
      val width = abs.width
      val hasMin = (abs.upperBound + math.Pi) % (2 * math.Pi) <= width
      val hasMax = abs.upperBound % (2 * math.Pi) <= width
      (hasMin, hasMax)
    }
  }

  def tan: Interval = {
    val abs = this.abs
    if (abs.upperBound.isInfinity) {
      Interval.Whole
    } else {
      val hasSingularity = (abs.upperBound + math.Pi / 2.0) % math.Pi <= abs.width
      if (hasSingularity) Interval.Whole else Interval(math.tan(lowerBound), math.tan(upperBound))
    }
  }

  def asin: Interval =
    if (isEmpty || lowerBound > 1.0 || upperBound < -1.0) {
      Interval.Empty
    } else {
      Interval(math.asin(lowerBound.max(-1.0)), math.asin(upperBound.min(1.0)))
    }

  def acos: Interval =
    if (isEmpty || lowerBound > 1.0 || upperBound < -1.0) {
      Interval.Empty
    } else {
      Interval(math.acos(upperBound.min(1.0)), math.acos(lowerBound.max(-1.0)))
    }

  def atan: Interval =
    Interval(math.atan(lowerBound), math.atan(upperBound))

  def exp: Interval =
    Interval(math.exp(lowerBound), math.exp(upperBound))

  def log: Interval =
    if (isEmpty || upperBound < 0.0) {
      Interval.Empty
    } else {
      Interval(math.log(lowerBound.max(0.0)), math.log(upperBound))
    }

  def ulp: Double =
    math.ulp(lowerBound).max(math.ulp(upperBound))
}

object Interval {
  def fromEndpoints(endpoints: (Double, Double)): Interval = endpoints match {
    case (lowerBound, upperBound) => Interval(lowerBound, upperBound)
  }

  def singleton(value: Double): Interval =
    Interval(value, value)

  /** The empty interval (contains no values).
    *
    * This is returned in situations such as the intersection of two non-overlapping intervals, the
    * square root of an interval containing only negative values, or the sum of the empty interval
    * and any other interval.
    */
  val Empty: Interval = new Interval(Double.NaN, Double.NaN)

  val Whole: Interval = new Interval(Double.NegativeInfinity, Double.PositiveInfinity)

  val Unit: Interval = new Interval(0.0, 1.0)

  val Zero: Interval = new Interval(0.0, 0.0)

  val NegativeInfinity: Interval = Interval(Double.NegativeInfinity, Double.NegativeInfinity)

  val PositiveInfinity: Interval = Interval(Double.PositiveInfinity, Double.PositiveInfinity)

  val PositiveHalf: Interval = Interval(0.0, Double.PositiveInfinity)

  val NegativeHalf: Interval = Interval(Double.NegativeInfinity, 0.0)

  private[Interval] val SinCosFullRange = Interval(-1.0, 1.0)

  private[Interval] def nondecreasing(lowerBound: Double, upperBound: Double): Interval =
    if (lowerBound <= upperBound) {
      Interval(lowerBound, upperBound)
    } else if (!lowerBound.isNaN) {
      singleton(lowerBound)
    } else if (!upperBound.isNaN) {
      singleton(upperBound)
    } else {
      Empty
    }
}
