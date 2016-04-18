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

trait CurveFunction1d extends Function1[Double, Double] {
  def apply(interval: Interval): Interval

  def isZeroWithin(interval: Interval, tolerance: Double): Boolean = {
    val bounds = this(interval)
    if (bounds.isZero(tolerance)) {
      true
    } else if (bounds.isNonZero(tolerance)) {
      false
    } else {
      val midpoint = interval.midpoint
      if (interval.lowerBound < midpoint && midpoint < interval.upperBound) {
        isZeroWithin(Interval(interval.lowerBound, midpoint), tolerance) &&
        isZeroWithin(Interval(midpoint, interval.upperBound), tolerance)
      } else {
        this(interval.lowerBound).isZero(tolerance) &&
        this(interval.upperBound).isZero(tolerance)
      }
    }
  }

  def isNonZeroWithin(interval: Interval, tolerance: Double): Boolean = {
    val bounds = this(interval)
    if (bounds.isZero(tolerance)) {
      false
    } else if (bounds.isNonZero(tolerance)) {
      true
    } else {
      val midpoint = interval.midpoint
      if (interval.lowerBound < midpoint && midpoint < interval.upperBound) {
        isNonZeroWithin(Interval(interval.lowerBound, midpoint), tolerance) &&
        isNonZeroWithin(Interval(midpoint, interval.upperBound), tolerance)
      } else {
        this(interval.lowerBound).isNonZero(tolerance) &&
        this(interval.upperBound).isNonZero(tolerance)
      }
    }
  }

  def firstRootWithin(interval: Interval): Option[Double] =
    if (this(interval).contains(0.0)) {
      val xLow = interval.lowerBound
      val xMid = interval.midpoint
      val xHigh = interval.upperBound
      if (xLow < xMid && xMid < xHigh) {
        firstRootWithin(Interval(xLow, xMid)).orElse(firstRootWithin(Interval(xMid, xHigh)))
      } else {
        if (this(xLow).abs <= this(xHigh).abs) Some(xLow) else Some(xHigh)
      }
    } else {
      None
    }

  def bisectionPointWithin(interval: Interval, tolerance: Double): Option[Double] = {
    def recurseWithBias(interval: Interval, bias: Int): Option[Double] = {
      val midpoint = interval.midpoint
      if (midpoint <= interval.lowerBound || midpoint >= interval.upperBound) {
        // Cannot bisect - interval is too small
        None
      } else if (this(midpoint).abs > tolerance) {
        // Midpoint is a valid bisection point
        Some(midpoint)
      } else if (this(interval).isZero(tolerance)) {
        // Entire interval is zero to within tolerance - no valid bisection point
        None
      } else {
        // Recurse into left and right halves, biasing towards the global midpoint
        def recurseLeft(bias: Int): Option[Double] =
          recurseWithBias(Interval(interval.lowerBound, midpoint), bias)
        def recurseRight(bias: Int): Option[Double] =
          recurseWithBias(Interval(midpoint, interval.upperBound), bias)
        bias match {
          case -1 =>
            recurseLeft(bias).orElse(recurseRight(bias))
          case 1 =>
            recurseRight(bias).orElse(recurseLeft(bias))
          case _ =>
            recurseLeft(1).orElse(recurseRight(-1))
        }
      }
    }
    recurseWithBias(interval, 0)
  }
}

object CurveFunction1d {
  def compile(expression: Expression1d[CurveParameter]): CurveFunction1d = {
    val compiler = new ExpressionCompiler(1)
    val resultIndex = compiler.evaluate(expression)
    val arrayOperations = compiler.arrayOperations.toArray
    val arraySize = compiler.arraySize

    new CurveFunction1d {
      override def apply(parameterValue: Double): Double = {
        val array = Array.ofDim[Double](arraySize)
        array(0) = parameterValue
        arrayOperations.foreach(_.execute(array))
        array(resultIndex)
      }

      override def apply(parameterBounds: Interval): Interval = {
        val array = Array.ofDim[Interval](arraySize)
        array(0) = parameterBounds
        arrayOperations.foreach(_.execute(array))
        array(resultIndex)
      }
    }
  }
}
