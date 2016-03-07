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

trait Curve1d extends Bounded[Interval] {
  import Curve1d._

  def parameterized: ParametricCurve1d

  def parameterizedBy(
    expression: ScalarExpression[CurveParameter],
    domain: Interval
  ): ParametricCurve1d =
    new ParametricCurve1d(expression, domain) {
      override def bounds: Interval =
        Curve1d.this.bounds
    }

  // Return information about root order and direction as well as X value?
  def roots(tolerance: Double, maxOrder: Int = 2): List[Double] = {
    val parameterizedCurve = this.parameterized
    val expression = parameterizedCurve.expression
    val domain = parameterizedCurve.domain
    val derivatives = Array.iterate(expression, maxOrder + 2)(_.derivative(CurveParameter))
    val compiled = derivatives.map(ScalarExpression.compileCurve(_))
    val tolerances = derivativeTolerances(tolerance, maxOrder, domain)
    val compiledExpression = compiled(0)
    val leftXValue = domain.lowerBound
    val leftYValue = compiledExpression.evaluate(leftXValue)
    val leftIsRoot = leftXValue.isZero(tolerance)
    val leftRadius =
      if (leftIsRoot) radiusOfInfluence(leftXValue, compiled, tolerance) else 0.0
    if (leftIsRoot && leftRadius >= domain.width) {
      // Curve is identically zero
      List.empty[Double]
    } else {
      val (leftRoot, leftBound) = if (leftIsRoot) {
        (List(leftXValue), leftXValue + leftRadius)
      } else {
        (List.empty[Double], leftXValue)
      }
      val rightXValue = domain.upperBound
      val rightYValue = compiledExpression.evaluate(rightXValue)
      val rightIsRoot = rightXValue.isZero(tolerance)
      val (rightRoot, rightBound) = if (rightIsRoot) {
        (List(rightXValue), rightXValue - radiusOfInfluence(rightXValue, compiled, tolerance))
      } else {
        (List.empty[Double], rightXValue)
      }
      // Search within Interval(leftBound, rightBound) for highest-order roots
      // return leftRoot ++ midRoots ++ rightRoot
      ???
    }
  }
}

object Curve1d {
  private[Curve1d] def derivativeTolerances(
    tolerance: Double,
    maxOrder: Int,
    domain: Interval
  ): Array[Double] = {
    val tolerances = Array.ofDim[Double](maxOrder + 2)
    tolerances(0) = tolerance
    val inverseWidth = 1.0 / domain.width
    for (i <- 1 to maxOrder + 1) {
      tolerances(i) = i * tolerances(i - 1) * inverseWidth
    }
    tolerances
  }

  private[Curve1d] def radiusOfInfluence(
    xValue: Double,
    derivatives: Array[ScalarExpression.CompiledCurve],
    tolerance: Double
  ): Double = {
    def radiusOfInfluence(order: Int): Double = {
      val derivativeValue = derivatives(order).evaluate(xValue)
      val ratio = (tolerance / derivativeValue).abs
      order match {
        case 1 =>
          ratio
        case 2 =>
          math.sqrt(2 * ratio)
        case 3 =>
          math.cbrt(6 * ratio)
        case 4 =>
          math.sqrt(math.sqrt(24 * ratio))
        case _ =>
          throw new MatchError(s"Radius of influence not implemented for order $order")
      }
    }
    (1 to derivatives.size - 1).map(radiusOfInfluence(_)).max
  }

  private[Curve1d] def rootAt(
    xValue: Double,
    expression: ScalarExpression.CompiledCurve,
    tolerance: Double
  ): Option[Double] =
    if (expression.evaluate(xValue).isZero(tolerance)) Some(xValue) else None

  private[Curve1d] def rootsWithin(
    xInterval: Interval,
    derivatives: Array[ScalarExpression.CompiledCurve],
    tolerance: Double
  ): Iterable[Double] = {
    val expression = derivatives(0)
    val yInterval = expression.evaluateBounds(xInterval)
    if (yInterval.contains(0.0, tolerance)) {
      // Y interval includes zero (to within tolerance) - this X interval may contain a root
      // Attempt to find a derivative guaranteed not to be zero within this X interval
      val nonZeroIndex = derivatives.indexWhere(!_.evaluateBounds(xInterval).contains(0.0), 1)
      if (nonZeroIndex >= 0) {
        // Found guaranteed non-zero derivative
        safeRootsWithin(xInterval, derivatives, tolerance, nonZeroIndex)
      } else {
        // Could not find a guaranteed non-zero derivative in this X interval - attempt to bisect
        val xMin = xInterval.lowerBound
        val xMid = xInterval.midpoint
        val xMax = xInterval.upperBound
        if (xMin < xMid && xMid < xMax) {
          // X interval is large enough to bisect - solve recursively
          val leftInterval = Interval(xMin, xMid)
          val rightInterval = Interval(xMid, xMax)
          val leftRoots = rootsWithin(leftInterval, derivatives, tolerance)
          val midRoot = if (expression.evaluate(xMid) == 0.0) Some(xMid) else None
          val rightRoots = rootsWithin(rightInterval, derivatives, tolerance)
          leftRoots ++ midRoot ++ rightRoots
        } else {
          // Cannot bisect any further - see if we have found a root by bisection
          rootWithinEpsilon(xInterval, expression)
        }
      }
    } else {
      // Y interval does not include zero - guaranteed no root in this X interval
      None
    }
  }

  private[this] def rootWithinEpsilon(
    xInterval: Interval,
    expression: ScalarExpression.CompiledCurve
  ): Option[Double] = {
    val x0 = xInterval.lowerBound
    val x1 = xInterval.upperBound
    val y0 = expression.evaluate(x0)
    val y1 = expression.evaluate(x1)
    // If the Y values strictly bracket 0.0, choose the X value closest to the root (the one
    // corresponding to the Y value of smallest magnitude)
    if (y0 < 0.0 && y1 > 0.0) {
      Some(if (-y0 < y1) x0 else x1)
    } else if (y0 > 0.0 && y1 < 0.0) {
      Some(if (y0 < -y1) x0 else x1)
    } else {
      None
    }
  }

  private[this] def safeRootsWithin(
    xInterval: Interval,
    derivatives: Array[ScalarExpression.CompiledCurve],
    tolerance: Double,
    nonZeroIndex: Int
  ): Iterable[Double] = {
    val nonZeroDerivative = derivatives(nonZeroIndex)
    val derivativeToSolve = derivatives(nonZeroIndex - 1)
    val expression = derivatives(0)
    val slopeIsPositive =
      derivativeToSolve.evaluate(xInterval.upperBound) >=
      derivativeToSolve.evaluate(xInterval.lowerBound)
    val derivativeRoot =
      singleRootWithin(xInterval, derivativeToSolve, nonZeroDerivative, slopeIsPositive)
    val midRoot = derivativeRoot.flatMap(rootAt(_, expression, tolerance))
    if (nonZeroIndex > 1) {
      //val leftInterval = Interval(xInterval.lowerBound, )
      //val leftRoots = safeRootsWithin(Interval(xInterval.lowerBound))
      ???
    } else {
      midRoot
    }
  }

  private[this] def singleRootWithin(
    interval: Interval,
    expression: ScalarExpression.CompiledCurve,
    derivative: ScalarExpression.CompiledCurve,
    slopeIsPositive: Boolean
  ): Option[Double] = {
    // Try Newton-Raphson iteration first
    Numerics.newtonRaphson(expression.evaluate, derivative.evaluate, interval).orElse({
      // Newton-Raphson did not converge, so bisect
      val xMin = interval.lowerBound
      val xMid = interval.midpoint
      val xMax = interval.upperBound
      val yMid = expression.evaluate(xMid)
      if (xMin < xMid && xMid < xMax) {
        // Interval was small enough to bisect
        if (yMid == 0.0) {
          // Root happens to be exactly at the midpoint
          Some(xMid)
        } else if ((yMid > 0.0) == slopeIsPositive) {
          // Root is to the left of the midpoint
          singleRootWithin(Interval(xMin, xMid), expression, derivative, slopeIsPositive)
        } else {
          // Root is to the right of the midpoint
          singleRootWithin(Interval(xMid, xMax), expression, derivative, slopeIsPositive)
        }
      } else {
        // Interval was too small to bisect further - check if we have found a root by bisection
        rootWithinEpsilon(interval, expression)
      }
    })
  }
}
