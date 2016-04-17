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

import java.lang.Math
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.math

trait Curve1d extends Bounded[Interval] {
  import Curve1d._

  def parameterized: ParametricCurve1d

  def parameterizedBy(
    expression: Expression1d[CurveParameter],
    domain: Interval
  ): ParametricCurve1d =
    new ParametricCurve1d(expression, domain) {
      override def bounds: Interval =
        Curve1d.this.bounds
    }

  def roots(tolerance: Double, maxRootOrder: Int = 2): List[Root] =
    if (maxRootOrder < 0) {
      List.empty[Root]
    } else {
      val parameterized = this.parameterized
      val expression = parameterized.expression
      val domain = parameterized.domain
      val function = parameterized.function

      // Compile all necessary derivative functions
      val maxDerivativeOrder = maxRootOrder + 1
      val derivatives = Array.ofDim[CurveFunction1d](maxDerivativeOrder + 1)
      derivatives(0) = function
      var derivativeExpression = expression
      for (order <- 1 to maxDerivativeOrder) {
        derivativeExpression = derivativeExpression.derivative(CurveParameter)
        derivatives(order) = CurveFunction1d.compile(derivativeExpression)
      }

      // Calculate tolerances for each derivative order: tolerance(n) = n! * tolerance / width^n
      val tolerances = Array.ofDim[Double](derivatives.size)
      tolerances(0) = tolerance
      (1 to maxDerivativeOrder).foreach(n => tolerances(n) = n * tolerances(n - 1) / domain.width)

      // Use consistent resolution for whole domain (e.g., don't bisect to extremely small values
      // near zero)
      val resolution = domain.ulp

      def nonZeroBisectionPoint(
        function: CurveFunction1d,
        xInterval: Interval,
        tolerance: Double
      ): Option[Double] =
        interpolationValues.view
          .map(interpolationValue => xInterval.interpolated(interpolationValue))
          .find(x => function(x).abs > tolerance)

      def solveMonotonic(xInterval: Interval, order: Int, tail: List[Root]): List[Root] = {
        val monotonicFunction = derivatives(order)
        val nonZeroDerivative = derivatives(order + 1)
        val y1 = monotonicFunction(xInterval.lowerBound)
        val y2 = monotonicFunction(xInterval.upperBound)
        if ((y1 > roundoff && y2 > roundoff) || (y1 < -roundoff && y2 < -roundoff)) {
          if (order > 0) solveMonotonic(xInterval, order - 1, tail) else tail
        } else {
          val root =
            Numerics.newtonRaphson(monotonicFunction, nonZeroDerivative, xInterval, tolerance)
          root match {
            case Some(x) => {
              if (function(x).abs < tolerance) {
                Root(x, order, nonZeroDerivative(x).signum) :: tail
              } else {
                val rightInterval = Interval(x, xInterval.upperBound)
                val rightRoots = solveMonotonic(rightInterval, order - 1, tail)
                val leftInterval = Interval(xInterval.lowerBound, x)
                solveMonotonic(leftInterval, order - 1, rightRoots)
              }
            }
            case None => {
              ???
            }
          }
        }
      }

      def evaluateWithin(xInterval: Interval): Array[Interval] = {
        val results = Array.ofDim[Interval](derivatives.size)
        results(maxDerivativeOrder) = derivatives(maxDerivativeOrder)(xInterval)
        for (order <- maxDerivativeOrder - 1 to 0 by -1) {
          val derivative = derivatives(order)
          val nextDerivativeBounds = results(order + 1)
          results(order) =
            if (nextDerivativeBounds.isFinite && !nextDerivativeBounds.contains(0.0)) {
              derivative(xInterval.lowerBound).hull(derivative(xInterval.upperBound))
            } else {
              derivative(xInterval)
            }
        }
        results
      }

      def rootsWithin(xInterval: Interval, tail: List[Root]): List[Root] = {
        val derivativeBounds = evaluateWithin(xInterval)
        val functionBounds = derivativeBounds(0)
        if (functionBounds.isEmpty || functionBounds.isNonZero(roundoff)) {
          tail
        } else {
          val nonZeroOrder =
            (1 to derivatives.size)
              .find(order => derivativeBounds(order).isNonZero(tolerances(order)))
          nonZeroOrder match {
            case Some(order) =>
              solveMonotonic(xInterval, order - 1, tail)
            case None => {
              ???
            }
          }
        }
      }

      rootsWithin(domain, List.empty[Root])
    }
}

object Curve1d {
  case class Root(x: Double, order: Int, sign: Int)

  private[Curve1d] val interpolationValues =
    List(
      0.5,
      math.sqrt(2.0) - 1,
      2 - math.sqrt(2.0),
      3.0 - math.E,
      math.E - 2.0,
      math.Pi - 3.0,
      4.0 - math.Pi
    )
}
