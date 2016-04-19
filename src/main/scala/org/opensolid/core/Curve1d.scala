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
import scala.util.Try

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

  def roots(tolerance: Double, maxRootOrder: Int = 2): Try[List[Root]] = Try {
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
      val derivativeOrders = 1 to maxDerivativeOrder
      for (order <- derivativeOrders) {
        derivativeExpression = derivativeExpression.derivative(CurveParameter)
        derivatives(order) = CurveFunction1d.compile(derivativeExpression)
      }

      // Check for any infinities
      if (!function.isFiniteWithin(domain)) {
        throw GeometricException.InfiniteFunctionValue
      }
      derivativeOrders.find(!derivatives(_).isFiniteWithin(domain)) match {
        case Some(order) =>
          throw GeometricException.InfiniteDerivativeValue(order)
        case None =>
          ()
      }

      // Calculate tolerances for each derivative order: tolerance(n) = n! * tolerance / width^n
      val tolerances = Array.ofDim[Double](derivatives.size)
      tolerances(0) = tolerance
      derivativeOrders.foreach(n => tolerances(n) = n * tolerances(n - 1) / domain.width)
      val roundoff = tolerance / 2

      // Use consistent resolution for whole domain (e.g., don't bisect to extremely small values
      // near zero)
      val resolution = domain.ulp

      def evaluateAllWithin(xInterval: Interval): Array[Interval] = {
        val results = Array.ofDim[Interval](derivatives.size)
        results(maxDerivativeOrder) = derivatives(maxDerivativeOrder)(xInterval)
        for (order <- maxDerivativeOrder - 1 to 0 by -1) {
          val derivative = derivatives(order)
          val nextDerivativeBounds = results(order + 1)
          results(order) =
            if (!nextDerivativeBounds.contains(0.0)) {
              derivative(xInterval.lowerBound).hull(derivative(xInterval.upperBound))
            } else {
              derivative(xInterval)
            }
        }
        results
      }

      def rootAt(x: Double, order: Int, tail: List[Root]): List[Root] =
        Root(x, order, derivatives(order + 1)(x).signum) :: tail

      def solveMonotonic(xInterval: Interval, order: Int, tail: List[Root]): List[Root] =
        derivatives(order).firstRootWithin(xInterval) match {
          case Some(x) =>
            if (function(x).abs < tolerance) {
              rootAt(x, order, tail)
            } else if (order > 0) {
              val (leftInterval, rightInterval) = xInterval.bisectedAt(x)
              val rightRoots = solveMonotonic(rightInterval, order - 1, tail)
              solveMonotonic(leftInterval, order - 1, rightRoots)
            } else {
              tail
            }
          case None =>
            if (order > 0) solveMonotonic(xInterval, order - 1, tail) else tail
        }

      def solveWithinTolerance(xInterval: Interval, tail: List[Root]): List[Root] = {
        val nonZeroOrder =
          derivativeOrders
            .find(order => derivatives(order).isNonZeroWithin(xInterval, tolerances(order)))
        nonZeroOrder match {
          case Some(order) =>
            derivatives(order - 1).firstRootWithin(xInterval) match {
              case Some(x) =>
                rootAt(x, order - 1, tail)
              case None =>
                if (function(xInterval.lowerBound).abs <= function(xInterval.upperBound).abs) {
                  rootAt(xInterval.lowerBound, order - 1, tail)
                } else {
                  rootAt(xInterval.upperBound, order - 1, tail)
                }
            }
          case None =>
            throw GeometricException.NoNonZeroDerivative
        }
      }

      def rootsWithin(xInterval: Interval, tail: List[Root]): List[Root] = {
        val derivativeBounds = evaluateAllWithin(xInterval)
        val functionBounds = derivativeBounds(0)
        if (functionBounds.isEmpty || functionBounds.isNonZero(roundoff)) {
          tail
        } else if (functionBounds.isZero(tolerance)) {
          solveWithinTolerance(xInterval, tail)
        } else {
          val nonZeroOrder =
            derivativeOrders.find(order => derivativeBounds(order).isNonZero(tolerances(order)))
          nonZeroOrder match {
            case Some(order) =>
              solveMonotonic(xInterval, order - 1, tail)
            case None => {
              function.bisectionPointWithin(xInterval, roundoff) match {
                case Some(bisectionValue) => {
                  val (leftInterval, rightInterval) = xInterval.bisectedAt(bisectionValue)
                  rootsWithin(leftInterval, rootsWithin(rightInterval, tail))
                }
                case None =>
                  if (function.isZeroWithin(xInterval, tolerance)) {
                    solveWithinTolerance(xInterval, tail)
                  } else {
                    throw GeometricException.FunctionDoesNotConverge
                  }
              }
            }
          }
        }
      }

      rootsWithin(domain, List.empty[Root])
    }
  }
}

object Curve1d {
  case class Root(x: Double, order: Int, sign: Int)
}
