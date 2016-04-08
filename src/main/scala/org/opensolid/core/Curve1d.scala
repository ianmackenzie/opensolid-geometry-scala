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

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
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

  def zeros(tolerance: Double, maxRootOrder: Int = 2): Zeros = {
    val parameterizedCurve = this.parameterized
    val expression = parameterizedCurve.expression
    val domain = parameterizedCurve.domain
    val width = domain.width
    val derivativeSet = DerivativeSet(expression, domain, tolerance, maxRootOrder)
    val leftXValue = domain.lowerBound
    val leftIsZero = derivativeSet.isZeroAt(leftXValue)
    val leftRoot = if (leftIsZero) derivativeSet.rootAt(leftXValue) else None
    if (leftIsZero && leftRoot.isEmpty) {
      EntireCurve
    } else {
      val rightXValue = domain.upperBound
      val rightIsZero = derivativeSet.isZeroAt(rightXValue)
      val rightRoot = if (rightIsZero) derivativeSet.rootAt(rightXValue) else None
      val leftBound = leftRoot match {
        case None =>
          leftXValue
        case Some(root) => {
          val derivative = derivativeSet.derivatives(root.order + 1)
          @tailrec
          def leftRadius(testRadius: Double): Double = {
            val leftInterval = Interval(leftXValue, leftXValue + testRadius)
            if (derivative.evaluateBounds(leftInterval).contains(0.0)) {
              leftRadius(testRadius / 2.0)
            } else {
              testRadius
            }
          }
          leftXValue + leftRadius(width)
        }
      }
      val rightBound = rightRoot match {
        case None =>
          rightXValue
        case Some(root) => {
          val derivative = derivativeSet.derivatives(root.order + 1)
          @tailrec
          def rightRadius(testRadius: Double): Double = {
            val rightInterval = Interval(rightXValue - testRadius, rightXValue)
            if (derivative.evaluateBounds(rightInterval).contains(0.0)) {
              rightRadius(testRadius / 2.0)
            } else {
              testRadius
            }
          }
          rightXValue + rightRadius(width)
        }
      }
      val rightRoots =
        if (leftBound < rightBound) {
          derivativeSet.rootsWithin(Interval(leftBound, rightBound), rightRoot.toList)
        } else {
          rightRoot.toList
        }
      leftRoot match {
        case None =>
          Roots(rightRoots)
        case Some(root) =>
          Roots(root :: rightRoots)
      }
    }
  }
}

object Curve1d {
  private case class DerivativeSet(
    expression: ScalarExpression[CurveParameter],
    domain: Interval,
    tolerance: Double,
    maxRootOrder: Int
  ) {
    // To find 0th order roots, we need up to 1st order derivatives
    val maxDerivativeOrder = maxRootOrder + 1

    // Compile all necessary derivatives
    val derivatives: Array[ScalarExpression.CompiledCurve] = {
      val derivativeExpressions =
        Array.iterate(expression, maxDerivativeOrder + 1)(_.derivative(CurveParameter))
      derivativeExpressions.map(ScalarExpression.compileCurve(_))
    }

    // Calculate tolerances for each derivative order: tolerance(n) = n! * tolerance / width^n
    val tolerances: Array[Double] = {
      val width = domain.width
      val result = Array.ofDim[Double](maxDerivativeOrder + 1)
      result(0) = tolerance
      for (n <- 1 to maxDerivativeOrder) {
        result(n) = n * result(n - 1) / width
      }
      result
    }

    @tailrec
    final def rootAt(xValue: Double, minRootOrder: Int = 0): Option[Root] =
      if (minRootOrder >= maxDerivativeOrder) {
        None
      } else {
        val derivativeOrder = minRootOrder + 1
        val derivativeValue = derivatives(derivativeOrder).evaluate(xValue)
        if (derivativeValue.isZero(tolerances(derivativeOrder))) {
          // No root of the given minimum order - try the next higher order
          rootAt(xValue, derivativeOrder)
        } else {
          Some(Root(xValue, minRootOrder, derivativeValue.signum))
        }
      }

    def isZeroAt(xValue: Double, derivativeOrder: Int = 0): Boolean =
      derivatives(derivativeOrder).evaluate(xValue).isZero(tolerances(derivativeOrder))

    def rootsWithin(xInterval: Interval, currentRoots: List[Root]): List[Root] = {
      val mergedRegions = mergeRegions(monotonicRegionsWithin(xInterval, Nil))
      ???
    }

    private[this] def rootWithin(region: MonotonicRegion): Option[Root] = {
      ???
    }

    private[this] def monotonicRegionsWithin(
      xInterval: Interval,
      currentRegions: List[MonotonicRegion]
    ): List[MonotonicRegion] = {
      val derivativeBounds = evaluateAllWithin(xInterval)
      if (derivativeBounds(0).isZero(tolerance)) {
        MonotonicRegion(xInterval, -1) :: currentRegions
      } else {
        val nonZeroDerivativeOrder =
          (0 to maxDerivativeOrder).find(
            order => !derivativeBounds(order).expandedBy(tolerances(order)).contains(0.0)
          )
        nonZeroDerivativeOrder match {
          case None => {
            val (leftInterval, rightInterval) = xInterval.bisected
            val rightRegions = monotonicRegionsWithin(rightInterval, currentRegions)
            monotonicRegionsWithin(leftInterval, rightRegions)
          }
          case Some(order) =>
            MonotonicRegion(xInterval, order) :: currentRegions
        }
      }
    }

    private[this] def mergeRegions(regions: List[MonotonicRegion]): List[MonotonicRegion] =
      regions match {
        case Nil =>
          Nil
        case head :: tail => {
          val buffer = ListBuffer[MonotonicRegion]()
          @tailrec
          def add(current: MonotonicRegion, rest: List[MonotonicRegion]): Unit = rest match {
            case Nil =>
              buffer += current
            case next :: remaining =>
              if (current.nonZeroDerivativeOrder == next.nonZeroDerivativeOrder) {
                val mergedInterval =
                  Interval(current.xInterval.lowerBound, next.xInterval.upperBound)
                add(MonotonicRegion(mergedInterval, current.nonZeroDerivativeOrder), remaining)
              } else {
                buffer += current
                add(next, remaining)
              }
          }
          add(head, tail)
          buffer.toList
        }
      }

    private[this] def evaluateAllWithin(xInterval: Interval): Array[Interval] = {
      val result = Array.ofDim[Interval](derivatives.size)
      result(maxDerivativeOrder) = derivatives(maxDerivativeOrder).evaluateBounds(xInterval)
      for (derivativeOrder <- (maxDerivativeOrder - 1) to 0 by -1) {
        val derivative = derivatives(derivativeOrder)
        val nextDerivativeBounds = result(derivativeOrder + 1)
        result(derivativeOrder) =
          if (nextDerivativeBounds.contains(0.0) || nextDerivativeBounds.width.isInfinity) {
            derivative.evaluateBounds(xInterval)
          } else {
            // Next derivative is always finite and never zero in this interval, i.e. this
            // derivative is monotonic - can evaluate bounds by looking at endpoints only
            val leftValue = derivative.evaluate(xInterval.lowerBound)
            val rightValue = derivative.evaluate(xInterval.upperBound)
            leftValue.hull(rightValue)
          }
      }
      result
    }
  }

  private case class MonotonicRegion(xInterval: Interval, nonZeroDerivativeOrder: Int)

  sealed trait Zeros

  object EntireCurve extends Zeros

  case class Roots(roots: List[Root]) extends Zeros

  case class Root(x: Double, order: Int, sign: Int)
}
