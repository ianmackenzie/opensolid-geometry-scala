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
          val xInterval = Interval(leftBound, rightBound)
          derivativeSet.rootsWithin(xInterval, xInterval.ulp, maxRootOrder + 2, rightRoot.toList)
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

    def rootsWithin(
      xInterval: Interval,
      resolution: Double,
      knownNonZeroOrder: Int,
      tail: List[Root]
    ): List[Root] = {
      val function = derivatives(0)
      val functionBounds = function.evaluateBounds(xInterval)
      if (functionBounds.isNotZero(tolerance)) {
        tail
      } else {
        if (xInterval.width > resolution) {
          def bisect(newNonZeroOrder: Int) = {
            val (leftInterval, rightInterval) = xInterval.bisected
            val updatedTail = rootsWithin(rightInterval, resolution, newNonZeroOrder, tail)
            rootsWithin(leftInterval, resolution, newNonZeroOrder, updatedTail)
          }

          @tailrec
          def bisect(minOrder: Int, minOrderBounds: Interval): List[Root] =
            if (minOrder > maxRootOrder) {
              tail
            } else {
              val derivativeOrder = minOrder + 1
              if (derivativeOrder == knownNonZeroOrder) {
                if (minOrderBounds.contains(0.0)) {
                  bisect(knownNonZeroOrder)
                } else {
                  tail
                }
              } else {
                val derivativeBounds = derivatives(derivativeOrder).evaluateBounds(xInterval)
                val derivativeTolerance = tolerances(derivativeOrder)
                if (minOrderBounds.contains(0.0)) {
                  if (derivativeBounds.isNotZero(derivativeTolerance)) {
                    bisect(derivativeOrder)
                  } else if (!derivativeBounds.isZero(derivativeTolerance)) {
                    bisect(knownNonZeroOrder)
                  } else {
                    bisect(derivativeOrder, derivativeBounds)
                  }
                } else {
                  if (derivativeBounds.isNotZero(derivativeTolerance)) {
                    tail
                  } else {
                    bisect(derivativeOrder, derivativeBounds)
                  }
                }
              }
            }
          bisect(0, functionBounds)
        } else {
          @tailrec
          def prependRoot(minOrder: Int, minOrderBounds: Interval): List[Root] = {
            if (minOrder > maxRootOrder) {
              tail
            } else {
              val function = derivatives(minOrder)
              val derivativeOrder = minOrder + 1
              val derivative = derivatives(derivativeOrder)
              val derivativeBounds = derivative.evaluateBounds(xInterval)
              if (minOrderBounds.contains(0.0)) {
                if (derivativeBounds.isNotZero(tolerances(derivativeOrder))) {
                  val x1 = xInterval.lowerBound
                  val x2 = xInterval.upperBound
                  val y1 = function.evaluate(x1)
                  val y2 = function.evaluate(x2)
                  if (y2 < 0.0) {
                    if (y1 > 0.0) {
                      Root(xInterval.midpoint, minOrder, -1) :: tail
                    } else if (y1 < 0.0) {
                      tail
                    } else if (y1 == 0.0) {
                      Root(x1, minOrder, -1) :: tail
                    } else {
                      Root(x2, minOrder, -1) :: tail
                    }
                  } else if (y2 > 0.0) {
                    if (y1 < 0.0) {
                      Root(xInterval.midpoint, minOrder, 1) :: tail
                    } else if (y1 > 0.0) {
                      tail
                    } else if (y1 == 0.0) {
                      Root(x1, minOrder, 1) :: tail
                    } else {
                      Root(x2, minOrder, 1) :: tail
                    }
                  } else if (y2 == 0.0) {
                    tail
                  } else {
                    if (y1 < 0.0) {
                      Root(x1, minOrder, 1) :: tail
                    } else if (y1 > 0.0) {
                      Root(x1, minOrder, -1) :: tail
                    } else if (y1 == 0.0) {
                      Root(x1, minOrder, derivativeBounds.lowerBound.signum) :: tail
                    } else {
                      tail
                    }
                  }
                } else {
                  prependRoot(derivativeOrder, derivativeBounds)
                }
              } else {
                prependRoot(derivativeOrder, derivativeBounds)
              }
            }
          }
          prependRoot(0, functionBounds)
        }
      }
    }
  }

  sealed trait Zeros

  object EntireCurve extends Zeros

  case class Roots(roots: List[Root]) extends Zeros

  case class Root(x: Double, order: Int, sign: Int)
}
