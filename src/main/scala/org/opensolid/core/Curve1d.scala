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

  def zeros(tolerance: Double, maxOrder: Int = 2): Zeros = {
    val parameterizedCurve = this.parameterized
    val expression = parameterizedCurve.expression
    val domain = parameterizedCurve.domain
    val width = domain.width
    val derivatives =
      for {
        derivative <- Array.iterate(expression, maxOrder + 2)(_.derivative(CurveParameter))
      } yield {
        ScalarExpression.compileCurve(derivative)
      }

    // Calculate tolerances for each derivative order: tolerance(n) = n! * tolerance / width^n
    val tolerances = Array.ofDim[Double](maxOrder + 2)
    tolerances(0) = tolerance
    for (n <- 1 to maxOrder + 1) {
      tolerances(n) = n * tolerances(n - 1) / width
    }

    // Check left end
    val leftXValue = domain.lowerBound
    val leftIsZero = derivatives(0).evaluate(leftXValue).isZero(tolerance)
    val leftRoot =
      if (leftIsZero) rootAt(leftXValue, derivatives, tolerances, 0, width) else None
    if (leftIsZero && leftRoot.isEmpty) {
      // Curve is identically zero
      EntireCurve
    } else {
      // Check right end
      val rightXValue = domain.upperBound
      val rightIsZero = derivatives(0).evaluate(rightXValue).isZero(tolerance)
      val rightRoot =
        if (rightIsZero) rootAt(rightXValue, derivatives, tolerances, 0, width) else None
      if (rightIsZero && rightRoot.isEmpty) {
        // Curve is identically zero
        EntireCurve
      } else {
        // Search for roots
        val (leftRoots, leftBound) = leftRoot match {
          case None =>
            (List.empty[Root], leftXValue)
          case Some(RootWithRadius(root, radius)) =>
            (List(root), leftXValue + radius)
        }
        val (rightRoots, rightBound) = rightRoot match {
          case None =>
            (List.empty[Root], rightXValue)
          case Some(RootWithRadius(root, radius)) =>
            (List(root), rightXValue - radius)
        }
        val midRoots =
          if (leftBound < rightBound) {
            val fullInterval = Interval(leftBound, rightBound)
            allRootsWithin(fullInterval, derivatives, tolerances, maxOrder, width).map(_.root)
          } else {
            List.empty[Root]
          }
        Roots(leftRoots ++ midRoots ++ rightRoots)
      }
    }
  }
}

object Curve1d {
  sealed trait Zeros

  object EntireCurve extends Zeros

  case class Roots(roots: List[Root]) extends Zeros

  case class Root(x: Double, order: Int, sign: Int)

  private[Curve1d] case class RootWithRadius(root: Root, radiusOfInfluence: Double)

  @tailrec
  private[Curve1d] def rootAt(
    xValue: Double,
    derivatives: Array[ScalarExpression.CompiledCurve],
    tolerances: Array[Double],
    minOrder: Int,
    maxRadius: Double
  ): Option[RootWithRadius] = {
    val derivativeOrder = minOrder + 1
    if (derivativeOrder >= derivatives.size) {
      None
    } else {
      val derivative = derivatives(derivativeOrder)
      val derivativeValue = derivative.evaluate(xValue)
      if (derivativeValue.isNotZero(tolerances(derivativeOrder))) {
        val radius = nonZeroRadiusAbout(xValue, derivative, maxRadius)
        Some(RootWithRadius(Root(xValue, minOrder, derivativeValue.signum), radius))
      } else {
        rootAt(xValue, derivatives, tolerances, derivativeOrder, maxRadius)
      }
    }
  }

  private[this] def nonZeroRadiusAbout(
    xValue: Double,
    derivative: ScalarExpression.CompiledCurve,
    radius: Double
  ): Double =
    if (derivative.evaluateBounds(Interval(xValue - radius, xValue + radius)).contains(0.0)) {
      if (radius == 0.0) radius else nonZeroRadiusAbout(xValue, derivative, radius / 2.0)
    } else {
      radius
    }

  private[Curve1d] def allRootsWithin(
    xInterval: Interval,
    derivatives: Array[ScalarExpression.CompiledCurve],
    tolerances: Array[Double],
    order: Int,
    maxRadius: Double
  ): List[RootWithRadius] = {
    val roots = rootsWithin(xInterval, derivatives, tolerances, order, maxRadius)
    order match {
      case 0 =>
        roots
      case _ => {
        val (tail, upperBound) =
          roots.foldRight((List.empty[RootWithRadius], xInterval.upperBound))(
            (left, accumulator) => accumulator match {
              case (results, upperBound) => {
                val interval = Interval(left.root.x + left.radiusOfInfluence, upperBound)
                val lowerOrder =
                  allRootsWithin(interval, derivatives, tolerances, order - 1, maxRadius)
                (left +: lowerOrder ::: results, left.root.x - left.radiusOfInfluence)
              }
            }
          )
        val headInterval = Interval(xInterval.lowerBound, upperBound)
        val head = allRootsWithin(headInterval, derivatives, tolerances, order - 1, maxRadius)
        head ++ tail
      }
    }
  }

  private[Curve1d] def rootsWithin(
    xInterval: Interval,
    derivatives: Array[ScalarExpression.CompiledCurve],
    tolerances: Array[Double],
    order: Int,
    maxRadius: Double
  ): List[RootWithRadius] = {
    val derivativeBounds = evaluateWithin(xInterval, derivatives)
    val lowerOrderNonZero =
      (0 to order - 1).exists(
        index => !derivativeBounds(index).expandedBy(tolerances(index)).contains(0.0)
      )
    if (lowerOrderNonZero || !derivativeBounds(order).contains(0.0)) {
      // No roots of the given order exist since a derivative of that order or lower is non-zero
      List.empty[RootWithRadius]
    } else {
      // Attempt to bisect
      val xMin = xInterval.lowerBound
      val xMid = xInterval.midpoint
      val xMax = xInterval.upperBound
      if (xMin < xMid && xMid < xMax) {
        // X interval is large enough to bisect - recurse into each half
        val leftInterval = Interval(xMin, xMid)
        val rightInterval = Interval(xMid, xMax)
        val leftRoots = rootsWithin(leftInterval, derivatives, tolerances, order, maxRadius)
        val rightRoots = rootsWithin(rightInterval, derivatives, tolerances, order, maxRadius)
        mergeRoots(leftRoots, rightRoots)
      } else {
        // Cannot bisect any further - check if a root has been found
        val nextDerivative = derivatives(order + 1)
        val nextDerivativeValue = nextDerivative.evaluate(xMid)
        val radius = nonZeroRadiusAbout(xMid, nextDerivative, maxRadius)
        List(RootWithRadius(Root(xMid, order, nextDerivativeValue.signum), radius))
      }
    }
  }

  private[this] def evaluateWithin(
    domain: Interval,
    derivatives: Array[ScalarExpression.CompiledCurve]
  ): Array[Interval] = {
    val result = Array.ofDim[Interval](derivatives.size)
    val maxOrder = derivatives.size - 1
    result(maxOrder) = derivatives(maxOrder).evaluateBounds(domain)
    for (order <- maxOrder - 1 to 0 by -1) {
      val derivative = derivatives(order)
      val nextDerivativeBounds = result(order + 1)
      result(order) =
        if (nextDerivativeBounds.contains(0.0) || nextDerivativeBounds.width.isInfinity) {
          derivative.evaluateBounds(domain)
        } else {
          // Next derivative is always finite and never zero in this interval, i.e. this derivative
          // is monotonic - can evaluate bounds by looking at endpoints only
          val leftValue = derivative.evaluate(domain.lowerBound)
          val rightValue = derivative.evaluate(domain.upperBound)
          if (leftValue.isNaN || rightValue.isNaN) {
            // Fall back to bounds-based evaluation - function is undefined on part of the domain
            derivative.evaluateBounds(domain)
          } else {
            leftValue.hull(rightValue)
          }
        }
    }
    result
  }

  private[this] def mergeRoots(
    leftRoots: List[RootWithRadius],
    rightRoots: List[RootWithRadius]
  ): List[RootWithRadius] = leftRoots match {
    case Nil =>
      rightRoots
    case _ => {
      val leftReversed = leftRoots.reverse
      val leftLast :: leftInitReversed = leftReversed
      rightRoots match {
        case Nil =>
          leftRoots
        case rightHead :: rightTail => {
          val leftUpper = leftLast.root.x + leftLast.radiusOfInfluence
          val rightLower = rightHead.root.x - rightHead.radiusOfInfluence
          if (leftUpper >= rightLower) {
            // Last left root is equal to right head root - discard last left root
            leftInitReversed reverse_::: rightRoots
          } else {
            // No duplication - keep all
            leftReversed reverse_::: rightRoots
          }
        }
      }
    }
  }
}
